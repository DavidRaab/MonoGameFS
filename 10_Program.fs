module MyGame.App
open MyGame.DataTypes
open MyGame.Components
open MyGame.State
open MyGame.Entity
open MyGame.Utility
open MyGame.Timer
open MyGame.Assets
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

// Only load the Keys -- I have my own Input Implementation on top of MonoGame
type Key    = Input.Keys
type Button = Input.Buttons

// Model
type Model = {
    Knight: Entity
}

// Initialize the Game Model
let initModel assets =
    let arrow = Entity.init (fun e ->
        e.addPosition (Position.createXY 100f 100f)
        e.addView     (
            View.fromTexture assets.Texture.Arrow FG1
            |> View.withOrigin Center
        )
        Systems.Timer.addTimer (Timer.every (sec 0.5) () (fun _ dt ->
            e |> State.View.iter (fun view ->
                View.setRotation (view.Rotation + Radiant.fromDeg 45.0<deg>) view |> ignore
            )
            State ()
        ))
    )

    let knight = Entity.init (fun e ->
        e.addPosition (Position.createXY 427f 0f)
        e.addView     (
            SheetAnimations.toView FG1 assets.Knight
            |> View.setScale (Vector2.create 3f 3f)
            |> View.withOrigin Top
        )
        e.addSheetAnimations (assets.Knight)
    )

    let boxes = ResizeArray<_>()
    let yOffset = 50f
    for x=1 to 75 do
        for y=1 to 40 do
            boxes.Add (Entity.init (fun box ->
                box.addPosition (Position.createXY (float32 x * 11f) (float32 y * 11f + yOffset))
                box.addView     (View.fromTexture assets.Texture.WhiteBox BG1)
            ))

    Systems.Timer.addTimer (Timer.every (sec 1.0) false (fun state dt ->
        let vec = if state then Vector2.right 10f else Vector2.left 10f
        for box in boxes do
            State.Position.map (fun pos -> {pos with Position = pos.Position + vec}) box
        State (not state)
    ))

    Systems.Timer.addTimer (Timer.every (sec 1.0) () (fun _ _ ->
        System.GC.Collect ()
        State ()
    ))

    let gameState = {
        Knight = knight
    }
    gameState

type KnightState =
    | IsAttack of elapsed:TimeSpan * duration:TimeSpan
    | IsLeft
    | IsRight
    | IsCrouch
    | IsIdle

let statePriority state =
    match state with
    | IsAttack _ -> 4
    | IsLeft     -> 3
    | IsRight    -> 3
    | IsCrouch   -> 2
    | IsIdle     -> 1

type Action =
    | Attack
    | MoveLeft
    | MoveRight
    | Crouch

let mutable knightState = IsIdle

// A Fixed Update implementation that tuns at the specified fixedUpdateTiming
let fixedUpdateTiming = sec (1.0 / 60.0)
let fixedUpdate model (deltaTime:TimeSpan) =
    let fDeltaTime = float32 deltaTime.TotalSeconds
    Systems.Movement.update        deltaTime
    Systems.Timer.update           deltaTime
    Systems.SheetAnimations.update deltaTime

    let nextKnightState previousState =
        let actions = Input.mapInput {
            Keyboard = [
                IsPressed (Key.Space, Attack)
                IsKeyDown (Key.Left,  MoveLeft)
                IsKeyDown (Key.Right, MoveRight)
                IsKeyDown (Key.Down,  Crouch)
            ]
            GamePad = [
                IsKeyDown (Button.X,         Attack)
                IsKeyDown (Button.DPadLeft,  MoveLeft)
                IsKeyDown (Button.DPadRight, MoveRight)
                IsKeyDown (Button.DPadDown,  Crouch)
            ]
        }

        let action2state action state =
            if List.contains action actions then state else IsIdle

        let isAttack    = action2state Attack   (IsAttack (TimeSpan.Zero, SheetAnimation.fullDuration (model.Knight.getAnimationExn "Attack")))
        let isLeft      = action2state MoveLeft  IsLeft
        let isRight     = action2state MoveRight IsRight
        let isCrouch    = action2state Crouch    IsCrouch
        let wantedState = List.maxBy statePriority [isAttack;isLeft;isRight;isCrouch]

        let setAnim name =
            model.Knight |> State.SheetAnimations.iter (fun anims ->
                SheetAnimations.setAnimation name anims
            )

        let setState state =
            match state with
            | IsAttack (e,d) ->
                setAnim "Attack"; IsAttack (e,d)
            | IsCrouch ->
                setAnim "Crouch"; IsCrouch
            | IsLeft         ->
                setAnim "Run";
                model.Knight |> State.View.iter     (View.flipHorizontal true)
                model.Knight |> State.Position.iter (Position.addX (-200f * fDeltaTime))
                IsLeft
            | IsRight        ->
                setAnim "Run";
                model.Knight |> State.View.iter     (View.flipHorizontal false)
                model.Knight |> State.Position.iter (Position.addX (200f * fDeltaTime))
                IsRight
            | IsIdle ->
                setAnim "Idle";
                IsIdle

        match previousState, wantedState with
        | IsAttack (e,d), wantedState ->
            let elapsed = e + deltaTime
            if elapsed >= d
            then setState wantedState
            else IsAttack (elapsed,d)
        | _ , wanted  -> setState wanted

    knightState <- nextKnightState knightState


    // Update Camera Position
    let updateCamera key (vec:Vector2) =
        if Keyboard.isKeyDown key then
            Camera.add (vec * (3.0f - float32 State.camera.Zoom) * fDeltaTime) State.camera

    let SpeedPx = 400f
    if Keyboard.isKeyDown Key.Home then
        Camera.setPosition (Vector2.create 0f 0f) State.camera |> ignore

    updateCamera Key.W (Vector2(0f,-SpeedPx))
    updateCamera Key.A (Vector2(-SpeedPx,0f))
    updateCamera Key.S (Vector2(0f,SpeedPx))
    updateCamera Key.D (Vector2(SpeedPx,0f))


    // Update Camera Zoom
    if Keyboard.isKeyDown Key.R then
        Camera.addZoom (1.0 * deltaTime.TotalSeconds) State.camera
    if Keyboard.isKeyDown Key.F then
        Camera.subtractZoom (1.0 * deltaTime.TotalSeconds) State.camera

    // Resets the Keyboard State
    Keyboard.nextState ()
    model

// Type Alias for my game
type MyGame = MonoGame<Assets,Model>

let mutable fixedUpdateElapsedTime = TimeSpan.Zero
let update (model:Model) (gameTime:GameTime) (game:MyGame) =
    // Get current keyboard state and add it to our KeyBoard module
    // This way we ensure that fixedUpdate has correct keyboard state between
    // fixedUpdate calls and not just from the current update.
    let keyboard = Input.Keyboard.GetState ()
    Keyboard.addKeys (keyboard.GetPressedKeys())

    // Close Game
    if keyboard.IsKeyDown Key.Escape then
        game.Exit ()

    let deltaTime = gameTime.ElapsedGameTime
    FPS.update deltaTime

    // FixedUpdate Handling
    fixedUpdateElapsedTime <- fixedUpdateElapsedTime + deltaTime
    let model =
        if fixedUpdateElapsedTime >= fixedUpdateTiming then
            fixedUpdateElapsedTime <- fixedUpdateElapsedTime - fixedUpdateTiming
            fixedUpdate model fixedUpdateTiming
        else
            model

    (*
    // Vibration through Triggers
    // printfn "%f %f" gamePad.Triggers.Left gamePad.Triggers.Right
    GamePad.SetVibration(0,
        gamePad.Triggers.Left,
        gamePad.Triggers.Right
    ) |> ignore

    if keyboard.IsKeyDown Keys.Space then
        ignore <| GamePad.SetVibration(0, 1.0f, 1.0f)

    if GamePad.isPressed gamePad.Buttons.A then
        printfn "Pressed A"

    if GamePad.isPressed gamePad.Buttons.Back || keyboard.IsKeyDown Keys.Escape then
        game.Exit()
    *)

    model

let draw (model:Model) (gameTime:GameTime) (game:MyGame) =
    game.GraphicsDevice.Clear(Color.CornflowerBlue)

    let doSpriteBatch (sb:SpriteBatch) (camera:Camera) f =
        sb.Begin(transformMatrix = Camera.matrix camera)
        f sb
        sb.End()
    let onCamera = doSpriteBatch game.spriteBatch

    onCamera State.cameraScreen (fun sb ->
        FPS.draw game.Asset.Font.Default sb
    )

    onCamera State.camera (fun sb ->
        Systems.View.draw sb
    )

// Initialization of the Game
let init (game:MyGame) =
    let width, height = 854, 480
    game.Graphics.SynchronizeWithVerticalRetrace <- false
    game.IsFixedTimeStep       <- false
    game.TargetElapsedTime     <- sec (1.0 / 60.0)
    game.Content.RootDirectory <- "Content"
    game.IsMouseVisible        <- true
    game.SetResolution width height
    State.camera       <- Camera.create width height
    State.cameraScreen <- Camera.create width height

// Loading Assets
let loadAssets (game:MyGame) =
    let load     str = game.Content.Load<Texture2D>(str)
    let loadFont str = game.Content.Load<SpriteFont>(str)
    let gd           = game.GraphicsDevice
    let texture      = Texture2D.create gd
    Assets.load load loadFont texture

// Run MonoGame Application
[<EntryPoint;System.STAThread>]
let main argv =
    using (new MonoGame<Assets,Model>(init, loadAssets, initModel, update, draw)) (fun game ->
        game.Run()
    )
    1
