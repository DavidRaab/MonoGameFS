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
    Point:  Vector2
}

// Initialize the Game Model
let initModel assets =
    let box = Entity.init (fun e ->
        e.addPosition (Position.createXY 0f 0f)
        e.addView     (View.fromTexture assets.Texture.WhiteBox FG1)
    )

    let arrow = Entity.init (fun e ->
        e.addPosition (Position.createXY 100f 100f)
        e.addView     (
            View.fromTexture assets.Texture.Arrow FG1
            |> View.withOrigin Center
        )
        Systems.Timer.addTimer (Timer.every (sec 0.5) () (fun _ dt ->
            e |> State.View.iter (fun view ->
                View.setRotation (view.Rotation + Radian.fromDeg 45.0<deg>) view |> ignore
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
        Point  = Vector2(200f,0f)
    }
    gameState

type KnightState =
    | IsAttack of elapsed:TimeSpan * duration:TimeSpan
    | IsLeft   of Vector2
    | IsRight  of Vector2
    | IsCrouch
    | IsIdle

let statePriority state =
    match state with
    | IsAttack _ -> 4
    | IsLeft   _ -> 3
    | IsRight  _ -> 3
    | IsCrouch   -> 2
    | IsIdle     -> 1

type Action =
    | Attack
    | MoveLeft  of Vector2
    | MoveRight of Vector2
    | Crouch
    | Movement  of Vector2
    | Camera    of Vector2
    | CameraHome
    | ZoomIn
    | ZoomOut

let mutable knightState = IsIdle

// A Fixed Update implementation that tuns at the specified fixedUpdateTiming
let fixedUpdateTiming = sec (1.0 / 60.0)
let fixedUpdate model (deltaTime:TimeSpan) =
    let fDeltaTime = float32 deltaTime.TotalSeconds
    Systems.Movement.update        deltaTime
    Systems.Timer.update           deltaTime
    Systems.SheetAnimations.update deltaTime

    // Get all Input of user and maps them into actions
    let actions = Input.mapInput {
        Keyboard = [
            IsPressed (Key.Space, Attack)
            IsKeyDown (Key.Left,  MoveLeft -Vector2.UnitX)
            IsKeyDown (Key.Right, MoveRight Vector2.UnitX)
            IsKeyDown (Key.Down,  Crouch)
            IsKeyDown (Key.W,     Camera -Vector2.UnitY)
            IsKeyDown (Key.A,     Camera -Vector2.UnitX)
            IsKeyDown (Key.S,     Camera  Vector2.UnitY)
            IsKeyDown (Key.D,     Camera  Vector2.UnitX)
            IsKeyDown (Key.Home,  CameraHome)
            IsKeyDown (Key.R,     ZoomIn)
            IsKeyDown (Key.F,     ZoomOut)
        ]
        GamePad = [
            IsKeyDown (Button.X,         Attack)
            IsKeyDown (Button.DPadLeft,  MoveLeft  -Vector2.UnitX)
            IsKeyDown (Button.DPadRight, MoveRight  Vector2.UnitX)
            IsKeyDown (Button.DPadDown,  Crouch)
        ]
        ThumbStick = {
            Left  = Movement
            Right = Camera
        }
    }

    // A state machine, but will be replaced later by some library
    let nextKnightState previousState =
        // helper-function that describes how an action is mapped to a knightState
        let action2state action =
            match action with
            | Attack      -> IsAttack (TimeSpan.Zero, SheetAnimation.fullDuration (model.Knight.getAnimationExn "Attack"))
            | MoveLeft  v -> IsLeft v
            | MoveRight v -> IsRight v
            | Crouch      -> IsCrouch
            | Movement v  ->
                if   v.X > 0f then IsRight <| Vector2(v.X,0f)
                elif v.X < 0f then IsLeft  <| Vector2(v.X,0f)
                else IsIdle
            | _           -> IsIdle

        // 2. Find the next state by mapping every action to a state, and get the one with the highest priority.
        //    For example, when user hits Attack button, it has higher priority as moving
        let wantedState =
            match List.map action2state actions with
            | [] -> IsIdle
            | xs -> List.maxBy statePriority xs

        // Function that describes the transition to a new state. Mostly it means setting the
        // correct animation and moving the character
        let setState state =
            match state with
            | IsAttack (e,d) ->
                model.Knight.setAnimation "Attack"; IsAttack (e,d)
            | IsCrouch ->
                model.Knight.setAnimation "Crouch"; IsCrouch
            | IsLeft v       ->
                model.Knight.setAnimation "Run";
                model.Knight |> State.View.iter     (View.flipHorizontal true)
                model.Knight |> State.Position.iter (Position.add (v * 300f * fDeltaTime))
                IsLeft v
            | IsRight v     ->
                model.Knight.setAnimation "Run";
                model.Knight |> State.View.iter     (View.flipHorizontal false)
                model.Knight |> State.Position.iter (Position.add (v * 300f * fDeltaTime))
                IsRight v
            | IsIdle ->
                model.Knight.setAnimation "Idle";
                IsIdle

        // 3. Real state machine. Checks the current state, and the new state, and does
        //    a transition to the new state if allowed.
        match previousState, wantedState with
        | IsAttack (e,d), wantedState ->
            let elapsed = e + deltaTime
            if elapsed >= d
            then setState wantedState
            else IsAttack (elapsed,d)
        | _ , wanted  -> setState wanted

    // Compute new Knight State
    knightState <- nextKnightState knightState

    // Update Camera
    for action in actions do
        match action with
        | CameraHome -> Camera.setPosition (Vector2.create 0f 0f) State.camera |> ignore
        | ZoomIn     -> Camera.addZoom (1.0 * deltaTime.TotalSeconds) State.camera
        | ZoomOut    -> Camera.subtractZoom (1.0 * deltaTime.TotalSeconds) State.camera
        | Camera v   -> State.camera |> Camera.add (v * 400f * ((float32 State.camera.MaxZoom + 1f) - float32 State.camera.Zoom) * fDeltaTime)
        | _          -> ()

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

    let mouse = Input.Mouse.GetState()
    let point =
        if mouse.LeftButton = Input.ButtonState.Pressed
        then Camera.screenToWorld (mouse.Position.ToVector2()) State.camera
        else model.Point

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

    { model with
        Point = point
    }

let draw (model:Model) (gameTime:GameTime) (game:MyGame) =
    game.GraphicsDevice.Clear(Color.CornflowerBlue)

    let doSpriteBatch (sb:SpriteBatch) (camera:Camera) f =
        sb.Begin(transformMatrix = Camera.matrix camera)
        f sb
        sb.End()
    let onCamera = doSpriteBatch game.SpriteBatch

    let drawRect =
        Systems.Debug.rectangle game.Asset.Texture.Pixel 2 Color.MidnightBlue

    onCamera State.camera (fun sb ->
        Systems.View.draw sb
        drawRect (Vector2.create 100f 100f) model.Point sb
    )

    onCamera State.cameraScreen (fun sb ->
        FPS.draw game.Asset.Font.Default sb
        Systems.Debug.mousePosition sb game.Asset.Font.Default
        Systems.Debug.trackPosition sb game.Asset.Font.Default model.Knight (Vector2.create 400f 460f)
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
    Input.Mouse.SetCursor(Input.MouseCursor.Crosshair)
    State.camera       <- Camera.create width height |> Camera.withMinMaxZoom 0.03 3
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
