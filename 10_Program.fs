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
type Button = FGamePadButton

// Model
type MouseRectangle =
    | NoRectangle
    | StartRectangle of Vector2
    | DrawRectangle  of Vector2 * Point
    | EndRectangle   of Vector2 * Vector2

type Model = {
    Knight:         Entity
    MouseRectangle: MouseRectangle
}

// Called in initModel - Sets up all the boxes
let boxes assets =
    // black box that rotates
    let boxesOrigin = Entity.init (fun e ->
        e.addView      (View.fromSpriteCenter BG1 assets.Sprites.WhiteBox |> View.setTint Color.Black)
        e.addTransform (Transform.fromPosition 0f 0f)
        e.addMovement {
            Direction = ValueNone // ValueSome (Relative (Vector2.Right * 50f))
            Rotation  = ValueSome 2f<rad>
        }
    )

    let boxes = ResizeArray<_>()
    //     0 boxes                -> 12500 fps
    //
    //  3000 boxes without parent -> 2500 fps
    //  4000 boxes without parent -> 1950 fps
    //  5000 boxes without parent -> 1550 fps
    //  6000 boxes without parent -> 1300 fps
    // 10000 boxes without parent ->  600 fps
    //
    //  3000 boxes with parent    -> 1550 fps
    //  4000 boxes with parent    -> 1150 fps
    //  5000 boxes with parent    ->  925 fps
    //  6000 boxes with parent    ->  750 fps
    // 10000 boxes with parent    ->  350 fps
    //
    // Create 3600 Boxes as child of boxesOrigin (1320 fps)
    for x=1 to 60 do
        for y=1 to 60 do
            boxes.Add (Entity.init (fun box ->
                box.addTransform       (
                    Transform.fromPosition (float32 x * 11f) (float32 y * 11f)
                    // this cost a lot of performance because rotation/position/scale of all 3.000 boxes
                    // must be computed with a matrix calculated of the parent.
                    |> Transform.withParent (ValueSome boxesOrigin)
                )
                box.addView      (Sheets.createView BG1 Center assets.Box)
                box.addAnimation (Animation.create assets.Box)
                // box |> State.View.map (View.withOrigin Center)
                box.addMovement {
                    Direction = ValueNone //ValueSome (Relative (Vector2.Right * 25f))
                    Rotation  = ValueSome (2f<rad>)
                }
            ))

    // let all boxes move
    let rng = System.Random ()
    Systems.Timer.addTimer (Timer.every (sec 1.0) () (fun state dt ->
        // changes direction and rotation of every box every second to a
        // new random direction/rotation
        for box in boxes do
            // 10% of all boxes will move to world position 0,0 with 10px per second
            // all other boxes move in a random direction at 25px per second
            box |> State.Movement.add {
                Direction = ValueSome(
                    if   rng.NextSingle() < 0.1f
                    then Absolute (Vector2.Zero,10f)
                    else Relative (Vector2.randomDirection 25f)
                )
                Rotation = ValueSome(
                    Radian.fromDeg (rng.NextSingle() * 60f<deg> - 30f<deg>)
                )
            }
        State ()
    ))

    // only show every second box - 3000 out of 6000
    //   - with parent    1500 fps
    //   - without parent 2500 fps
    // let mutable switch = true
    // for box in boxes do
    //     if switch then
    //         State.View.switchVisibility box
    //     switch <- not switch

    //----------
    // randomly switch visibility. after some seconds roughly the half of boxes are visisble
    //
    // All without parent
    // rendering 3000 boxes all shown                -> 2500 fps
    // rendering 3000 boxes from 6000 (half visible) -> 2200 fps
    //
    // Calling switchVisibility has some costs as a view has to be added/removed to
    // different containers. But usually in a typical game this is not often
    // called. If visibility stays the same then showing half of the boxes
    // nearly has same performance as showing all boxes without anyone being
    // deactivated
    // let rng2 = System.Random ()
    // Systems.Timer.addTimer (Timer.every (sec 0.25) () (fun _ _ ->
    //     for i=1 to 250 do
    //         let ridx = rng2.Next(boxes.Count)
    //         State.View.switchVisibility boxes.[ridx]
    //     State ()
    // ))

    ()

// Initialize the Game Model
let initModel assets =
    boxes assets

    let arrow = Entity.init (fun e ->
        e.addTransform (
            Transform.fromPosition 100f 100f
            // |> Transform.setRotationVector (Vector2.Right)
        )
        e.addView (View.fromSpriteCenter FG1 assets.Sprites.Arrow)
        Systems.Timer.addTimer (Timer.every (sec 0.1) () (fun _ dt ->
            e |> State.Transform.fetch (fun tf ->
                Transform.addRotation 0.1f<rad> tf
            )
            State ()
        ))
    )

    let knight = Entity.init (fun e ->
        e.addTransform (Transform.fromPosition 320f 200f)
        e.addView (
            Sheets.createView FG1 Top assets.Knight
            |> View.setScale (Vector2.create 2f 2f)
        )
        e.addAnimation (Animation.create assets.Knight)
    )

    // Creates a box that is a parent of the knight and moves when Knight moves
    let box = Entity.init (fun e ->
        e.addTransform (
            Transform.fromPosition 0f 80f
            |> Transform.withParent (ValueSome knight)
        )
        e.addView (
            View.fromSpriteCenter FG1 assets.Sprites.WhiteBox
            |> View.setTint Color.Aqua
        )
    )

    let sun = Entity.init (fun e ->
        e.addTransform (Transform.fromPosition 200f 200f)
        e.addView (
            View.fromSpriteCenter FG1 assets.Sprites.WhiteBox
            |> View.setTint Color.Yellow
        )
        Systems.Timer.addTimer (Timer.every (sec 0.1) (Choice1Of2 0) (fun state dt ->
            match state with
            | Choice1Of2 right ->
                e |> State.Transform.fetch (Transform.addPosition (Vector2.Right * 5f))
                if right < 20
                then State (Choice1Of2 (right+1))
                else State (Choice2Of2 (right-1))
            | Choice2Of2 left ->
                e |> State.Transform.fetch (Transform.addPosition (Vector2.Left * 5f))
                if left > 0
                then State (Choice2Of2 (left-1))
                else State (Choice1Of2 (left+1))
        ))
    )

    let planet1 = Entity.init (fun e ->
        e.addTransform(
            Transform.fromPosition 0f -100f
            |> Transform.withParent (ValueSome sun)
        )
        e.addView (
            View.fromSpriteCenter FG1 assets.Sprites.WhiteBox
            |> View.setTint Color.BlueViolet
        )
    )

    let planet2 = Entity.init (fun e ->
        e.addTransform(
            Transform.fromPosition 0f -50f
            |> Transform.withParent (ValueSome planet1)
        )
        e.addView (
            View.fromSpriteCenter FG1 assets.Sprites.WhiteBox
            |> View.setTint Color.DarkViolet
        )
    )

    let planet3 = Entity.init (fun e ->
        e.addTransform(
            Transform.fromPosition 0f -20f
            |> Transform.withParent (ValueSome planet2)
        )
        e.addView (
            View.fromSpriteCenter FG1 assets.Sprites.WhiteBox
            |> View.setTint Color.Brown
        )
    )

    // Let stars rotate at 60 fps and 1° each frame
    let deg1 = Radian.fromDeg 1f<deg>
    Systems.Timer.addTimer (Timer.every (sec (1.0/60.0)) () (fun state dt ->
        List.iter (State.Transform.fetch (Transform.addRotation deg1)) [sun;planet1;planet2;planet3]
        State ()
    ))

    // Makes the box over the knight move from left/right like Knight Rider!
    Systems.Timer.addTimer (Timer.every (sec 0.1) (Choice1Of2 0) (fun state dt ->
        match state with
        | Choice1Of2 state ->
            box |> State.Transform.fetch (fun t ->
                Transform.addPosition (Vector2.create 10f 0f) t
            )
            if state < 4
            then State (Choice1Of2 (state+1))
            else State (Choice2Of2 (state+1))
        | Choice2Of2 state ->
            box |> State.Transform.fetch (fun t ->
                Transform.addPosition (Vector2.create -10f 0f) t
            )
            if state > -4
            then State (Choice2Of2 (state-1))
            else State (Choice1Of2 (state-1))
    ))

    // Periodically run Garbage Collector
    Systems.Timer.addTimer (Timer.every (sec 10.0) () (fun _ _ ->
        System.GC.Collect ()
        State ()
    ))

    let gameState = {
        Knight         = knight
        MouseRectangle = NoRectangle
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
    | ScrollZoom  of int
    | ZoomIn
    | ZoomOut
    | DragStart   of Vector2
    | DragBetween of Point
    | DragEnd     of Vector2

let mutable knightState = IsIdle

// Input mapping to User Actions
let inputMapping = {
    Keyboard = [
        Key.Space, IsPressed, Attack
        Key.Space, IsPressed, Attack
        Key.Left,  IsKeyDown, MoveLeft  Vector2.Left
        Key.Right, IsKeyDown, MoveRight Vector2.Right
        Key.Down,  IsKeyDown, Crouch
        Key.W,     IsKeyDown, Camera Vector2.Up
        Key.A,     IsKeyDown, Camera Vector2.Left
        Key.S,     IsKeyDown, Camera Vector2.Down
        Key.D,     IsKeyDown, Camera Vector2.Right
        Key.Home,  IsKeyDown, CameraHome
        Key.R,     IsKeyDown, ZoomIn
        Key.F,     IsKeyDown, ZoomOut
    ]
    GamePad = {
        Buttons = [
            Button.X,         IsPressed, Attack
            Button.DPadLeft,  IsKeyDown, MoveLeft  Vector2.Left
            Button.DPadRight, IsKeyDown, MoveRight Vector2.Right
            Button.DPadDown,  IsKeyDown, Crouch
        ]
        ThumbStick = {
            Left  = Some Movement
            Right = Some Camera
        }
        Trigger = {
            Left  = Some (fun m -> MoveLeft  (Vector2.Left  * m))
            Right = Some (fun m -> MoveRight (Vector2.Right * m))
        }
    }
    Mouse = {
        Buttons = [
            MouseButton.Left, IsPressed,  World (DragStart)
            MouseButton.Left, IsKeyDown,  Screen(DragBetween)
            MouseButton.Left, IsReleased, World (DragEnd)
        ]
        ScrollWheel           = Some (cmpInt (is ScrollZoom 1) (is ScrollZoom -1) (is ScrollZoom 0))
        HorizontalScrollWheel = None
        Position              = None
    }
}

// A Fixed Update implementation that tuns at the specified fixedUpdateTiming
let mutable resetInput = false
let fixedUpdateTiming = sec (1.0 / 60.0)
let fixedUpdate model (deltaTime:TimeSpan) =
    let fDeltaTime = float32 deltaTime.TotalSeconds
    Systems.Timer.update      deltaTime
    Systems.Movement.update   deltaTime
    Systems.Animations.update deltaTime

    // Get all Input of user and maps them into actions
    let actions = FInput.mapInput State.camera inputMapping

    // Handle Rectangle Drawing
    let model =
        let action = actions |> List.tryFind (function
            | DragStart _ | DragBetween _ | DragEnd _ -> true
            | _ -> false
        )
        match action with
        | Some (DragStart start) ->
            { model with MouseRectangle = StartRectangle start }
        | Some (DragBetween p) ->
            let mr =
                match model.MouseRectangle with
                | NoRectangle             -> StartRectangle (Camera.screenPointToWorld p State.camera)
                | StartRectangle start    -> DrawRectangle  (start,p)
                | DrawRectangle (start,_) -> DrawRectangle  (start,p)
                | EndRectangle  (_, _)    -> StartRectangle (Camera.screenPointToWorld p State.camera)
            { model with MouseRectangle = mr }
        | Some (DragEnd stop) ->
            let mr =
                match model.MouseRectangle with
                | NoRectangle             -> NoRectangle
                | StartRectangle start    -> NoRectangle
                | DrawRectangle (start,_) -> EndRectangle (start,stop)
                | EndRectangle  (_, _)    -> NoRectangle
            { model with MouseRectangle = mr }
        | Some _ -> model
        | None   -> model


    // A state machine, but will be replaced later by some library
    let nextKnightState previousState =
        // helper-function that describes how an action is mapped to a knightState
        let action2state = function
            | Attack      -> IsAttack (TimeSpan.Zero, Sheet.duration (model.Knight.getSheetExn "Attack"))
            | MoveLeft  v -> IsLeft v
            | MoveRight v -> IsRight v
            | Crouch      -> IsCrouch
            | Movement v  ->
                if   v.X > 0f then IsRight <| Vector2(v.X,0f)
                elif v.X < 0f then IsLeft  <| Vector2(v.X,0f)
                else IsIdle
            | _           -> IsIdle

        // helper-function that describes the transition to a new state. Mostly it means setting the
        // correct animation and moving the character
        let setState state =
            match state with
            | IsAttack (e,d) -> IsAttack (e,d)
            | IsCrouch       -> IsCrouch
            | IsLeft v       ->
                model.Knight |> State.View.iter      (View.flipHorizontal true)
                model.Knight |> State.Transform.fetch (Transform.addPosition (v * 300f * fDeltaTime))
                IsLeft v
            | IsRight v     ->
                model.Knight |> State.View.iter      (View.flipHorizontal false)
                model.Knight |> State.Transform.fetch (Transform.addPosition (v * 300f * fDeltaTime))
                IsRight v
            | IsIdle -> IsIdle

        let setAnimation state =
            let anim =
                match state with
                | IsAttack (_,_) -> "Attack"
                | IsCrouch       -> "Crouch"
                | IsLeft _       -> "Run"
                | IsRight _      -> "Run"
                | IsIdle         -> "Idle"
            model.Knight.setAnimation anim

        // 1. Find the next state by mapping every action to a state, and get the one with the highest priority.
        //    For example, when user hits Attack button, it has higher priority as moving
        let wantedState =
            match List.map action2state actions with
            | [] -> IsIdle
            | xs -> List.maxBy statePriority xs

        // 2. Real state machine. Checks the current state, and the new state, and does
        //    a transition to the new state if allowed.
        match previousState, wantedState with
        | IsAttack (e,d), wantedState ->
            let elapsed = e + deltaTime
            if elapsed >= d
            then
                setAnimation wantedState
                setState wantedState
            else IsAttack (elapsed,d)
        | previous, wanted  ->
            // When state changed we need to switch animation
            if previous <> wanted then
                setAnimation wanted
            setState wanted

    // Compute new Knight State
    knightState <- nextKnightState knightState

    // Update Camera
    for action in actions do
        match action with
        | CameraHome                 -> Camera.setPosition   (Vector2.create 0f 0f) State.camera |> ignore
        | ZoomIn                     -> Camera.addZoom       (1.0f * fDeltaTime) State.camera
        | ZoomOut                    -> Camera.subtractZoom  (1.0f * fDeltaTime) State.camera
        | ScrollZoom (IsGreater 0 x) -> Camera.addZoom        0.1f State.camera
        | ScrollZoom (IsSmaller 0 x) -> Camera.subtractZoom   0.1f State.camera
        | Camera v                   -> Camera.add           (v * 400f * ((float32 State.camera.MaxZoom + 1f) - float32 State.camera.Zoom) * fDeltaTime) State.camera
        | _                          -> ()

    // Whenever one fixedUpdate runs the Input states should be resetted
    // But the current input information should also be avaiable in draw
    // when needed. So we just set the flag and reset the input after
    // we have drawn everything
    resetInput <- true

    // The next model
    model

// Type Alias for my game
type MyGame = MonoGame<Assets,Model>

let mutable fixedUpdateElapsedTime = TimeSpan.Zero
let update (model:Model) (gameTime:GameTime) (game:MyGame) =
    // Get current keyboard/GamePad state and add it to our KeyBoard/GamePad module
    // This way we ensure that fixedUpdate has correct keyboard/GamePad state between
    // fixedUpdate calls and not just from the current update.
    let keyboard = Input.Keyboard.GetState ()
    FKeyboard.addKeys (keyboard.GetPressedKeys())
    let gamepad  = Input.GamePad.GetState(0)
    FGamePad.addState gamepad
    let mouse    = Input.Mouse.GetState ()
    FMouse.addState (mouse,State.camera)

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

// Helper for the spriteBatch Pattern. You always call Begin doSomething and then End.
// On top i always use a camera for position everything
let inline doSpriteBatch (sb:SpriteBatch) (camera:Camera) samplerState ([<InlineIfLambda>] f) =
    sb.Begin(transformMatrix = Camera.matrix camera, samplerState = samplerState)
    f sb
    sb.End()

let draw (model:Model) (gameTime:GameTime) (game:MyGame) =
    game.GraphicsDevice.Clear Color.CornflowerBlue

    // Helper to draw a Rectangle
    let drawRect =
        Systems.Drawing.rectangle game.Asset.Sprites.Pixel 2 Color.MidnightBlue

    // Draw Game Elements
    doSpriteBatch game.SpriteBatch State.camera SamplerState.PointWrap (fun sb ->
        Systems.View.draw sb

        match model.MouseRectangle with
        | NoRectangle         -> ()
        | StartRectangle p    -> ()
        | DrawRectangle (start,stop) ->
            let stop = Camera.screenPointToWorld stop State.camera
            drawRect start stop sb
        | EndRectangle (start,stop) ->
            drawRect start stop sb
    )

    // Draw Game UI
    doSpriteBatch game.SpriteBatch State.uiCamera SamplerState.LinearClamp (fun sb ->
        FPS.draw game.Asset.Font.Default sb
        Systems.Drawing.mousePosition sb game.Asset.Font.Default (FMouse.position ()) (Vector2.create 3f 340f)
        Systems.Drawing.trackPosition sb game.Asset.Font.Default model.Knight (Vector2.create 400f 460f)

        sb.DrawString(
            spriteFont = game.Asset.Font.Default,
            text       = String.Format("Visible: {0}", State.View.visible.Count),
            position   = Vector2(320f, 3f),
            color      = Color.Yellow,
            rotation   = 0f,
            origin     = Vector2.Zero,
            scale      = 1f,
            effects    = SpriteEffects.None,
            layerDepth = 0f
        )
    )

    if resetInput then
        resetInput <- false
        FKeyboard.nextState ()
        FGamePad.nextState  ()
        FMouse.nextState    ()

// Initialization of the Game
let init (game:MyGame) =
    // The virtual width/height is the internal resolution the game renders its image
    // The window width/height is the actual size of the window
    let virtualWidth, virtualHeight = game.RenderTarget.Width, game.RenderTarget.Height
    let windowWidth,  windowHeight  = 1200, 480

    game.Graphics.SynchronizeWithVerticalRetrace <- false
    game.IsFixedTimeStep          <- false
    game.TargetElapsedTime        <- sec (1.0 / 60.0)
    game.Content.RootDirectory    <- "Content"
    game.IsMouseVisible           <- true
    game.Window.AllowUserResizing <- true
    game.SetResolution windowWidth windowHeight

    Input.Mouse.SetCursor Input.MouseCursor.Crosshair

    game.CalculateViewport ()
    let viewport = game.GraphicsDevice.Viewport
    State.camera   <- Camera.create (virtualWidth,virtualHeight) viewport |> Camera.withMinMaxZoom 0.03f 3f
    State.uiCamera <- Camera.create (virtualWidth,virtualHeight) viewport

    // Event when user resize window
    game.Window.ClientSizeChanged |> Event.add (fun args ->
        let g, w = game.Graphics, game.Window
        g.PreferredBackBufferWidth  <- w.ClientBounds.Width
        g.PreferredBackBufferHeight <- w.ClientBounds.Height
        g.ApplyChanges()
        game.CalculateViewport ()

        let viewport = game.GraphicsDevice.Viewport
        State.camera   <- Camera.withViewport viewport State.camera
        State.uiCamera <- Camera.withViewport viewport State.uiCamera
    )

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
    let renderTarget = {
        Width  = 640
        Height = 360
    }
    using (new MonoGame<Assets,Model>(init, loadAssets, initModel, update, draw, renderTarget)) (fun game ->
        game.Run()
    )
    1
