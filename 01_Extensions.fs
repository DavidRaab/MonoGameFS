namespace MyGame
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type HashSet<'a>       = System.Collections.Generic.HashSet<'a>
type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>
type TimeSpan          = System.TimeSpan

[<AutoOpen>]
module Extensions =
    // Milliseconds
    [<Measure>] type ms
    // Radiant and Degree types
    [<Measure>] type rad
    [<Measure>] type deg

    // Partial Active Patterns
    let (|IsGreater|_|) target input = if input > target then Some input else None
    let (|IsSmaller|_|) target input = if input < target then Some input else None
    let (|IsEqual|_|)   target input = if input = target then Some input else None

    // Common dispatch for Numbers. Do something if number
    // is greater, smaller or equal zero
    let inline cmp fGreater fSmaller fEqual x =
        if   x > 0.0 then fGreater x
        elif x < 0.0 then fSmaller x
        else fEqual x

    let inline cmpF fGreater fSmaller fEqual x =
        if   x > 0.0f then fGreater x
        elif x < 0.0f then fSmaller x
        else fEqual x

    let inline cmpInt fGreater fSmaller fEqual x =
        if   x > 0 then fGreater x
        elif x < 0 then fSmaller x
        else fEqual x

    // Returns always the same value as a function
    let is x _ = x

    // StackTrace as a String
    let stackTrace (skip:int) =
        let skip = if skip < 0 then 1 else skip+1
        let st   = System.Diagnostics.StackTrace(skip, true)
        "  " + System.String.Join("\n  ",
            st.GetFrames() |> Array.map (fun frame ->
                sprintf "%s:%d %s"
                    (System.IO.Path.GetFileName(frame.GetFileName()))
                    (frame.GetFileLineNumber())
                    (frame.GetMethod().Name)
            )
        )

    let sec sec =
        TimeSpan.FromSeconds sec

    let clamp (min:float) max value =
        System.Math.Clamp(value, min, max)

    let clampF (min:float32) max value =
        System.Math.Clamp(value, min, max)

    let inline nearly target difference value =
        (abs (value - target)) < difference

    let inline notNearly target difference value =
        not (nearly target difference value)

    module Texture2D =
        let create gd width height data =
            let tex = new Texture2D(gd, width, height)
            tex.SetData data
            tex

    type Graphics.SpriteBatch with
        member this.Draw(
            texture         : Texture2D,  position : Vector2, scale   : Vector2,
            sourceRectangle : Rectangle,  color    : Color,
            rotation        : float<rad>, origin   : Vector2, effects : SpriteEffects,
            layerDepth      : float32
        ) =
            this.Draw(
                texture         = texture,
                position        = position,
                scale           = scale,
                sourceRectangle = sourceRectangle,
                color           = color,
                rotation        = float32 rotation,
                origin          = origin,
                effects         = effects,
                layerDepth      = layerDepth
            )

        member this.DrawO(
            texture: Texture2D, position: Vector2,
            ?sourceRectangle: Rectangle, ?scale: Vector2, ?color: Color,
            ?rotation: float<rad>, ?origin: Vector2,
            ?effects: SpriteEffects, ?layerDepth: float32
        ) =
            let srcRect    = defaultArg sourceRectangle (Rectangle(0,0,texture.Width, texture.Height))
            let scale      = defaultArg scale            Vector2.One
            let color      = defaultArg color            Color.White
            let rotation   = defaultArg rotation         0.0<rad>
            let origin     = defaultArg origin           Vector2.Zero
            let effects    = defaultArg effects          SpriteEffects.None
            let layerDepth = defaultArg layerDepth       0f
            this.Draw(texture, position, scale, srcRect, color, rotation, origin, effects, layerDepth)


    module TimeSpan =
        let oneSecond = sec 1.0

    type Vector2 with
        static member create (x:float32) (y:float32) =
            Vector2(x,y)
        static member Multiply (left:Vector2, right:TimeSpan) =
            Vector2.Multiply(left, float32 right.TotalSeconds)

        static member toPoint (vec:Vector2) = vec.ToPoint ()
        static member length  (vec:Vector2) = vec.Length ()

        static member left  = Vector2(-1f,  0f)
        static member right = Vector2( 1f,  0f)
        static member up    = Vector2( 0f, -1f)
        static member down  = Vector2( 0f,  1f)

        static member addX x (vec:Vector2) = Vector2(vec.X+x, vec.Y  )
        static member addY y (vec:Vector2) = Vector2(vec.X  , vec.Y+y)
        static member flipY  (vec:Vector2) = Vector2(vec.X  ,-vec.Y  )

    type System.Collections.Generic.Dictionary<'a,'b> with
        static member add key value (dic:Dictionary<'a,'b>) =
            if   dic.ContainsKey(key)
            then dic.[key] <- value
            else dic.Add(key,value)

        static member change key f (dic:Dictionary<'a,'b>) =
            match dic.TryGetValue key with
            | true, value ->
                match f (ValueSome value) with
                | ValueNone   -> ignore (dic.Remove key)
                | ValueSome x -> dic.[key] <- x
            | false, _ ->
                match f ValueNone with
                | ValueNone   -> ()
                | ValueSome x -> dic.Add(key, x)

        static member changeValue defaultValue key f (dic:Dictionary<'a,'b>) =
            match dic.TryGetValue key with
            | true, value -> dic.[key] <- f value
            | false, _    -> dic.Add(key, f defaultValue)

        static member find key (dic:Dictionary<'a,'b>) =
            match dic.TryGetValue key with
            | true, value -> ValueSome value
            | false, _    -> ValueNone

    module Map =
        let changeValue (init:'Value) (key:'Key) f map =
            Map.change key (function
                | None   -> Some (f init)
                | Some x -> Some (f x)
            ) map

    module HashSet =
        let intersect (x:HashSet<'a>) (y:HashSet<'a>) =
            let smaller, greater =
                if x.Count < y.Count then x,y else y,x

            let newHashSet = HashSet<'a>()
            for x in smaller do
                if greater.Contains x then
                    ignore (newHashSet.Add x)

            newHashSet

        let clone (set:HashSet<'a>) =
            let nh = HashSet()
            for x in set do
                nh.Add x |> ignore
            nh

        let intersectMany (sets:seq<HashSet<'a>>) =
            if Seq.isEmpty sets then
                HashSet()
            else
                let smallest = clone (sets |> Seq.minBy (fun set -> set.Count))
                for set in sets do
                    smallest.IntersectWith set
                smallest
