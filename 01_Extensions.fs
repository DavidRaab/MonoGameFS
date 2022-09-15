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
    // Radiant and Degree types
    [<Measure>] type rad
    [<Measure>] type deg
    let deg2rad (degree:float32<deg>) =
        degree * MathHelper.Pi / 180.0f * 1.0f<rad/deg>

    let rad2deg (radiant:float32<rad>) =
        radiant * 180.0f / MathHelper.Pi * 1.0f<deg/rad>

    let sec sec =
        TimeSpan.FromSeconds sec

    module Texture2D =
        let create gd width height data =
            let tex = new Texture2D(gd, width, height)
            tex.SetData data
            tex

    module TimeSpan =
        let oneSecond = sec 1.0

    type Vector2 with
        static member create (x:float32) (y:float32) =
            Vector2(x,y)
        static member Multiply (left:Vector2, right:TimeSpan) =
            Vector2.Multiply(left, float32 right.TotalSeconds)
        static member toPoint (vec:Vector2) =
            vec.ToPoint ()
        static member right (scaling:float32) =
            Vector2(scaling, 0f)
        static member left (scaling:float32) =
            Vector2(-scaling, 0f)
        static member up (scaling:float32) =
            Vector2(0f, scaling)
        static member down (scaling:float32) =
            Vector2(0f, -scaling)
        static member addX x (vec:Vector2) =
            Vector2(vec.X+x,vec.Y)
        static member addY y (vec:Vector2) =
            Vector2(vec.X,vec.Y+y)

    type System.Collections.Generic.Dictionary<'a,'b> with
        static member add key value (dic:Dictionary<'a,'b>) =
            if   dic.ContainsKey(key)
            then dic.[key] <- value
            else dic.Add(key,value)

        static member change key f (dic:Dictionary<'a,'b>) =
            let mutable value = Unchecked.defaultof<_>
            if dic.TryGetValue(key, &value) then
                match f (ValueSome value) with
                | ValueNone   -> ignore (dic.Remove key)
                | ValueSome x -> dic.[key] <- x
            else
                match f ValueNone with
                | ValueNone   -> ()
                | ValueSome x -> dic.Add(key, x)

        static member changeValue defaultValue key f (dic:Dictionary<'a,'b>) =
            let mutable value = Unchecked.defaultof<_>
            if   dic.TryGetValue(key, &value)
            then dic.[key] <- f value
            else dic.Add(key, f defaultValue)

        static member find key (dic:Dictionary<'a,'b>) =
            let mutable value = Unchecked.defaultof<_>
            if   dic.TryGetValue(key, &value)
            then ValueSome value
            else ValueNone

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
