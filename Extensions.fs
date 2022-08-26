namespace MyGame
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

open System.Collections.Generic

[<AutoOpen>]
module Extensions =
    module Texture2D =
        let create gd width height data =
            let tex = new Texture2D(gd, width, height)
            tex.SetData data
            tex

    module TimeSpan =
        let oneSecond = TimeSpan.FromSeconds(1.0)

    type Vector2 with
        static member create (x:float32) (y:float32) =
            Vector2(x,y)
        static member Multiply (left:Vector2, right:TimeSpan) =
            Vector2.Multiply(left, float32 right.TotalSeconds)
        static member toPoint (vec:Vector2) =
            vec.ToPoint ()

    type Dictionary<'a,'b> with
        static member add key value (dic:Dictionary<'a,'b>) =
            if   dic.ContainsKey(key)
            then dic.[key] <- value
            else dic.Add(key,value)

        static member change init key f (dic:Dictionary<'a,'b>) =
            let mutable value = Unchecked.defaultof<_>
            if   dic.TryGetValue(key, &value)
            then dic.[key] <- f value
            else dic.Add(key, f init)

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