module Applicative

let constant a _ = a
let map = List.map
let (<^) fa = constant fa |> map
let fvoid fa = map (constant ()) fa
let pureA a = [a]
let (<*>) fal al = List.collect (fun fa -> map fa al) fal

// #Typeclass Applicative

// #Inherit Functor

let liftA f x = pureA f <*> x


let liftA2 f x = (<*>) (map f x)


let liftA3 f a b c = liftA2 f a b <*> c


let (|*>) a1 a2 = id <^ a1 <*> a2


let (<*|) x = liftA2 constant x


let (<^>) f = pureA f |> (<*>)


let (<?>) af a = pureA a |> (<*>) af


let filter pred list =
    let cons x xs = x :: xs
    List.foldBack (fun x -> liftA2 (fun flg -> if flg then cons x else id) (pred x)) list (pureA [])
    |> map List.rev
    
    
let sequence ms =
    let cons x xs = x :: xs
    List.fold (fun x m -> cons <^> m <*> x) (pureA []) ms
    |> map List.rev


let whenA b fu = if b then fu else pureA () 


let unlessA b = whenA !b

// #Typeclass End