module Monad

let flip f x y = f y x
let constant a _ = a
let map = List.map
let (<^) fa = constant fa |> map
let fvoid fa = map (constant ()) fa
let pureA a = [a]
let (<*>) fal al = List.collect (fun fa -> map fa al) fal
let (>>=) la fla = List.collect fla la

// #Typeclass Monad

// #Inherit Applicative

let join x = x >>= id


let (>>!) x y = x >>= fun _ -> y


let (=<<) f x = x >>= f


let (>=>) f1 f2 = (fun a -> f1 a >>= f2)


let ret = pureA


let mapM f ms =
    let k a r = f a >>= (fun x -> r >>= (fun xs -> ret (x::xs)))
    List.foldBack k ms (ret [])
    |> map List.rev


let liftM f = f >> ret |> (=<<)


let liftM2 f m1 m2 =
    m1 >>= (fun x1 ->
    m2 >>= (fun x2 ->
    f x1 x2 |> ret))


let liftM3 f m1 m2 m3 =
    m1 >>= (fun x1 ->
    m2 >>= (fun x2 ->
    m3 >>= (fun x3 ->
    f x1 x2 x3 |> ret)))


let liftM4 f m1 m2 m3 m4 =
    m1 >>= (fun x1 ->
    m2 >>= (fun x2 ->
    m3 >>= (fun x3 ->
    m4 >>= (fun x4 ->
    f x1 x2 x3 x4 |> ret))))


let liftM5 f m1 m2 m3 m4 m5 =
    m1 >>= (fun x1 ->
    m2 >>= (fun x2 ->
    m3 >>= (fun x3 ->
    m4 >>= (fun x4 ->
    m5 >>= (fun x5 ->
    f x1 x2 x3 x4 x5 |> ret)))))


let rec fold fab a = function
| [] -> ret a
| b :: b' -> fab a b >>= flip (fold fab) b'

// #Typeclass End