module MonadPlus

let constant a _ = a
let map = List.map
let (<^) fa = constant fa |> map
let fvoid fa = map (constant ()) fa
let pureA a = [a]
let (<*>) fal al = List.collect (fun fa -> map fa al) fal
let liftA f x = pureA f <*> x
let liftA2 f x = (<*>) (map f x)
let liftA3 f a b c = liftA2 f a b <*> c
let (|*>) a1 a2 = id <^ a1 <*> a2
let (<*|) x = liftA2 constant x
let (<^>) f = pureA f |> (<*>)
let (<?>) af a = pureA a |> (<*>) af
let (<|>) oa ob = oa @ ob
let empty = []
let flip f x y = f y x
let (>>=) la fla = List.collect fla la
let join x = x >>= id
let (>>!) x y = x >>= fun _ -> y
let (=<<) f x = x >>= f
let (>=>) f1 f2 = (fun a -> f1 a >>= f2)
let ret = pureA
let mapM f ms =
    let k a r = f a >>= (fun x -> r >>= (fun xs -> ret (x::xs)))
    List.foldBack k ms (ret [])
let sequence ms = mapM id ms
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

// #Typeclass MonadPlus

// #Inherit Alternative
// #Inherit Monad

let mzero = empty


let mplus ma = (<|>) ma


let where pred ma = ma >>= (fun a -> if pred a then ret a else empty)

// #Typeclass End