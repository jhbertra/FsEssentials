module Alternative

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

// #Typeclass Alternative

// #Inherit Applicative

let rec some v = 
    let cons x xs = x :: xs
    cons <^> v <*> many v
and many v = some v <|> pureA []


let optional v = Some <^> v <|> pureA None


let sum fas = List.foldBack (<|>) fas empty


let guard = function
| true -> pureA ()
| false -> empty

// #Typeclass End