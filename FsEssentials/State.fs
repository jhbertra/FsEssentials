namespace FsEssentials

open Prelude


type State<'s, 'a> = State of ('s -> 's * 'a)


module State =


    let inline create s = State s


    let inline fromValue x = create ( fun s -> ( s , x ) )


    let inline run initial ( State f ) = f initial


    let inline bind fa sa = 
        create ( fun s -> 
            let ( s , a ) = run s sa
            fa a |> run s )


    let (>>=) ra fa = bind fa ra


    let map fa ra = ra >>= (fa >> fromValue)


    let inline eval initial = run initial >> snd


    let inline exec initial = run initial >> fst


    let get = State (fun s -> (s,s))


    let inline put newState = State (fun s -> (newState, ()))


    let inline modify f = get >>= (f >> put)


    let inline gets f = get |> map f


    let pureA = fromValue


    let (<*>) sfa sa =
        create ( fun s ->
            let ( s , fa ) = run s sfa
            let ( s , a ) = run s sa
            fa a |> fromValue |> run s )


    let empty = State (fun s -> ( s , () ) )


    let (<|>) s1 s2 = 
        create ( fun s ->
            let ( s , result ) = run s s1
            run s s2 )


    // #Instance MonadPlus

    // #Typeclass Functor
    
    let (<^) fa = constant fa |> map
    
    
    let fvoid fa = map (constant ()) fa
    
    // #Typeclass End
    // #Typeclass Applicative
    
    
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
    // #Typeclass Monad
    
    
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
    // #Typeclass Alternative
    
    
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
    // #Typeclass MonadPlus
    
    
    let mzero = empty
    
    
    let mplus ma = (<|>) ma
    
    
    let where pred ma = ma >>= (fun a -> if pred a then ret a else empty)
    
    // #Typeclass End

    // #Instance End


    type StateBuilder() =

        member this.Bind(x, f) = x >>= f

        member this.Combine(a, b) = a <|> b

        member this.Delay(f) = f()

        member inline this.For (seq : 'a seq, f : 'a -> State<'b, 'c>) = Seq.map f seq

        member this.Return(x) = ret x

        member this.ReturnFrom(o) = o

        member this.Yield(x) = ret x

        member this.YieldFrom(o) = o

        member this.While (f : unit -> bool, x : State<'a, 'b>) = if f () then x <|> this.While (f, x) else x

        member this.Zero () = mzero


    let state = StateBuilder()
