namespace FsEssentials

open Prelude
open System.Xml

module Result =

    let (>>=) sa fa = Result.bind fa sa


    let map fa sa = sa >>= (fa >> Ok)


    let pureA x = Ok x


    let (<*>) rfa ra =
        match ( rfa , ra ) with
        | ( Error es , Error e ) -> Error (es @ [e])
        | ( Error es, _ ) -> Error es
        | ( _ , Error e ) -> Error [e]
        | ( Ok fa , Ok a ) -> fa a |> Ok


    let (<|>) sa rb =
        match ( sa , rb ) with
        | ( Error _ , b ) -> b
        | ( a , _  ) -> a


    // #Instance Monad

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

    // #Instance End
    
    
    let filter pred =
        List.fold 
            (fun x a ->
                match ( x , pred a ) with
                | ( Error es , Error e ) -> Error (es @ [e])
                | ( Error _ , _ ) -> x
                | ( _ , Error e ) -> Error [e]
                | ( Ok xs , Ok true ) -> Ok (xs @ [a])
                | ( _ , Ok false) -> x 
            )
            (pureA [])
        
        
    let sequence results =
        List.fold 
            (fun x a ->
                match ( x , a ) with
                | ( Error es , Error e ) -> Error (es @ [e])
                | ( Error _ , _ ) -> x
                | ( _ , Error e ) -> Error [e]
                | ( Ok xs , Ok x ) -> Ok (xs @ [x])
            )            
            (pureA [])
            results


    type ResultBuilder() =

        member this.Bind(x, f) = x >>= f

        member this.Combine(a, b) = a <|> b

        member this.Delay(f) = f()

        member inline this.For (seq : 'a seq, f : 'a -> Result<'b, 'c>) = Seq.map f seq

        member this.Return(x) = ret x

        member this.ReturnFrom(o) = o

        member this.Yield(x) = ret x

        member this.YieldFrom(o) = o
        

    let validate = ResultBuilder()


    let fromOption e = function
    | None -> Error e
    | Some a -> Ok a
