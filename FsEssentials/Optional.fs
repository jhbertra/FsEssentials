namespace FsEssentials

open Prelude

module Option =

    let (>>=) oa fa =
        match oa with
        | None -> None
        | Some a -> fa a


    let (>=>) f1 f2 = f1 >> (>>=) f2


    let map fa oa = oa >>= (fa >> Some)


    let (<*>) ofa = function
    | None -> None
    | Some a -> map ((|>) a) ofa


    let (<^>) fa = (<*>) (Some fa)


    let (<?>) ofa a = ofa <*> Some a


    let (<|>) oa ob =
        match ( oa , ob ) with
        | ( None , b ) -> b
        | ( a , _  ) -> a


    let lift2 fab oa ob = fab <^> oa <*> ob


    let lift3 fabc oa ob oc = fabc <^> oa <*> ob <*> oc


    let lift4 fabcd oa ob oc od = fabcd <^> oa <*> ob <*> oc <*> od


    let join = function
    | None -> None
    | Some ( None ) -> None
    | Some ( Some a ) -> Some a


    let rec coalesce = function
    | [] -> None
    | o :: o' -> o <|> coalesce o'


    let filter fa oa =
        match map fa oa with
        | Some true -> oa
        | _ -> None


    let rec fold fab a = function
    | [] -> Some a
    | b :: b' -> fab a b >>= flip (fold fab) b'


    type OptionalBuilder() =

        member this.Bind(x, f) = x >>= f

        member this.Combine(a, b) = a <|> b

        member this.Delay(f) = f()

        member this.Return(x) = Some x

        member this.ReturnFrom(o) = o

        member this.Yield(x) = Some x

        member this.YieldFrom(o) = o

        member this.Zero() = None
        

    let builder = OptionalBuilder()


    let defaultIfNone defaultValue option =
        match option with
        | None -> defaultValue
        | Some value -> value


    let toSingletonList = function
    | None -> []
    | Some x -> [x]


    let rec filterNone = function
    | [] -> []
    | Some x :: xs -> x :: filterNone xs
    | None :: xs -> [] @ filterNone xs
