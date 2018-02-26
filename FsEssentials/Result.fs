namespace FsEssentials

open Prelude

module Result =

    let (>>=) ra fa = Result.bind fa ra


    let (>=>) f1 f2 = f1 >> Result.bind f2


    let (<*>) rfa ra =
        match ( rfa , ra ) with
        | ( Error efa , Error ea ) -> Error (ea :: efa)
        | ( Error efa , Ok _ ) -> Error efa
        | ( Ok _ , Error ea ) -> Error [ea]
        | ( Ok fa , Ok a ) -> Ok (fa a)


    let (<^>) fa = (<*>) (Ok fa)


    let (<?>) fa ra = fa <*> Ok ra


    let (<|>) ra rb =
        match ( ra , rb ) with
        | ( Error _ , b ) -> b
        | ( a , _  ) -> a


    let lift2 fab ra rb = fab <^> ra <*> rb


    let lift3 fabc ra rb rc = fabc <^> ra <*> rb <*> rc


    let lift4 fabcd ra rb rc rd = fabcd <^> ra <*> rb <*> rc <*> rd


    let join = function
    | Error e -> Error e
    | Ok ( Error e ) -> Error e
    | Ok ( Ok a ) -> Ok a


    let rec coalesce e = function
    | [] -> Error e
    | o :: o' -> o <|> coalesce e o'


    let filter fa oa =
        match Result.map fa oa with
        | Ok true -> oa
        | e -> e


    let rec fold fab a = function
    | [] -> Ok a
    | b :: b' -> fab a b >>= flip (fold fab) b'


    type ResultBuilder() =

        member this.Bind(x, f) = x >>= f

        member this.Combine(a, b) = a <|> b

        member this.Delay(f) = f()

        member this.Return(x) = Ok x

        member this.ReturnFrom(o) = o

        member this.Yield(x) = Ok x

        member this.YieldFrom(o) = o
        

    let builder = ResultBuilder()


    let fromOption e = function
    | None -> Error e
    | Some a -> Ok a