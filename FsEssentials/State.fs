namespace FsEssentials

open Prelude

module State =

    type State<'s, 'a> = State of ('s -> 's * 'a)


    let inline create s = State s


    let inline fromValue x = create ( fun s -> ( s , x ) )


    let inline run initial ( State f ) = f initial


    let inline bind fa sa = 
        create ( fun s -> 
            let ( s , a ) = run s sa
            fa a |> run s )


    let (>>=) sa fa = bind fa sa


    let (>=>) f1 f2 = f1 >> (>>=) f2


    let (<*>) sfa sa =
        create ( fun s ->
            let ( s , fa ) = run s sfa
            let ( s , a ) = run s sa
            fa a |> fromValue |> run s )


    let (<^>) fa = (<*>) (fromValue fa)


    let (<?>) fa ra = fa <*> fromValue ra


    let (<+>) s1 s2 = 
        create ( fun s ->
            let result , s = run s s1
            run s s2 )


    let lift2 fab sa sb = fab <^> sa <*> sb


    let lift3 fabc sa sb sc = fabc <^> sa <*> sb <*> sc


    let lift4 fabcd sa sb sc sd = fabcd <^> sa <*> sb <*> sc <*> sd


    let join ssa =
        create (fun s ->
            let ( s , sa ) = run s ssa
            run s sa )


    let rec fold fab a = function
    | [] -> fromValue a
    | b :: b' -> fab a b >>= flip (fold fab) b'


    let inline eval initial = run initial >> fst


    let inline exec initial = run initial >> snd


    let inline map f = f >> fromValue |> bind


    let get = State (fun s -> (s,s))


    let inline put newState = State (fun s -> ((), newState))


    let inline modify f = get |> bind (f >> put)


    let inline gets f = get |> map f


    type StateBuilder() =

        member inline this.Bind ( x , f ) = bind f x

        member inline this.Combine (x1, x2) = x1 <+> x2

        member inline this.Delay f = f ()

        member inline this.For (seq, f) =
            if Seq.length seq = 0 then
                this.Zero ()
            else
                seq
                |> Seq.map f
                |> Seq.reduceBack (<+>)

        member inline this.Return x = fromValue x

        member inline this.ReturnFrom x = x

        member this.While (f, x) =
            if f () then
                x <+> this.While (f, x)
            else
                this.Zero ()

        member inline this.Zero () = create ( fun s -> (), s )


    let builder = StateBuilder()