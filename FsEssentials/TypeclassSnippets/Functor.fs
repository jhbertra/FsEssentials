module Functor

let constant a _ = a
let map = List.map

// #Typeclass Functor

let (<^) fa = constant fa |> map


let fvoid fa = map (constant ()) fa

// #Typeclass End