module Monoid

let mempty = 0

let mappend = (+)

// #Typeclass Monoid

let mconcat = List.fold mappend mempty

// #Typleclass End
