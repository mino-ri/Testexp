module Testexp.Test.Target

let tuple1of2 (x, _) = x

let tuple1of3 (x, _, _) = x

let tuple1of4 (x, _, _, _) = x

let tuple1of5 (x, _, _, _, _) = x

let tuple1of6 (x, _, _, _, _, _) = x

let tuple1of7 (x, _, _, _, _, _, _) = x

let tuple1of8 (x, _, _, _, _, _, _, _) = x

let curry1of2 (x: string) _ = x

let curry1of3 (x: string) _ _ = x

let curry1of4 (x: string) _ _ _ = x

let curry1of5 (x: string) _ _ _ _ = x

let curry1of6 (x: string) _ _ _ _ _ = x

let curry1of7 (x: string) _ _ _ _ _ _ = x

let curry1of8 (x: string) _ _ _ _ _ _ _ = x

let alwaysFail () : unit = failwith "test"
