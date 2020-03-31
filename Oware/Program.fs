// RULES: http://www.joansala.com/auale/rules/en/
module Oware

type StartingPosition =
    | South
    | North
 
let getSeeds n board = failwith "Not implemented"

let useHouse n board = failwith "Not implemented"

let start position = failwith "Not implemented"

let score board = failwith "Not implemented"

let gameState board = failwith "Not implemented"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
   // 0 // return an integer exit code
    let board = [4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4]
    printfn "Holes: %A" board
    0
(*Thoughts: 
3 Phases: Game states ? Collecting, Sowing, Capturing <- All in a turn




*)