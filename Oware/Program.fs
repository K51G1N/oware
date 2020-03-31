// RULES: http://www.joansala.com/auale/rules/en/
module Oware

type StartingPosition =
    | South
    | North

let getSeeds n (board:int list) = board.Item(n-1)  //g17e4476: get's the seed's at a location on the board, n-1 as we always start from 0

let useHouse n board = failwith "Not implemented"

let start position = failwith "Not implemented"

let score board = failwith "Not implemented"

let gameState board = failwith "Not implemented"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    let n = 12
   // 0 // return an integer exit code
    (*
   // Trying to get the getSeeds function working and it does. 
    let board =[1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12]
    let x = getSeeds n board
    printfn "The  nr of seeds: %A" x
    printfn "Holes: %A" board
    *)
    0
