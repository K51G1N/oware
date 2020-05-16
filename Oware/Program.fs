// RULES: http://www.joansala.com/auale/rules/en/
module Oware
open System

type StartingPosition =
    | North  // A
    | South  // B 

type Player = {  //A = North, B = South
    score: int
    holes: int*int*int*int*int*int //holes/house interchangeable.
    nrPieces: int
}
type Board = {
   PlayerA: Player  
   PlayerB: Player
   Turn: StartingPosition
}

let getSeeds n board = 
   let (aN,bN,cN,dN,eN,fN),(aS,bS,cS,dS,eS,fS) = board.PlayerA.holes,board.PlayerB.holes
   match n with 
   | 1 -> aN
   | 2 -> bN
   | 3 -> cN
   | 4 -> dN
   | 5 -> eN
   | 6 -> fN
   | 7 -> aS
   | 8 -> bS
   | 9 -> cS
   | 10 -> dS
   | 11 -> eS
   | 12 -> fS
   | _ -> failwith "Invalid Choice of hole"


let useHouse n board = failwith "Not implemented"

let start position = 
   let hole = (4,4,4,4,4,4)
   let pA = {holes = hole ; score = 0; nrPieces = 24 }
   let pB = {holes = hole ; score = 0; nrPieces = 24 }
   {PlayerA = pA; PlayerB = pB; Turn = position}

let score board =
   let southScore,northScore = board.PlayerA.score , board.PlayerB.score 
   southScore,northScore  
 
let returnScore board = 
   let NS = board.PlayerA.score |> string
   let SS = board.PlayerB.score |> string
   "North Score: " + NS + "\nSouth Score: " + SS + "\n"

let gameState board =
   let a,b = score board
   match a > 24 with 
   |true -> "North won"
   |false -> 
        match b > 24  with 
        |true -> "South won"
        |false -> 
            match a = 24 && b = 24 with 
            |true ->  "Game ended in a draw"
            |false ->  
                match board.Turn with  
                |North -> "North's turn"
                |South -> "South's turn"

let returnHole n board = 
   let hole = getSeeds n board
   " House " + string n + ": " + string hole

let printHoles board =
   let rec print count =
      match count with 
      | 12 -> printfn "House %i: %i" count (getSeeds count board)
      | 13 -> ()
      | _ -> printfn "%s" (returnHole count board);(print (count+1))
   print 1



let beginGame board = 
   let rec Game board =
       Console.Clear () //wipe it at start of each turn to make things look nicer
       let state =  (gameState board)
       match state with
       | "South won" -> printfn "{%s}" state  
       | "North won"  -> printfn "{%s}" state 
       | "Game ended in a draw" -> printfn "{%s}" state 
       | _ -> 
       printfn "%s" state
       printHoles board
       printfn "%s" (returnScore board)
       
       printfn "Choose a hole to play"
       let hole = Console.ReadLine() |> string
       let hole = (useHouse (hole |> int) board)
       Game hole                           
   Game (start North)

[<EntryPoint>]
let main _ =
   //  printfn "Hello from F#!"
    let beginning = beginGame (start North)
   // 0 // return an integer exit code
    (*
   // Trying to get the getSeeds function working and it does. 
    let board =[1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12]
    let x = getSeeds n board
    printfn "The  nr of seeds: %A" x
    printfn "Holes: %A" board
    *)
    0
  