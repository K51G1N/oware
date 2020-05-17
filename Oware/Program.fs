// RULES: http://www.joansala.com/auale/rules/en/
module Oware
open System

type StartingPosition =
    | North  // A
    | South  // B 

type Player = {  //A = North, B = South
    score: int
    holes: int*int*int*int*int*int  // g17e4476: holes/house interchangeable.
    nrPieces: int                   // g17e4476: nr of pieces on the board
}
type Board = {
   PlayerA: Player  
   PlayerB: Player
   Turn: StartingPosition //g17e4476: North/South we let South go first
}


// g17e4476: Returns the nr of seeds in the nth hole
let getSeeds n board = 
   let (aN,bN,cN,dN,eN,fN),(aS,bS,cS,dS,eS,fS) = board.PlayerA.holes,board.PlayerB.holes //g17e4476: distinguish North & South
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
   | _ -> failwith "Invalid Choice of hole"  // g17e4476: If user picks an invalid hole

//Used in the useHouse function 
//g19p6350
let incrementSeed n (aN,bN,cN,dN,eN,fN,aS,bS,cS,dS,eS,fS) = 
    //Used to increment preceeding houses
    match n with 
    //South side
    |1 -> (aN+1,bN,cN,dN,eN,fN,aN',bN',cN',dN',eN',fN')
    |2 -> (aN,bN+1,cN,dN,eN,fN,aN',bN',cN',dN',eN',fN')
    |3 -> (aN,bN,cN+1,dN,eN,fN,aN',bN',cN',dN',eN',fN')
    |4 -> (aN,bN,cN,dN+1,eN,fN,aN',bN',cN',dN',eN',fN')
    |5 -> (aN,bN,cN,dN,eN+1,fN,aN',bN',cN',dN',eN',fN')
    |6 -> (aN,bN,cN,dN,eN,fN+1,aN',bN',cN',dN',eN',fN')
    //North side 
    |7 -> (aS,bS,cS,dS,eS,fS,aS'+1,bS',cS',dS',eS',fS')
    |8 -> (aS,bS,cS,dS,eS,fS,aS',bS'+1,cS',dS',eS',fS')
    |9 -> (aS,bS,cS,dS,eS,fS,aS',bS',cS'+1,dS',eS',fS')
    |10 -> (aS,bS,cS,dS,eS,fS,aS',bS',cS',dS'+1,eS',fS')
    |11 -> (aS,bS,cS,dS,eS,fS,aS',bS',cS',dS',eS'+1,fS')
    |12 -> (aS,bS,cS,dS,eS,fS,aS',bS',cS',dS',eS',fS'+1)
    |_ -> failwith "{incrementSeed} out of range"


let useHouse n board = failwith "Not implemented"



let start position = 
   let hole = (4,4,4,4,4,4)
   let pA = {holes = hole ; score = 0; nrPieces = 24 }
   let pB = {holes = hole ; score = 0; nrPieces = 24 }
   {PlayerA = pA; PlayerB = pB; Turn = position}

// g17e4476: The tuple set up as specified to return NorthScore (NS) and South Score (SS)
let score board =
   let southScore,northScore = board.PlayerA.score , board.PlayerB.score 
   southScore,northScore  
 
// Sets up the string of the score for north and south (NS, SS respectively)
let returnScore board = 
   let NS = board.PlayerA.score |> string
   let SS = board.PlayerB.score |> string
   "North Score: " + NS + "\nSouth Score: " + SS + "\n"

// g17e4476: returns the state of the game using the strings as specified
let gameState board =
   let a,b = score board // g17e4476: player a's score, player b's score (Ns and SS respectively)
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



let chooseHouse n board = 
    //g19p6350
    //The player takes their turn given a house number 
    //then the chosen house is set to 0
    let (aN,bN,cN,dN,eN,fN) = board.PlayerA.houses
    let (aS,bS,cS,dS,eS,fS) = board.PlayerB.houses
    match n with
    |1  -> {board with PlayerA = {board.PlayerA with houses = (0,bN,cN,dN,eN,fN)} }
    |2  -> {board with PlayerA = {board.PlayerA with houses = (aN,0,cN,dN,eN,fN)} }
    |3  -> {board with PlayerA = {board.PlayerA with houses = (aN,bN,0,dN,eN,fN)} } 
    |4  -> {board with PlayerA = {board.PlayerA with houses = (aN,bN,cN,0,eN,fN)} }
    |5  -> {board with PlayerA = {board.PlayerA with houses = (aN,bN,cN,dN,0,fN)} } 
    |6  -> {board with PlayerA = {board.PlayerA with houses = (aN,bN,cN,dN,eN,0)} } 
    |7  -> {board with PlayerB = {board.PlayerB with houses = (0,bS,cS,dS,eS,fS)} }
    |8  -> {board with PlayerB = {board.PlayerB with houses = (aS,0,cS,dS,eS,fS)} }
    |9  -> {board with PlayerB = {board.PlayerB with houses = (aS,bS,0,dS,eS,fS)} }
    |10 -> {board with PlayerB = {board.PlayerB with houses = (aS,bS,cS,0,eS,fS)} } 
    |11 -> {board with PlayerB = {board.PlayerB with houses = (aS,bS,cS,dS,0,fS)} }
    |12 -> {board with PlayerB = {board.PlayerB with houses = (aS,bS,cS,dS,eS,0)} }
    |_  -> failwith "{chooseHouse} house is not in 1 and 12 range."

//Used in the useHouse function below
// g17e4476: returns the nth Hole and the nr of seeds in the nth hole
let returnHole n board = 
   let hole = getSeeds n board
   " House " + string n + ": " + string hole

 // g17e4476: recursively goes through each hole displaying the contents of the hole. Impure output
let printHoles board =
   let rec print count =
      match count with 
      | 12 -> printfn " House %i: %i" count (getSeeds count board)
      | 13 -> ()
      | _ -> printfn "%s" (returnHole count board);(print (count+1))
   print 1

let nextPlayersTurn position = 
    //Simple function that is used to alternate player turns.
    match position with
    | South -> North //this means that South (player one) just had their turn and now it is North's (player two's) turn.
    | North -> South //this means that North (player two) just had their turn and now it is South's (player one's) turn


   // g17e4476: We can play the game. Impure input
let beginGame board = 
   let rec Game board =
       Console.Clear () // g17e4476: Clears the console at the start of every turn/round
       let state =  (gameState board) // g17e4476: Fetches the games state
       match state with
       | "South won" -> printfn "{%s}" state  
       | "North won"  -> printfn "{%s}" state 
       | "Game ended in a draw" -> printfn "{%s}" state 
       | _ -> 
       printfn "%s" state
       printHoles board
       printfn "%s" (returnScore board)
       
       printfn "Choose a hole to play"
       let hole = Console.ReadLine() |> string  // g17e4476: takes user input of the hole to use
       let hole = (useHouse (hole |> int) board) // g17e4476: converts input to int and then applys useHouse function
       Game hole                           
   Game (start North)

[<EntryPoint>]
let main _ =
   // g17e4476: Get's us into the game
    let beginning = beginGame (start North)
    0
