﻿ // RULES: http://www.joansala.com/auale/rules/en/
module Oware
open System

type StartingPosition =
    | North  // A
    | South  // B 

type Player = {  //A = North, B = South
    score: int
    holes: int*int*int*int*int*int  // g17e4476: holes/house interchangeable.
    nrSeeds: int                   // g17e4476: nr of pieces on the board
}
type Board = {
   PlayerA: Player  
   PlayerB: Player
   Turn: StartingPosition //g17e4476: North/South we let North go first
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

   // This returns the nr of seeds on the opponents side
let opponentSeeds board =
   let turn = board.Turn
   match turn with
   | North -> board.PlayerA.nrSeeds
   | South -> board.PlayerB.nrSeeds

//g19p6350
let incrementSeedCount n (aN,bN,cN,dN,eN,fN,aS,bS,cS,dS,eS,fS) = 
    //Used to increment preceeding houses
    match n with 
    //North side
    |1 -> (aN+1,bN,cN,dN,eN,fN,aS,bS,cS,dS,eS,fS)
    |2 -> (aN,bN+1,cN,dN,eN,fN,aS,bS,cS,dS,eS,fS)
    |3 -> (aN,bN,cN+1,dN,eN,fN,aS,bS,cS,dS,eS,fS)
    |4 -> (aN,bN,cN,dN+1,eN,fN,aS,bS,cS,dS,eS,fS)
    |5 -> (aN,bN,cN,dN,eN+1,fN,aS,bS,cS,dS,eS,fS)
    |6 -> (aN,bN,cN,dN,eN,fN+1,aS,bS,cS,dS,eS,fS)
    //North Houses 
    |7 -> (aN,bN,cN,dN,eN,fN,aS+1,bS,cS,dS,eS,fS)
    |8 -> (aN,bN,cN,dN,eN,fN,aS,bS+1,cS,dS,eS,fS)
    |9 -> (aN,bN,cN,dN,eN,fN,aS,bS,cS+1,dS,eS,fS)
    |10 ->(aN,bN,cN,dN,eN,fN,aS,bS,cS,dS+1,eS,fS)
    |11 ->(aN,bN,cN,dN,eN,fN,aS,bS,cS,dS,eS+1,fS)
    |12 ->(aN,bN,cN,dN,eN,fN,aS,bS,cS,dS,eS,fS+1)
    |_ -> failwith "{incrementSeedCount} out of range"

let chosenHole n board = 
    //g19p6350
    //The player takes their turn given a house number 
    //then the chosen house is set to 0
    let (aN,bN,cN,dN,eN,fN) = board.PlayerA.holes
    let (aS,bS,cS,dS,eS,fS) = board.PlayerB.holes
    match n with
    |1  -> {board with PlayerA = {board.PlayerA with holes = (0,bN,cN,dN,eN,fN)} }
    |2  -> {board with PlayerA = {board.PlayerA with holes = (aN,0,cN,dN,eN,fN)} }
    |3  -> {board with PlayerA = {board.PlayerA with holes = (aN,bN,0,dN,eN,fN)} } 
    |4  -> {board with PlayerA = {board.PlayerA with holes = (aN,bN,cN,0,eN,fN)} }
    |5  -> {board with PlayerA = {board.PlayerA with holes = (aN,bN,cN,dN,0,fN)} } 
    |6  -> {board with PlayerA = {board.PlayerA with holes = (aN,bN,cN,dN,eN,0)} } 
    |7  -> {board with PlayerB = {board.PlayerB with holes = (0,bS,cS,dS,eS,fS)} }
    |8  -> {board with PlayerB = {board.PlayerB with holes = (aS,0,cS,dS,eS,fS)} }
    |9  -> {board with PlayerB = {board.PlayerB with holes = (aS,bS,0,dS,eS,fS)} }
    |10 -> {board with PlayerB = {board.PlayerB with holes = (aS,bS,cS,0,eS,fS)} } 
    |11 -> {board with PlayerB = {board.PlayerB with holes = (aS,bS,cS,dS,0,fS)} }
    |12 -> {board with PlayerB = {board.PlayerB with holes = (aS,bS,cS,dS,eS,0)} }
    |_  -> failwith "{chooseHouse} house is not in 1 and 12 range."  


let incrementScore lastHousePlaced turn board= 
  //this function will capture seeds and adjust the score.
  let rec scoreAdder  lastHousePlaced board =
    let plANumPieces = board.PlayerA.nrSeeds
    let plBNumPieces = board.PlayerB.nrSeeds
    match lastHousePlaced<13 && lastHousePlaced>0 with //makes sure lastHouse is valid
    |true ->
        match turn=North with
        |true -> match getSeeds lastHousePlaced board, lastHousePlaced<7 && lastHousePlaced>0, (plANumPieces - 2 > 0 || (plANumPieces = 2 && plBNumPieces = 0)), (plANumPieces - 3 > 0 || (plANumPieces = 3 && plBNumPieces = 0)) with //if it is north's turn, seeds should only be collected from south's side
                 | 2,true,true,_ -> let newboard = (scoreAdder (lastHousePlaced - 1) (chosenHole lastHousePlaced board)) //updates board to remove collected seeds
                                    {newboard with PlayerA = {newboard.PlayerA with score = newboard.PlayerA.score + 2}; PlayerB = {newboard.PlayerB with nrSeeds = newboard.PlayerB.nrSeeds - 2}}//adds to score
                 | 3,true,_,true -> let newboard = (scoreAdder (lastHousePlaced - 1) (chosenHole lastHousePlaced board))
                                    {newboard with PlayerA = {newboard.PlayerA with score = newboard.PlayerA.score + 3}; PlayerB = {newboard.PlayerB with nrSeeds = newboard.PlayerB.nrSeeds - 3}}
                 | _ -> board
        |_  -> match getSeeds lastHousePlaced board, lastHousePlaced<13 && lastHousePlaced>6, (plBNumPieces - 2 > 0 || (plBNumPieces = 2 && plANumPieces = 0)), (plBNumPieces - 3 > 0 || (plBNumPieces = 3 && plANumPieces = 0)) with //if it is south's turn, seeds should only be collected from north's side
               | 2, true,true,_ -> let newboard = (scoreAdder (lastHousePlaced - 1) (chosenHole lastHousePlaced board))
                                   {newboard with PlayerB = {newboard.PlayerB with score = newboard.PlayerB.score + 2}; PlayerA = {newboard.PlayerA with nrSeeds = newboard.PlayerA.nrSeeds - 2}}
               | 3, true,_,true ->let newboard = (scoreAdder (lastHousePlaced - 1) (chosenHole lastHousePlaced board))
                                  {newboard with PlayerB = {newboard.PlayerB with score = newboard.PlayerB.score + 3}; PlayerA = {newboard.PlayerA with nrSeeds = newboard.PlayerA.nrSeeds - 3}}
               | _ -> board
    |_ -> board    
  
  scoreAdder lastHousePlaced board  

// g17e4476: The tuple set up as specified to return NorthScore (NS) and South Score (SS)
let score board =
   let southScore,northScore = board.PlayerB.score , board.PlayerA.score 
   southScore,northScore  
// ^^^ Sets up the string of the score for north and south (NS, SS respectively)

// g17e4476: returns the state of the game using the strings as specified
let gameState board =
   let b,a = score board // g17e4476: player a's score, player b's score (Ns and SS respectively)
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

// Player's can't manipulate what doesn't belong to this function verifies this.
// Using the sumative of each side's respective holes we can tell if it's it's own hole or not
let checkOwnHole n position = 
   match position with
   |South -> match (57-n) < 51 with    // Sum(7-12) 
             | true -> false
             | false -> true
   |North -> match (57-n) > 50 with   // Sum(1->6)
             | true -> false
             | false -> true 
   // I do realise I can change the conditioning and then just have true to true and false to false
   // will do so later if time permits

let nextPlayersTurn position = 
    //Simple function that is used to alternate player turns.
    match position with
    | South -> North //this means that South (player A) just had their turn and now it is North's (player two's) turn.
    | North -> South //this means that North (player B) just had their turn and now it is South's (player one's) turn


let useHouse n board =
   let originalState = board //Need to protec the original state of the board if we make an invalid move
   let endHole =
      match (getSeeds n board) + n > 12 with  //If the sum goes above 
      | true -> (getSeeds n board) + n-12
      | _ -> (getSeeds n board) + n
   
    //Validation of hole choice
   match checkOwnHole n board.Turn with
   | false -> originalState //Illegal option and returns the original state of the board 
   | _ ->
      match getSeeds n board with 
      | 0 -> board  //User has chosen an empty hole
      |_ ->
         let (aN,bN,cN,dN,eN,fN),(aS,bS,cS,dS,eS,fS) = (chosenHole n board).PlayerA.holes,(chosenHole n board).PlayerB.holes 
         let holesUpdated = (aN,bN,cN,dN,eN,fN,aS,bS,cS,dS,eS,fS)
         let nrSeeds = getSeeds n board
         
         let rec sowingSeeds n seedsToBeSown holesUpdated startingHole = 
             let n = match n with // Wrap around function. Each time we get to the end of the board we need to go back to 1
                     | 13 -> 1 
                     | _ -> n
             match  seedsToBeSown with
             | 0 -> holesUpdated
             | _ -> match n = startingHole with 
                      |false -> sowingSeeds (n+1) (seedsToBeSown-1) (incrementSeedCount n holesUpdated) startingHole 
                      |_ -> sowingSeeds (n+1) seedsToBeSown holesUpdated startingHole // We need to skip over the startingHole
         
         let (aN,bN,cN,dN,eN,fN,aS,bS,cS,dS,eS,fS) =  sowingSeeds (n+1) nrSeeds holesUpdated n
         //We need to update the board after distribution of seeds.
         let pA = {board.PlayerA with holes = (aN,bN,cN,dN,eN,fN); score = board.PlayerA.score; nrSeeds = (aN+bN+cN+dN+eN+fN)} //Simply sum the contents of each hole
         let pB = {board.PlayerB with holes = (aS,bS,cS,dS,eS,fS); score = board.PlayerB.score; nrSeeds = (aS+bS+cS+dS+eS+fS)} 
         let board = {board with PlayerA = pA; PlayerB = pB; Turn = board.Turn}
        
         //Since we've sown the seeds we now need to update the scoreboard, similar to the way the board was updated.
         let scoreboard = incrementScore endHole board.Turn board 
         let pA = {board.PlayerA with holes = scoreboard.PlayerA.holes; score = scoreboard.PlayerA.score}
         let pB = {board.PlayerB with holes = scoreboard.PlayerB.holes; score = scoreboard.PlayerB.score}
         // The board itself needs to contain the new score  
         let updatedBoard = {board with PlayerA = pA; PlayerB = pB}
         //Pass the turn
         let turn = 
            match board.Turn with
            | North -> South
            | South -> North

         match opponentSeeds board with   //If 0 is returned then an invalid move and we return the original board.
         | 0 -> originalState
         | _ -> { updatedBoard with Turn = turn }

let returnScore board = 
   let NS = board.PlayerA.score |> string
   let SS = board.PlayerB.score |> string
   "North Score: " + NS + "\nSouth Score: " + SS + "\n"

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

//We setup the board over here.
let start position = 
   let hole = (4,4,4,4,4,4)
   let pA = {holes = hole ; score = 0; nrSeeds = 24 }
   let pB = {holes = hole ; score = 0; nrSeeds = 24 }
   {PlayerA = pA; PlayerB = pB; Turn = position}
   // g17e4476: We can play the game. Impure input
let playGame board = 
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
   Game (start South)

[<EntryPoint>]
let main _ =
   // g17e4476: Get's us into the game
    let beginning = playGame (start South)
    0
    
