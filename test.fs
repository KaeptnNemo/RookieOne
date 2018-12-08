open RookieOne.Engine

[<EntryPoint>]
let main args =
    let state = State.createDefault
    
    let moves = [
        Move.createReadable White Pawn 'E' 2 'E' 4
        Move.createReadable White Pawn 'E' 4 'E' 5
        Move.createReadable Black Pawn 'D' 7 'D' 5
        Move.createReadable White Pawn 'E' 5 'D' 6
    ]

    let newState = List.fold (State.applyMove false) state moves
    printf  "%O" state
    printfn "#############################"
    printf  "%O" newState
    0
