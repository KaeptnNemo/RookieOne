open RookieOne.Engine

[<EntryPoint>]
let main args =
    let state = State.createDefault
    
    let move = Move.createReadable White Pawn 'E' 2 'E' 4

    let newState = State.applyMove state move
    printf  "%O" state
    printfn "#############################"
    printf  "%O" newState
    0
