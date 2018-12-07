open RookieOne.Engine

[<EntryPoint>]
let main args =
    let state = State.createDefault
    
    let move = Move.create (Piece.create White Pawn) (Pos.fromReadable 'E' 2) (Pos.fromReadable 'E' 4)

    let newState = State.applyMove state move
    printf  "%O" state
    printfn "#############################"
    printf  "%O" newState
    0
