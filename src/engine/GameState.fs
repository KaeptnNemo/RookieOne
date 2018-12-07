namespace RookieOne.Engine




/// piece is moved from source to destination
type Move = { piece: Piece; src: Pos; dst: Pos }

/// state of a game (all pieces and positions)
type State = {
    // how many moves have been played?
    moveCount: int

    // the actial board
    board: Piece Option array

    // lists of removed pieces
    removedWhite: Piece list
    removedBlack: Piece list
    
    // can king and rook still switch positions?
    blackKingMoved:  bool
    whiteKingMoved:  bool
    blackRookLMoved: bool
    whiteRookLMoved: bool
    blackRookRMoved: bool
    whiteRookRMoved: bool
}
with    

    /// take character and number in reachable format and give address into array
    static member fancyArrayAddr c y =
        (Pos.fromReadable c y) |> Pos.toAddr

    /// format a game state as string
    static member toString self =
        seq {
            // top row A - H
            yield "  "
            for x = 'A' to 'H' do yield (sprintf " %c " x)
            yield "\n  "
            for i in 0 .. 7 do yield "+--"
            yield "+\n"

            // actual board
            for y = 8 downto 1 do
                yield (sprintf "%i " y)
                for x = 'A' to 'H' do
                    let f = self.board.[State.fancyArrayAddr x y]
                    yield
                        match f with
                        | None   -> "|  "
                        | Some p -> "|" + p.ToString()
                yield (sprintf "| %i\n  " y)
                for i in 0 .. 7 do yield "+--"
                yield "+\n"

            // bottom row A -H
            yield "  "
            for x = 'A' to 'H' do yield (sprintf " %c " x)
            yield "\n"
        }
        |> Seq.fold (+) ""

    /// instance version of State.toString
    override self.ToString() = State.toString self

    // helper function to create an array for the default state
    static member private createDefaultArray() =
        let board = Array.create<Option<Piece>> 64 None
        let placePiece c y color pieceType =
            board.[State.fancyArrayAddr c y]
                <- Some (Piece.create color pieceType)
        
        placePiece 'A' 1 White Rook
        placePiece 'B' 1 White Knight
        placePiece 'C' 1 White Bishop
        placePiece 'D' 1 White Queen
        placePiece 'E' 1 White King
        placePiece 'F' 1 White Bishop
        placePiece 'G' 1 White Knight
        placePiece 'H' 1 White Rook

        placePiece 'A' 8 Black Rook
        placePiece 'B' 8 Black Knight
        placePiece 'C' 8 Black Bishop
        placePiece 'D' 8 Black Queen
        placePiece 'E' 8 Black King
        placePiece 'F' 8 Black Bishop
        placePiece 'G' 8 Black Knight
        placePiece 'H' 8 Black Rook

        for i in 'A' .. 'H' do
            placePiece i 2 White Pawn
            placePiece i 7 Black Pawn

        board

    /// the default game state
    static member createDefault = {
        moveCount = 0
        board = State.createDefaultArray()
        removedBlack = []
        removedWhite = []
        blackKingMoved = false
        whiteKingMoved = false
        blackRookLMoved = false
        whiteRookLMoved = false
        blackRookRMoved = false
        whiteRookRMoved = false
    }
