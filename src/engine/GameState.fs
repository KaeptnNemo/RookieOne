namespace RookieOne.Engine



/// piece is moved from source to destination
type Move = { Piece: Piece; Src: Pos; Dst: Pos }
with
    static member create p s d = { Piece = p; Src = s; Dst = d }

    static member toString self =
        sprintf "%O: %O -> %O" self.Piece.Type self.Src self.Dst

    override self.ToString() = Move.toString self

/// state of a game (all pieces and positions)
type State = {
    // how many moves have been played?
    MoveCount: int

    // the actial board
    Board: Piece Option array

    // lists of removed pieces
    RemovedWhite: Piece list
    RemovedBlack: Piece list
    
    // can king and rook still switch positions?
    BlackKingMoved:  bool
    WhiteKingMoved:  bool
    BlackRookLMoved: bool
    WhiteRookLMoved: bool
    BlackRookRMoved: bool
    WhiteRookRMoved: bool
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
                    let f = self.Board.[State.fancyArrayAddr x y]
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
        MoveCount = 0
        Board = State.createDefaultArray()
        RemovedBlack = []
        RemovedWhite = []
        BlackKingMoved  = false
        WhiteKingMoved  = false
        BlackRookLMoved = false
        WhiteRookLMoved = false
        BlackRookRMoved = false
        WhiteRookRMoved = false
    }

    static member applyMove self move =
        if (move.Src |> Pos.isValid |> not) || (move.Dst |> Pos.isValid |> not) then
            failwith (sprintf "Position out of bounds: %O" move)
        let srcAddr = Pos.toAddr move.Src
        let dstAddr = Pos.toAddr move.Dst
        match self.Board.[srcAddr] with
        | None -> failwith (sprintf "No piece to be moved here: %O" move)
        | Some p when (p <> move.Piece) -> failwith (sprintf "Wrong piece found: %O" move)
        | _ -> ()

        let board = Array.copy self.Board
        let dstPiece = self.Board.[dstAddr]
        board.[dstAddr] <- Some move.Piece
        board.[srcAddr] <- None

        // prepend captured piece to list
        let capture p =
            match p.Color with
            | Black -> (p :: self.RemovedBlack, self.RemovedWhite)
            | White -> (self.RemovedBlack, p :: self.RemovedWhite)

        // check whether a piece needs to be captured
        let (rmB,rmW) = // new lists of removed pieces
            match dstPiece with
            // dst was empty -> check for en-passent
            | None ->
                let backAddr =
                    move.Dst |> Board.backward move.Piece.Color |> Seq.head |> Pos.toAddr
                if move.Piece.Type = PieceType.Pawn && board.[backAddr] <> None then
                    // en-passent -> capture piece at backAddr
                    let p = board.[backAddr] |> Option.get
                    board.[backAddr] <- None
                    capture p
                else
                    (self.RemovedBlack, self.RemovedWhite)
            // dst was not empty -> 
            | Some p -> capture p
                

        let br = Piece.create Black Rook
        let wr = Piece.create White Rook
        let topLeft  = Pos.fromReadable 'A' 8
        let topRight = Pos.fromReadable 'H' 8
        let botLeft  = Pos.fromReadable 'A' 1
        let botRight = Pos.fromReadable 'H' 1
        {
            MoveCount = self.MoveCount + 1
            Board = board
            RemovedBlack = rmB
            RemovedWhite = rmW
            BlackKingMoved  = self.BlackKingMoved  || move.Piece = (Piece.create Black King)
            WhiteKingMoved  = self.WhiteKingMoved  || move.Piece = (Piece.create White King)
            BlackRookLMoved = self.BlackRookLMoved || (move.Piece = br && move.Src = topRight)
            WhiteRookLMoved = self.WhiteRookLMoved || (move.Piece = wr && move.Src = botLeft)
            BlackRookRMoved = self.BlackRookRMoved || (move.Piece = br && move.Src = topLeft)
            WhiteRookRMoved = self.WhiteRookRMoved || (move.Piece = wr && move.Src = botRight)
        }




