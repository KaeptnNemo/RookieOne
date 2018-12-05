module RookieOne.Engine




/// the color of a piece (either black or white)
type Color = | Black | White
with

    /// get the opposite color
    static member other self = 
        match self with
        | Black -> White
        | White -> Black

    /// instance version of Color.other
    member this.Other() = Color.other this

    /// format a color as string
    static member toString self =
        match self with
        | Black -> "b"
        | White -> "w"

    /// instance version of Color.toString (used by printf)
    override self.ToString() = Color.toString self

    /// parse a color from a string (case insensitive) ("w","white","b","black")
    static member fromString (s: string) =
        match s.ToLower() with
        | "b" | "black" -> Black
        | "w" | "white" -> White
        | _ -> failwith "given string was not a valid color"


// ---------------------------------------------------------------------------------------------- //


/// stores the type of piece
type PieceType = 
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King
with

    /// format the piece type as a string
    static member toString self =
        match self with
        | Pawn   -> "P"
        | Knight -> "K"
        | Bishop -> "B"
        | Rook   -> "R"
        | Queen  -> "Q"
        | King   -> "K"

    /// instance version of PieceType.toString (used by printf)
    override self.ToString() = PieceType.toString self

    /// map a type of piece to it's value
    static member value self =
        match self with
        | Pawn   -> 1
        | Knight -> 3
        | Bishop -> 3
        | Rook   -> 5
        | Queen  -> 9
        | King   -> 0

    /// instance version of PieceType.value
    member self.Value() = PieceType.value self

    /// parse a piece type from a string (singe character or full name) (case insensitive)
    static member fromString (s: string) =
        match s.ToLower() with
        | "p" | "pawn"   -> Pawn
        | "k" | "knight" -> Knight
        | "b" | "bishop" -> Bishop
        | "r" | "rook"   -> Rook
        | "q" | "queen"  -> Queen
        | "k" | "king"   -> King
        | _ -> failwith "given string was not a valid piece type"


// ---------------------------------------------------------------------------------------------- //


/// a pice has a color and a type
type Piece = { Color: Color; Type: PieceType}
with

    /// constructor
    static member create c t =
        { Color = c; Type = t }

    /// format a piece as string
    static member toString self =
        self.Color.ToString() + self.Type.ToString()

    /// instance version of Piece.toString (used by printf)
    override self.ToString() = Piece.toString self