module RookieOne.Board




/// position in a board
type Pos = { x: int; y: int}
with

    /// to tuple of integers (0-7 each)
    static member toInteger self =
        (self.x, self.y)

    /// instance version of Pos.toInteger
    member self.ToInteger() = Pos.toInteger self

    /// to tuple of char and interger ('A'-'H',1-8)
    static member toReadable self =
        let rx = self.x |> (+) (int 'A') |> char
        let ry = self.y + 1
        (rx,ry)

    /// instance version of Pos.toReadable
    member self.ToReadable() = Pos.toReadable self

    /// format as a readable string
    static member toString self =
        let (rx,ry) = Pos.toReadable self
        sprintf "(%c,%i)" rx ry

    /// instance version of Pos.toString (used by printf)
    override self.ToString() = Pos.toString self

    /// construct pos from two integers
    static member fromInteger x y = { x = x; y = y }

    /// construct pos from readable character and integer
    static member fromReadable c y =
        let x = c |> System.Char.ToLower |> int |> (-) (int 'a')
        { x = x; y = y-1 }

    /// check whether a pos is within the boards bounds
    static member isValid self =
        self.x >= 0 && self.x < 8 && self.y >= 0 && self.y < 8

    /// instance version of Pos.isValid
    member self.IsValid() = Pos.isValid self
