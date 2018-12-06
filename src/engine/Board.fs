namespace RookieOne.Engine




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

    /// go one step up (from white's perspective)
    static member up self =
        { x = self.x; y = self.y + 1}

    /// go one step down (from white's perspective)
    static member down self =
        { x = self.x; y = self.y - 1}

    /// go one step left (from white's perspective)
    static member left self =
        { x = self.x - 1; y = self.y}

    /// go one step right (from white's perspective)
    static member right self =
        { x = self.x + 1; y = self.y}

    /// go one step up and left (from white's perspective)
    static member upLeft self =
        Pos.up >> Pos.left

    /// go one step up and right (from white's perspective)
    static member upRight self =
        Pos.up >> Pos.right

    /// go one step down and left (from white's perspective)
    static member downLeft self =
        Pos.down >> Pos.left

    /// go one step down and right (from white's perspective)
    static member downRight self =
        Pos.down >> Pos.right

    /// move in knight pattern (further left than up)
    static member knightLeftUp self =
        Pos.left >> Pos.upLeft

    /// move in knight pattern (further up than left)
    static member knightUpLeft self =
        Pos.up >> Pos.upLeft

    /// move in knight pattern (further right than up)
    static member knightRightUp self =
        Pos.right >> Pos.upRight

    /// move in knight pattern (further up than right)
    static member knightUpRight self =
        Pos.up >> Pos.upRight

    /// move in knight pattern (further left than down)
    static member knightLeftDown self =
        Pos.left >> Pos.downLeft

    /// move in knight pattern (further down than left)
    static member knightDownLeft self =
        Pos.down >> Pos.downLeft

    /// move in knight pattern (further right than down)
    static member knightRightDown self =
        Pos.right >> Pos.downRight

    /// move in knight pattern (further down than right)
    static member knightDownRight self =
        Pos.down >> Pos.downRight

    /// instance version of Pos.up
    member self.Up() = Pos.up self

    /// instance version of Pos.down
    member self.Down() = Pos.down self

    /// instance version of Pos.left
    member self.Left() = Pos.left self

    /// instance version of Pos.right
    member self.Right() = Pos.right self 

    /// instance version of Pos.upLeft
    member self.UpLeft() = Pos.upLeft self

    /// instance version of Pos.upRight
    member self.UpRight() = Pos.upRight self

    /// instance version of Pos.downLeft
    member self.DownLeft() = Pos.downLeft self

    /// instance version of Pos.downRight
    member self.DownRight() = Pos.downRight self

    /// instance version of Pos.knightLeftUp
    member self.KnightLeftUp() = Pos.knightLeftUp self

    /// instance version of Pos.knightUpLeft
    member self.KnightUpLeft() = Pos.knightUpLeft self

    /// instance version of Pos.knightRightUp
    member self.KnightRightUp() = Pos.knightRightUp self

    /// instance version of Pos.knightUpRight
    member self.KnightUpRight() = Pos.knightUpRight self

    /// instance version of Pos.knightLeftDown
    member self.KnightLeftDown() = Pos.knightLeftDown self

    /// instance version of Pos.knightDownLeft
    member self.KnightDownLeft() = Pos.knightDownLeft self

    /// instance version of Pos.knightRightDown
    member self.KnightRightDown() = Pos.knightRightDown self

    /// instance version of Pos.knightDownRight
    member self.KnightDownRight() = Pos.knightDownRight self