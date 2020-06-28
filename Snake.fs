namespace Snake

open System.Linq
open System
open Avalonia.FuncUI.DSL
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.Helpers


[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
    let skipLast count (s: 'a list) =
        Enumerable.SkipLast(s, count) |> List.ofSeq

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Array2D =
    let flat (arr: 'a [,]): 'a [] =
        [| for x in 0 .. (Array2D.length1 arr) - 1 do
            for y in 0 .. (Array2D.length2 arr) - 1 do
                yield arr.[x, y] |]


module Snake =
    type CellType =
        | Empty
        | Snake
        | Apple
        | Wall

    [<Struct>]
    type Cell =
        { X: int
          Y: int
          Type: CellType }
        static member Empty = { X = 0; Y = 0; Type = Empty }

    type Direction =
        | Left
        | Right
        | Up
        | Down

    type Snake =
        { Direction: Direction
          Cells: Cell list
          MustGrow: bool }

    let updateDirection direction snake =
        let newDirection =
            match snake.Direction with
            | Left when direction <> Right -> direction
            | Right when direction <> Left -> direction
            | Up when direction <> Down -> direction
            | Down when direction <> Up -> direction
            | _ -> snake.Direction

        { snake with Direction = newDirection }

    let tryGetHead { Cells = cells } = List.tryHead cells

    let getNewHead ({ X = x; Y = y } as cell) direction =
        let newX, newY =
            match direction with
            | Left -> x - 1, y
            | Right -> x + 1, y
            | Up -> x, y + 1
            | Down -> x, y - 1

        { cell with X = newX; Y = newY }

    let move snake =
        let oldHead = tryGetHead snake
        match oldHead with
        | None -> Error "No head"
        | Some old ->
            let newHead = getNewHead old snake.Direction

            let newCells =
                newHead
                :: (if snake.MustGrow then snake.Cells else List.skipLast 1 snake.Cells)

            Ok
                { snake with
                      Cells = newCells
                      MustGrow = false }

    let isHitSelf snake =
        match tryGetHead snake with
        | Some head ->
            snake.Cells
            |> List.skip 1
            |> List.exists (fun c -> c = head)
        | None -> false

    let grow snake = { snake with MustGrow = true }

    let isHitEdge (w, h) snake =
        match tryGetHead snake with
        | Some { X = x; Y = y } ->
            match snake.Direction with
            | Left -> x = 0
            | Right -> x = w - 1
            | Up -> y = h - 1
            | Down -> y = 0
        | None -> false

    let isHit snake { X = x; Y = y } =
        match tryGetHead snake with
        | Some { X = hx; Y = hy } -> x = hx && hy = y
        | _ -> false

module Game =
    open Snake

    type Board = { Size: int * int }

    type State =
        { Snake: Snake
          Apple: Cell
          Score: int
          Board: Board
          IsOver: bool
          IsPaused: bool
          StartTime: DateTime }

    let init () =
        { Snake =
              { Direction = Right
                MustGrow = false
                Cells =
                    [ { X = 5; Y = 10; Type = Snake }
                      { X = 4; Y = 10; Type = Snake }
                      { X = 3; Y = 10; Type = Snake } ] }
          Apple = { X = 20; Y = 10; Type = Apple }
          Score = 0
          Board = { Size = (32, 24) }
          IsOver = false
          IsPaused = false
          StartTime = DateTime.Now }

    type Message =
        | Left
        | Right
        | Up
        | Down
        | Update
        | Pause
        | NewGame
        | Empty
        member t.ToDirection =
            match t with
            | Left -> Some Direction.Left
            | Right -> Some Direction.Right
            | Up -> Some Direction.Down
            | Down -> Some Direction.Up // that's alright
            | _ -> None

    let rand = Random()

    let newApple board =
        let w, h = board.Size
        let x = rand.Next(1, w - 1)
        let y = rand.Next(1, h - 1)
        { X = x; Y = y; Type = Apple }

    let update msg state =
        match msg with
        | Update when not state.IsPaused && not state.IsOver ->
            match move state.Snake with
            | Error _ -> state
            | Ok snake ->
                if (isHitEdge state.Board.Size snake
                    || isHitSelf snake) then
                    { state with IsOver = true }
                elif isHit snake state.Apple then
                    { state with
                          Snake = grow snake
                          Apple = newApple state.Board
                          Score = state.Score + 1 }
                else
                    { state with Snake = snake }
        | NewGame -> init ()
        | Pause ->
            let isPaused = not state.IsPaused
            { state with IsPaused = isPaused }
        | Left
        | Right
        | Up
        | Down ->
            { state with
                  Snake = updateDirection msg.ToDirection.Value state.Snake }
        | _ -> state

    let private set (ar: Cell [,]) ({ X = x; Y = y } as cell) = ar.[y, x] <- cell

    let private createFilledBoard state =
        let w, h = state.Board.Size

        let ar =
            Array2D.init h w (fun x y ->
                let isBorder = x = 0 || x = h - 1 || y = 0 || y = w - 1

                let t =
                    if isBorder then Wall else CellType.Empty

                { X = x; Y = y; Type = t })

        for cell in state.Snake.Cells do
            set ar cell
        set ar state.Apple
        ar

    let boardView state dispatch =
        let w, h = state.Board.Size
        UniformGrid.create
            [ UniformGrid.columns w
              UniformGrid.rows h
              UniformGrid.children
                  (createFilledBoard state
                   |> Array2D.flat
                   |> Array.map (fun { Type = t } ->
                       let color =
                           match t with
                           | Apple -> "#FF0000"
                           | CellType.Empty -> "#000000"
                           | Wall -> "#FFFFFF"
                           | Snake -> "#00FF00"

                       let reduceBrightness (color: string) =
                           let substract value from = max 0.0 (from - value)
                           color.[1..]
                           |> Color.RGB.OfString
                           |> Option.get
                           |> fun rgb -> rgb.AsTuple
                           |> Tuple3.mapEach (substract 120.0)
                           |> Color.RGB.OfTuple
                           |> Color.toColorString

                       Border.create
                           [ Border.padding 3.
                             Border.background (reduceBrightness color)
                             Border.child (Border.create [ Border.background color ]) ]

                       |> generalize)
                   |> Array.toList) ]

    let menuView state dispatch =
        StackPanel.create
            [ StackPanel.orientation Orientation.Horizontal
              StackPanel.dock Dock.Top

              StackPanel.children
                  [ TextBlock.create
                      [ TextBlock.fontSize 16.
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        TextBlock.width 80.
                        TextBlock.text (sprintf "Score: %d" state.Score) ]
                    TextBlock.create
                        [ TextBlock.fontSize 16.
                          TextBlock.width 80.
                          TextBlock.horizontalAlignment HorizontalAlignment.Center
                          TextBlock.text (sprintf "%04.0fs" (DateTime.Now - state.StartTime).TotalSeconds) ] ] ]

    let gameOverView state dispatch =
        StackPanel.create
            [ StackPanel.orientation Orientation.Vertical
              StackPanel.horizontalAlignment HorizontalAlignment.Center
              StackPanel.verticalAlignment VerticalAlignment.Center
              StackPanel.children
                  [ TextBlock.create
                      [ TextBlock.fontSize 16.
                        TextBlock.margin 4.
                        TextBlock.text "Game Over" ]
                    TextBlock.create
                        [ TextBlock.fontSize 16.
                          TextBlock.margin 4.
                          TextBlock.text (sprintf "Score: %d" state.Score) ]
                    Button.create
                        [ Button.fontSize 16.
                          Button.margin 4.
                          Button.onClick (fun _ -> dispatch NewGame)
                          Button.content "New game" ] ] ]

    let view state dispatch =
        if state.IsOver then
            gameOverView state dispatch |> generalize
        else
            DockPanel.create
                [ DockPanel.lastChildFill true

                  DockPanel.children
                      [ menuView state dispatch
                        boardView state dispatch ] ]
            |> generalize
