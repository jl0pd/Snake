namespace Snake

open Elmish
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Input
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Components.Hosts
open Avalonia.Threading
open System

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Snake"
        base.Width <- 400.0
        base.Height <- 400.0
        base.MinWidth <- 400.0
        base.MinHeight <- 400.0

        let keyDownHandler _ =
            let sub dispatch =
                this.KeyDown.Add (fun eventArgs ->
                    match eventArgs.Key with
                    | Key.Up -> Game.Message.Up
                    | Key.Down -> Game.Message.Down
                    | Key.Left -> Game.Message.Left
                    | Key.Right -> Game.Message.Right
                    | Key.Space -> Game.Message.Pause
                    | Key.F2 -> Game.Message.NewGame
                    | _ -> Game.Message.Empty
                    |> dispatch)
                |> ignore
            Cmd.ofSub sub

        // this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
        // this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

        let timer (_state: Game.State) =
            let sub dispatch =
                let invoke () =
                    Game.Update |> dispatch
                    true

                DispatcherTimer.Run(Func<_>(invoke), TimeSpan.FromMilliseconds 100.0)
                |> ignore

            Cmd.ofSub sub

        Elmish.Program.mkSimple (fun () -> Game.init ()) Game.update Game.view
        |> Program.withHost this
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.withSubscription timer
        |> Program.withSubscription keyDownHandler
        |> Program.run


type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Load "avares://Avalonia.Themes.Default/DefaultTheme.xaml"
        this.Styles.Load "avares://Avalonia.Themes.Default/Accents/BaseDark.xaml"

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main (args: string []) =
        AppBuilder.Configure<App>().UsePlatformDetect().UseSkia().StartWithClassicDesktopLifetime(args)
