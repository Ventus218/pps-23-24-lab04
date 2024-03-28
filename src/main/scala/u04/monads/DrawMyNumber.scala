package u04.monads

import u04.monads.Optionals.Optional

object DrawMyNumberGame:
    import u04.monads.States.State
    import u04.monads.Optionals.Optional
    
    trait DrawMyNumberState:
        type DrawMyNumber
        def init(maxN: Int): DrawMyNumber
        def reset(maxN: Int): State[DrawMyNumber, Unit]
        def guess(n: Int): State[DrawMyNumber, Optional[String]]
        def nop(): State[DrawMyNumber, Unit]

    object DrawMyNumberStateImpl extends DrawMyNumberState:
        opaque type DrawMyNumber = Int
        def init(maxN: Int): DrawMyNumber = {val n = scala.util.Random.nextInt(maxN); println(n); n} // TODO: remove debug print
        def reset(maxN: Int): State[DrawMyNumber, Unit] = State(s => (scala.util.Random.nextInt(maxN), {}))
        def guess(n: Int): State[DrawMyNumber, Optional[String]] =
            State(s => (s,
                s match
                    case i if n == i => Optional.Empty()
                    case i if n > i => Optional.Just("Too high")
                    case _ => Optional.Just("Too low")
                )
            )
        def nop(): State[DrawMyNumber, Unit] = State(s => (s, {}))


@main def runDrawMyNumber =
    import Monads.*, Monad.*, States.*, State.*, WindowStateImpl.*, DrawMyNumberGame.*, DrawMyNumberStateImpl.*
    import u03.extensionmethods.Streams.*

    def mv[SM, SV, AM, AV](m1: State[SM,AM], f: AM => State[SV,AV]): State[(SM,SV), AV] = 
        State: (sm, sv) => 
            val (sm2, am) = m1.run(sm)
            val (sv2, av) = f(am).run(sv)
            ((sm2, sv2), av)

    def windowCreation(): State[Window, Stream[String]] = for 
        _ <- setSize(300, 300)
        _ <- addLabel(text = "Inserisci un numero", name = "SuggestionLabel")
        _ <- addTextField(name = "EntryTextField")
        _ <- addButton(text = "Try!", name = "TryButton")
        _ <- addButton(text = "Quit", name = "QuitButton")
        _ <- show()
        events <- eventStream()
    yield events

    val controller = for
        events <- mv(nop(), i => windowCreation())
        _ <- seqN(events.map(_ match
            case "TryButton" =>
                for
                    text <- mv(nop(), _ => getTextFieldText("EntryTextField"))
                    res <- mv(guess(text.toInt), sugg => sugg match
                        case Optional.Just(s) => toLabel(s, "SuggestionLabel")
                        case _ => toLabel("Correct", "SuggestionLabel"))
                yield {}
            case "QuitButton" => mv(nop(), _ => exec(sys.exit()))))
    yield ()

    controller.run((init(100), initialWindow))
