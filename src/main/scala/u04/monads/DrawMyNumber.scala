package u04.monads

import u04.monads.Optionals.Optional

object DrawMyNumberGame:
    import u04.monads.States.State
    import u04.monads.Optionals.Optional
    
    trait DrawMyNumberState:
        type DrawMyNumber
        def init(maxN: Int, attempts: Int): DrawMyNumber
        def reset(): State[DrawMyNumber, Unit]
        /// Returns an empty Optional if the guess is correct otherwise a suggestion String
        def guess(n: Int): State[DrawMyNumber, Optional[String]]
        def attemptsLeft(): State[DrawMyNumber, Int]
        def nop(): State[DrawMyNumber, Unit]

    object DrawMyNumberStateImpl extends DrawMyNumberState:
        opaque type DrawMyNumber = DrawMyNumberImpl
        private case class DrawMyNumberImpl(n: Int, config: Configuration, attemptsLeft: Int)
        private case class Configuration(attempts: Int, maxN: Int)
        def init(maxN: Int, attempts: Int): DrawMyNumber = 
            val n = scala.util.Random.nextInt(maxN)
            println(n) // For testing
            DrawMyNumberImpl(n, Configuration(attempts, maxN), attempts)
        def reset(): State[DrawMyNumber, Unit] = State(s => (init(s.config.maxN, s.config.attempts), {}))
        def guess(n: Int): State[DrawMyNumber, Optional[String]] =
            State(s => 
                val newState = DrawMyNumberImpl(s.n, s.config, Math.max(s.attemptsLeft - 1, 0))
                (newState, newState match
                    case DrawMyNumberImpl(_, _, left) if left <= 0 => Optional.Just("You lost")
                    case DrawMyNumberImpl(i, _, _) if n == i => Optional.Empty()
                    case DrawMyNumberImpl(i, _, left) if n > i => Optional.Just(s"Too high.  $left attempts left")
                    case DrawMyNumberImpl(_, _, left) => Optional.Just(s"Too low.  $left attempts left")
                )
            )
        def attemptsLeft(): State[DrawMyNumber, Int] = State(s => (s, s.attemptsLeft))
        def nop(): State[DrawMyNumber, Unit] = State(s => (s, {}))


@main def runDrawMyNumber =
    import Monads.*, Monad.*, States.*, State.*, WindowStateImpl.*, DrawMyNumberGame.*, DrawMyNumberStateImpl.*
    import u03.extensionmethods.Streams.*

    def mv[SM, SV, AM, AV](m1: State[SM,AM], f: AM => State[SV,AV]): State[(SM,SV), AV] = 
        State: (sm, sv) => 
            val (sm2, am) = m1.run(sm)
            val (sv2, av) = f(am).run(sv)
            ((sm2, sv2), av)

    def windowCreation(attemptsLeft: Int): State[Window, Stream[String]] = for 
        _ <- setSize(300, 300)
        _ <- addLabel(text = s"$attemptsLeft attempts left", name = "SuggestionLabel")
        _ <- addTextField(name = "EntryTextField")
        _ <- addButton(text = "Try!", name = "TryButton")
        _ <- addButton(text = "Reset", name = "ResetButton")
        _ <- addButton(text = "Quit", name = "QuitButton")
        _ <- show()
        events <- eventStream()
    yield events

    val controller = for
        events <- mv(attemptsLeft(), i => windowCreation(i))
        _ <- seqN(events.map(_ match
            case "TryButton" =>
                for
                    text <- mv(nop(), _ => getTextFieldText("EntryTextField"))
                    res <- mv(guess(text.toInt), sugg => sugg match
                        case Optional.Just(s) => toLabel(s, "SuggestionLabel")
                        case _ => toLabel("Correct", "SuggestionLabel"))
                yield {}
            case "ResetButton" => mv(seq(reset(), attemptsLeft()), left => toLabel(s"$left attempts left", "SuggestionLabel"))
            case "QuitButton" => mv(nop(), _ => exec(sys.exit()))))
    yield ()

    controller.run((init(100, 5), initialWindow))
