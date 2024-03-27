package u04lab
import u03.Sequences.* 
import Sequence.*
import u03.Optionals.Optional

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
    extension [A](t: T[A]) def logAll(using f: A => Unit): Unit
    
  def log[A](a: A): Unit = println("The next element is: "+a)
  given (Any => Unit) = log

  object TraversableSequence extends Traversable[Sequence]:
    extension [A](t: Sequence[A]) override def logAll(using f: A => Unit): Unit = t match
      case Cons(h, t) => f(h); t.logAll
      case _ => ()

  object TraversableOptional extends Traversable[Optional]:
    extension [A](t: Optional[A]) override def logAll(using f: A => Unit): Unit = t match
      case Optional.Just(a) => f(a)
      case _ => ()

  @main def tryTraversables =
    import TraversableSequence.logAll
    // QUESTION:
    // If both modules are imported then i can't use logAll because the compiler says it's ambiguous
    // Why isn't the compiler able to infer which logAll to use based on the type of its parameters?

    // import TraversableOptional.logAll
    // Optional.Just(100).logAll

    Cons(10, Cons(20, Nil())).logAll
    TraversableOptional.logAll(Optional.Just(100))
    
    Cons(10, Cons(20, Nil())).logAll(using println(_)) // Overriding the given
