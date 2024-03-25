package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.Ex3Stacks.StackImpl
import u03.Sequences.Sequence
import u03.Optionals.Optional

/* Tests should be clear, but note they are expressed independently of the 
   specific implementation
*/

class Stacktest:


  val stack = StackImpl

  import stack.*

  @Test def testAsSequence() =
    assertEquals(Sequence.Cons(3, Sequence.Cons(2, Sequence.Nil())), empty[Int].push(2).push(3))

  @Test def testEmpty() =
    assertEquals(Sequence.Nil(), empty[Int].asSequence())

  @Test def testPush() =
    assertEquals(Sequence.Cons(10, Sequence.Nil()), empty[Int].push(10).asSequence())

  @Test def testPopOnEmpty() =
    assertEquals(Optional.Empty(), empty[Int].pop())

  @Test def testPopOnNotEmpty() =
    assertEquals(Optional.Just((10, Sequence.Nil())), empty[Int].push(10).pop())

  @Test def testMultiplePops() =
    val stack = empty[Int].push(1).push(2)

    val stack2 = stack.pop() match
      case Optional.Just((value, stack)) =>
        assertEquals(2, value)
        stack
      case _ =>
        fail("Expected 2 got empty stack")
        stack

    val _ = stack2.pop() match
      case Optional.Just((value, stack)) =>
        assertEquals(1, value)
        stack
      case _ => fail("Expected 1 got empty stack")

    /*val (pop2, _) = stack.pop()
    val (pop1, _) = stack.pop()
    assertEquals(Optional.Just(2), pop2)
    assertEquals(Optional.Just(1), pop1)*/
    