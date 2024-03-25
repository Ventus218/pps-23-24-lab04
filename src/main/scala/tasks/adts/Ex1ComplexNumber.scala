package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:
    private case class ComplexImpl(re: Double, im: Double)
    opaque type Complex = ComplexImpl

    def complex(re: Double, im: Double): Complex = ComplexImpl(re, im)
    
    extension (complex: Complex)
      def re(): Double = complex match
        case ComplexImpl(re, _) => re
      def im(): Double = complex match
        case ComplexImpl(_, im) => im
      def sum(other: Complex): Complex = (complex, other) match
        case (ComplexImpl(re1, im1), ComplexImpl(re2, im2)) => ComplexImpl(re1 + re2, im1 + im2)
      def subtract(other: Complex): Complex = (complex, other) match
        case (ComplexImpl(re1, im1), ComplexImpl(re2, im2)) => ComplexImpl(re1 - re2, im1 - im2)
      def asString(): String = complex match
        case ComplexImpl(re, 0) => s"$re"
        case ComplexImpl(0, im) => s"${im}i"
        case ComplexImpl(re, im) => s"$re ${im.signString()} ${Math.abs(im)}i"

    extension (d: Double) private def signString(): String =
      if d >= 0 then "+" else "-"
