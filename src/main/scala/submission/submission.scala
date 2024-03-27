package submission

import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Optionals.*
import u03.Optionals.Optional.*
import u04.monads.Monads.Monad


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



object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    def school(): School
    private[submission] def _school(teachers: Sequence[Teacher], courses: Sequence[Course]): School
    private[submission] def _course(name: String): Course
    private[submission] def _teacher(name: String, courses: Sequence[Course]): Teacher
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      // I did not get why making this an extension method as it doesn't need
      // a school parameter. (Same goes for nameOfCourse and coursesOfATeacher methods)
      def nameOfTeacher(teacher: Teacher): String
      // nameOfCourse had a Teacher as parameter but i think was a copy-paste error so i changed it to Course.
      // The reason is that a teacher may have no courses and the result should be optional
      def nameOfCourse(course: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]
  
  given SchoolModule = SchoolModuleImpl

  object SchoolModuleImpl extends SchoolModule:
    opaque type Course = CourseImpl
    private case class CourseImpl(name: String)

    opaque type Teacher = TeacherImpl
    private case class TeacherImpl(name: String, courses: Sequence[Course])

    opaque type School = SchoolImpl
    private case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])

    def school(): School = SchoolImpl(Nil(), Nil())
    
    // QUESTION:
    // Is this a good way of allowing proper testing while keeping
    // case classes private?
    private[submission] def _school(teachers: Sequence[Teacher], courses: Sequence[Course]): School = SchoolImpl(teachers, courses)
    private[submission] def _course(name: String): Course = CourseImpl(name)
    private[submission] def _teacher(name: String, courses: Sequence[Course]): Teacher = TeacherImpl(name, courses)

    /// Adds a Course to a Sequence[Course] only if not already present
    extension (s: Sequence[Course]) private def addUniqueCourse(c: Course): Sequence[Course] = s match
      case Cons(h, t) => if h != c then t.addUniqueCourse(c) else s
      case Nil() => Cons(c, s)

    /// Adds a Teacher to a Sequence[Teacher] only if not already present
    extension (s: Sequence[Teacher]) private def addUniqueTeacher(teacher: Teacher): Sequence[Teacher] = s match
      case Cons(h, t) => if h != teacher then t.addUniqueTeacher(teacher) else s
      case Nil() => Cons(teacher, s)

    extension (school: School) 
      override def addCourse(name: String): School =
        SchoolImpl(school.teachers, school.courses.addUniqueCourse(CourseImpl(name)))

      override def nameOfTeacher(teacher: Teacher): String =
        teacher.name

      override def setTeacherToCourse(teacher: Teacher, course: Course): School =
        val schoolWithCourse = SchoolImpl(school.teachers, school.courses.addUniqueCourse(course))
        val teacherWithCourse = TeacherImpl(teacher.name, teacher.courses.addUniqueCourse(course))
        SchoolImpl(schoolWithCourse.teachers.addUniqueTeacher(teacherWithCourse), schoolWithCourse.courses)

      override def addTeacher(name: String): School =
        SchoolImpl(school.teachers.addUniqueTeacher(TeacherImpl(name, Nil())), school.courses)

      override def teacherByName(name: String): Optional[Teacher] = school match
        case SchoolImpl(Cons(h, t), _) => if h.name == name then Optional.Just(h) else teacherByName(name)
        case _ => Optional.Empty()

      override def courseByName(name: String): Optional[Course] = school match
        case SchoolImpl(_, Cons(h, t)) => if h.name == name then Optional.Just(h) else courseByName(name)
        case _ => Optional.Empty()

      override def nameOfCourse(course: Course): String = 
        course.name

      override def coursesOfATeacher(teacher: Teacher): Sequence[Course] = 
        teacher.courses



object Ex3Stacks:

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] // factory
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]
  
  object StackImpl extends StackADT:
    opaque type Stack[A] = Sequence[A]
    def empty[A]: Stack[A] = Nil()
    extension [A](stack: Stack[A])

      def push(a: A): Stack[A] = Cons(a, stack)

      def pop(): Optional[(A, Stack[A])] = stack match
        case Cons(h, t) => Just((h, t))
        case _ => Empty()

      def asSequence(): Sequence[A] = stack



object Ex4Summables:

  def sumAllInt(seq: Sequence[Int]): Int = seq match
    case Cons(h, t) => h + sumAllInt(t)
    case _ => 0

  trait Summable[A]:
    def sum(a1: A, a2: A): A
    def zero: A

  def sumAll[A: Summable](seq: Sequence[A]): A =
    val summable = summon[Summable[A]]
    @annotation.tailrec
    def _sumAll(seq: Sequence[A], acc: A): A = seq match
      case Cons(h, t) => _sumAll(t, summable.sum(acc, h))
      case _ => acc
    _sumAll(seq, summable.zero)

  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0

  given Summable[Double] with
    def sum(a1: Double, a2: Double): Double = a1 + a2
    override def zero: Double = 0

  given Summable[String] with
    def sum(a1: String, a2: String): String = a1 concat a2
    override def zero: String = ""



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



object Ex6TryModel:
  private enum TryImpl[A]:
    case Success(value: A)
    case Failure(exception: Throwable)

  opaque type Try[A] = TryImpl[A]

  def success[A](value: A): Try[A] = TryImpl.Success(value)
  def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)
  def exec[A](expression: => A): Try[A] = try success(expression) catch failure(_)

  extension [A](m: Try[A]) 
    def getOrElse[B >: A](other: B): B = m match
      case TryImpl.Success(value) => value
      case TryImpl.Failure(_) => other

  given Monad[Try] with
    override def unit[A](value: A): Try[A] = success(value)

    extension [A](m: Try[A])
      override def flatMap[B](f: A => Try[B]): Try[B] = m match
        case TryImpl.Success(value) => f(value)
        case TryImpl.Failure(e) => TryImpl.Failure(e)