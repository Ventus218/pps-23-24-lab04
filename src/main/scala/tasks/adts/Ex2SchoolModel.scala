package tasks.adts
import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Optionals.*
import u02.AlgebraicDataTypes.Person

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    def school(): School
    private[adts] def _school(teachers: Sequence[Teacher], courses: Sequence[Course]): School
    private[adts] def _course(name: String): Course
    private[adts] def _teacher(name: String, courses: Sequence[Course]): Teacher
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
    private[adts] def _school(teachers: Sequence[Teacher], courses: Sequence[Course]): School = SchoolImpl(teachers, courses)
    private[adts] def _course(name: String): Course = CourseImpl(name)
    private[adts] def _teacher(name: String, courses: Sequence[Course]): Teacher = TeacherImpl(name, courses)

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
        SchoolImpl(school.teachers, Cons(CourseImpl(name), school.courses))

      override def nameOfTeacher(teacher: Teacher): String =
        teacher.name

      override def setTeacherToCourse(teacher: Teacher, course: Course): School =
        val schoolWithCourse = SchoolImpl(school.teachers, school.courses.addUniqueCourse(course))
        val teacherWithCourse = TeacherImpl(teacher.name, teacher.courses.addUniqueCourse(course))
        SchoolImpl(schoolWithCourse.teachers.addUniqueTeacher(teacherWithCourse), schoolWithCourse.courses)

      override def addTeacher(name: String): School =
        SchoolImpl(Cons(TeacherImpl(name, Nil()), school.teachers), school.courses)

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
