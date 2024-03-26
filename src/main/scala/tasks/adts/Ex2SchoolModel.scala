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
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(teacher: Teacher): String
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

    extension (school: School) 
      override def addCourse(name: String): School =
        SchoolImpl(school.teachers, Cons(CourseImpl(name), school.courses))

      override def nameOfTeacher(teacher: Teacher): String = ???

      override def setTeacherToCourse(teacher: Teacher, course: Course): School = ???

      override def addTeacher(name: String): School =
        SchoolImpl(Cons(TeacherImpl(name, Nil()), school.teachers), school.courses)

      override def teacherByName(name: String): Optional[Teacher] = ???

      override def courseByName(name: String): Optional[Course] = ???

      override def nameOfCourse(teacher: Teacher): String = ???

      override def coursesOfATeacher(teacher: Teacher): Sequence[Course] = ???
