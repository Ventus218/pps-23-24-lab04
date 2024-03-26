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
    // QUESTION: 
    // I suppose that making this case class (and also Teacher and School) public
    // is wrong because it would mean showing implementations details.
    // But without doing it i don't know how to properly test this module.
    // What should i do?
    case class CourseImpl(name: String)

    opaque type Teacher = TeacherImpl
    case class TeacherImpl(name: String, courses: Sequence[Course])

    opaque type School = SchoolImpl
    case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])

    def school(): School = SchoolImpl(Nil(), Nil())

    extension (school: School) 
      override def addCourse(name: String): School = ???

      override def nameOfTeacher(teacher: Teacher): String = ???

      override def setTeacherToCourse(teacher: Teacher, course: Course): School = ???

      override def addTeacher(name: String): School = ???

      override def teacherByName(name: String): Optional[Teacher] = ???

      override def courseByName(name: String): Optional[Course] = ???

      override def nameOfCourse(teacher: Teacher): String = ???

      override def coursesOfATeacher(teacher: Teacher): Sequence[Course] = ???
