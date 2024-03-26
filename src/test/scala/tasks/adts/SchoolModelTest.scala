package tasks.adts

import org.junit.Test
import org.junit.Assert.*
import u03.Sequences.Sequence.*
import SchoolModel.*
import SchoolModel.given
import u03.Optionals.Optional.*

class SchoolModelTest:

    val module = summon[SchoolModule]
    import module.*

    val teacherName = "t"
    val t = _teacher(teacherName, Nil())

    val courseName = "c"
    val c = _course(courseName)

    @Test def newSchool(): Unit =
        assertEquals(_school(Nil(), Nil()), school())
    
    @Test def testAddTeacher(): Unit =
        assertEquals(_school(Cons(t, Nil()), Nil()), school().addTeacher(teacherName))

    @Test def testNameOfTeacher(): Unit =
        assertEquals(teacherName, school().nameOfTeacher(t))
    
    @Test def testTeacherByNamePresent(): Unit =
        assertEquals(Just(t), school().addTeacher(teacherName).teacherByName(teacherName))

    @Test def testTeacherByNameNotPresent(): Unit =
        assertEquals(Empty(), school().teacherByName(teacherName))
    
    @Test def testAddCourse(): Unit =
        assertEquals(_school(Nil(), Cons(c, Nil())), school().addCourse(courseName))

    @Test def testNameOfCourse(): Unit =
        assertEquals(courseName, school().nameOfCourse(c))

    @Test def testCourseByNamePresent(): Unit =
        assertEquals(Just(c), school().addCourse(courseName).courseByName(courseName))

    @Test def testCourseByNameNotPresent(): Unit =
        assertEquals(Empty(), school().courseByName(courseName))

    @Test def testCoursesOfATeacher(): Unit =
        assertEquals(Nil(), school().coursesOfATeacher(t))
        assertEquals(Cons(c, Nil()), school().coursesOfATeacher(_teacher("", Cons(c, Nil()))))
