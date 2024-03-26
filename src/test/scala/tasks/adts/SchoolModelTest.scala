package tasks.adts

import org.junit.Test
import org.junit.Assert.*
import u03.Sequences.Sequence.*
import SchoolModel.*
import SchoolModel.given
import SchoolModuleImpl._school
import SchoolModuleImpl._teacher
import SchoolModuleImpl._course

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
        val c = school().nameOfTeacher(t)
        assertEquals(_school(Cons(t, Nil()), Nil()), school().addTeacher(teacherName))
