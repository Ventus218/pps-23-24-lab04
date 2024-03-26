package tasks.adts

import org.junit.Test
import org.junit.Assert.*
import u03.Sequences.Sequence.*
import SchoolModel.*
import SchoolModel.given

class SchoolModelTest:

    val module = summon[SchoolModule]
    import module.*

    val teacherName = "t"
    val t = _teacher(teacherName, Nil())

    @Test def newSchool(): Unit =
        assertEquals(_school(Nil(), Nil()), school())
    
    @Test def testAddTeacher(): Unit =
        assertEquals(_school(Cons(t, Nil()), Nil()), school().addTeacher(teacherName))
