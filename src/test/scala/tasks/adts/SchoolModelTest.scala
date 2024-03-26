package tasks.adts

import org.junit.Test
import org.junit.Assert.*
import u03.Sequences.Sequence.*
import SchoolModel.*
import SchoolModel.given
import SchoolModel.SchoolModuleImpl.SchoolImpl
import SchoolModel.SchoolModuleImpl.TeacherImpl
import SchoolModel.SchoolModuleImpl.CourseImpl

class SchoolModelTest:

    val module = summon[SchoolModule]
    import module.*

    @Test def newSchool(): Unit =
        assertEquals(SchoolImpl(Nil(), Nil()), school())
