package tasks.adts

import org.junit.Test
import org.junit.Assert.*
import u03.Sequences.Sequence.*
import SchoolModel.*
import SchoolModel.given
import SchoolModuleImpl._school

class SchoolModelTest:

    val module = summon[SchoolModule]
    import module.*

    @Test def newSchool(): Unit =
        assertEquals(_school(Nil(), Nil()), school())
