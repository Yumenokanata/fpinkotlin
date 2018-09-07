package localeffects

import org.junit.Test

class StreamTest {
    @Test
    fun testTake() {
        println(ST.runST(p2).write { 1 })
    }
}