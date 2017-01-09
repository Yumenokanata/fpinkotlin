package laziness

import laziness.Stream.Companion.fibs
import laziness.Stream.Companion.ones
import org.junit.Test

import org.junit.Assert.*

/**
 * Created by yume on 17-1-9.
 */
class StreamTest {
    @Test
    fun testTake() {
        println(ones().take(10).toList().joinToString())
        println(fibs().take(10).toList().joinToString())
    }
}