package iomonad

import compose
import fj.data.List
import org.junit.Test
import iomonad.IOF.IO
import iomonad.IOF.function0Monad
import parallelism.Nonblocking

/**
 * Created by yume on 17-1-4.
 */
class IOTest {

    typealias Stay<A> = IOF.Free<F0U, A>

    @Test
    fun testIO() {
        // val f: (Int) -> Int = { it }
        // val g = List.replicate(100000, f).foldLeft({ h, l -> h compose l }, f)
        // g(42) will be throw StackOverflowError

        val f: (Int) -> Stay<Int> = { IOF.Free.Return(it) }
        val g: (Int) -> Stay<Int> = List.replicate(100000, f).foldLeft({ a: (Int) -> Stay<Int>, b: (Int) -> Stay<Int> -> { x: Int -> a(x).flatMap(b) } } , f)
        IOF.run(g(42), function0Monad)
    }
}
