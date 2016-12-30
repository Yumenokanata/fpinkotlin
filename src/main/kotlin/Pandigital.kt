import fj.F
import fj.P
import fj.P2
import fj.Show.intShow
import fj.Show.listShow
import fj.data.Stream
import fj.function.Integers

/**
 * Created by yume on 16-12-12.
 */

fun main(args: Array<String>) {
//    println(int_seq(listOf(1, 2, 3, 4)))
    val list = count().toList().nub()
    intShow.println(Integers.sum(list))
    listShow<Int>(intShow).println(list)
}

internal fun permute(stream: Stream<Char>): Stream<Stream<Char>> =
        stream.bind { x ->
            val s = stream.filter { a -> a != x }
            if (s.isEmpty)
                Stream.single(Stream.single(x))
            else
                permute(s).map { xs -> Stream.cons<Char>(x) { xs } }
        }

internal fun count(): Stream<Int> =
        permute(Stream.fromString("123456789"))
                .map { digitToInt(it) }
                .map { i -> quotRem(i, 100000) }
                .filter { p2 -> check(10, p2) || check(100, p2) }
                .map { it._1() }

internal fun digitToInt(stream: Stream<Char>): Int =
        stream.foldLeft({ s, n -> s * 10 + Character.getNumericValue(n) }, 0)

internal fun quotRem(x: Int, y: Int): P2<Int, Int> = P.p(x / y, x % y)

internal fun check(n: Int, p2: P2<Int, Int>): Boolean {
    val p = quotRem(p2._2(), n)
    return p2._1() == p._1() * p._2()
}