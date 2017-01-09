package iomonad

import compose
import fj.data.List
import org.junit.Test
import iomonad.IOF.IO
import iomonad.IOF.function0Monad
import monad.H1
import monad.H2
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

//        val f: (Int) -> Stay<Int> = { IOF.Free.Return(it) }
//        val g: (Int) -> Stay<Int> = List.replicate(100000, f).foldLeft({ a: (Int) -> Stay<Int>, b: (Int) -> Stay<Int> -> { x: Int -> a(x).flatMap(b) } } , f)
//        IOF.run(g(42), function0Monad)
        failingFn1(2)
    }

    fun failingFn2(i: Int): Int {
        try {
            val x = 42 + 5
            return x + ((throw Exception("fail")) as Int)
        } catch (e: Exception) {
            return 43
        }
    }


    fun failingFn1(i: Int): Int {
        val y: Int = (throw Exception("fail")) as Int
        try {
            val x = 42 + 5
            return x + y
        } catch (e: Exception) {
            return 43
        }
    }

    internal fun y(): Int {
        throw RuntimeException("fail")
    }
}

sealed class Either<L, R>: H2<Either.T, L, R> {
    class T

    data class Left<L, R>(val value: L) : Either<L, R>()

    data class Right<L, R>(val value: R) : Either<L, R>()
}

fun <L, R, B, C> Either<L, R>.map2(b: Either<L, B>, f: (R, B) -> C): Either<L, C> =
    when {
        this is Either.Right && b is Either.Right -> Either.Right(f(this.value, b.value))
        this is Either.Left && b is Either.Right -> Either.Left(this.value)
        this is Either.Right && b is Either.Left -> Either.Left(b.value)
        this is Either.Left && b is Either.Left -> Either.Left(this.value)
        else -> throw Exception("is impossible")
    }

data class Person(val name: Name, val age: Age)
data class Name(val value: String)
data class Age(val value: Int)

fun mkName(name: String): Either<String, Name> =
        if(name.isEmpty()) Either.Left("Name is empty.")
        else Either.Right(Name(name))

fun mkAge(age: Int): Either<String, Age> =
        if(age < 0) Either.Left("Age is out of range.")
        else Either.Right(Age(age))

fun mkPerson(name: String, age: Int): Either<String, Person> =
        mkName(name).map2(mkAge(age)) { n, a -> Person(n, a) }
