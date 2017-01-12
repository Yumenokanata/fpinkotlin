package errorhanding

import datastructures.List
import datastructures.List.Companion.foldRight
import datastructures.cons
import monad.H1
import monad.H2

/**
 * Created by yume on 17-1-12.
 */
fun <E, A> H2<Either.T, E, A>.toOri(): Either<E, A> = this as Either<E, A>

fun <E, A> H1<H1<Either.T, E>, A>.toOri(): Either<E, A> = this as Either<E, A>

sealed class Either<out E, out A> : H2<Either.T, E, A> {
    object T

    data class Left<out E>(val get: E) : Either<E, Nothing>()
    data class Right<out A>(val get: A) : Either<Nothing, A>()

    fun <B> map(f: (A) -> B): Either<E, B> =
            when(this) {
                is Right -> Right(f(get))
                is Left -> Left(get)
            }

    fun <C> fold(fa: (E) -> C, fb: (A) -> C): C = when(this) {
        is Right -> fb(get)
        is Left  -> fa(get)
    }

    companion object {
        fun <E, A> right(a: A): Either<E, A> = Right(a)

        fun <E, A> left(e: E): Either<E, A> = Left(e)

        fun <E : EE, EE, A, B> flatMap(a: Either<E, A>, f: (A) -> Either<EE, B>): Either<EE, B> =
                when(a) {
                    is Left -> Left(a.get)
                    is Right -> f(a.get)
                }

        fun <E : EE, EE, A : AA, AA> orElse(a: Either<E, A>, b: () -> Either<EE, AA>): Either<EE, AA> =
                when(a) {
                    is Left -> b()
                    is Right -> Right(a.get)
                }

        fun <E : EE, EE, A, B, C> map2(a: Either<E, A>, b: Either<EE, B>, f: (A, B) -> C): Either<EE, C> =
                flatMap(a) { a1 -> b.map { b1 -> f(a1, b1) } }

//        fun mean(xs: List<Double>): Either<String, Double> =
//        if (xs.isEmpty)
//        Left("mean of empty list!")
//        else
//        Right(xs.sum / xs.length)

        fun safeDiv(x: Int, y: Int): Either<Exception, Int> =
                try { Right(x / y) }
                catch (e: Exception){ Left(e) }

        fun <A> Try(a: () -> A): Either<Exception, A> =
                try { Right(a()) }
                catch (e: Exception){ Left(e) }

        fun <E, A, B> traverse(es: List<A>, f: (A) -> Either<E, B>): Either<E, List<B>> =
                when(es) {
                    is List.Nil -> Right(List.Nil)
                    is List.Cons -> map2(f(es.head), traverse(es.tail, f)) { a, b -> a cons b }
                }

        fun <E, A, B> traverse_1(es: List<A>, f: (A) -> Either<E, B>): Either<E, List<B>> =
                es.foldRight<A, Either<E,List<B>>>(Right(List.nil<B>()))
                { a, b -> map2(f(a), b) { a1, b1 -> a1 cons b1 } }

        fun <E, A> sequence(es: List<Either<E, A>>): Either<E, List<A>> =
                traverse(es) { it }

        /*
        There are a number of variations on `Option` and `Either`. If we want to accumulate multiple errors, a simple
        approach is a new data type that lets us keep a list of errors in the data constructor that represents failures:

        trait Partial[+A,+B]
        case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
        case class Success[+B](get: B) extends Partial[Nothing,B]

        There is a type very similar to this called `Validation` in the Scalaz library. You can implement `map`, `map2`,
        `sequence`, and so on for this type in such a way that errors are accumulated when possible (`flatMap` is unable to
        accumulate errors--can you see why?). This idea can even be generalized further--we don't need to accumulate failing
        values into a list; we can accumulate values using any user-supplied binary function.

        It's also possible to use `Either[List[E],_]` directly to accumulate errors, using different implementations of
        helper functions like `map2` and `sequence`.
        */
    }
}