package errorhanding

import applicative.listTraverse
import datastructures.List
import datastructures.List.Companion.foldRight
import datastructures.cons
import monad.H1

/**
 * Created by yume on 17-1-12.
 */

sealed class Option<out A> : H1<Option.T, A> {
    object T

    data class Some<out A>(val get: A) : Option<A>()
    object None : Option<Nothing>()

    fun <B> map(f: (A) -> B): Option<B> = when(this) {
        is None -> None
        is Some -> Some(f(get))
    }

    fun <B> flatMap(f: (A) -> Option<B>): Option<B> =
            map(f).getOrElse { None }

    /*
    Of course, we can also implement `flatMap` with explicit pattern matching.
    */
    fun <B> flatMap_1(f: (A) -> Option<B>): Option<B> = when(this) {
        is None -> None
        is Some -> f(get)
    }

    fun <B> option(b: B, f: (A) -> B): B = if(this is Some) f(get) else b

    fun filter(f: (A) -> Boolean): Option<A> = when {
        this is Some && f(get) -> this
        else -> None
    }

    /*
    This can also be defined in terms of `flatMap`.
    */
    fun filter_1(f: (A) -> Boolean): Option<A> =
            flatMap { a -> if (f(a)) Some(a) else None }

    companion object {
        fun <A> some(a: A): Option<A> = Some(a)

        fun <A> none(): Option<A> = None

        fun <A> fromNull(a: A?): Option<A> = if(a == null) None else Some(a)

        fun <A : B, B> Option<A>.getOrElse(default: () -> B): B = when(this) {
            is None -> default()
            is Some -> get
        }

        fun <A : B, B> Option<A>.orElse(ob: () -> Option<B>): Option<B> =
        this.map { Some(it) }.getOrElse(ob)

        /*
        Again, we can implement this with explicit pattern matching.
        */
        fun <A : B, B> Option<A>.orElse_1(ob: () -> Option<B>): Option<B> = when(this) {
            is None -> ob()
            is Some -> this
        }

//        fun mean(xs: List<Double>): Option<Double> =
//        if (xs.isEmpty) None
//        else Some(listTraverse.sum() / xs.length())

//        fun variance(xs: List<Double>): Option<Double> =
//        mean(xs) flatMap (m -> mean(xs.map(x -> math.pow(x - m, 2))))

        // a bit later in the chapter we'll learn nicer syntax for
        // writing functions like this
        fun <A, B, C> map2(a: Option<A>, b: Option<B>, f: (A, B) -> C): Option<C> =
                a.flatMap { aa -> b.map { bb -> f(aa, bb) } }

        /*
        Here's an explicit recursive version:
        */
        fun <A> sequence(a: List<Option<A>>): Option<List<A>> =
                when(a) {
                    is List.Nil -> Some(List.Nil)
                    is List.Cons -> a.head.flatMap { hh -> sequence(a.tail).map { hh cons it } }
                }

        /*
        It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise
        Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an
        unfortunate consequence of Scala using subtyping to encode algebraic data types.
        */
        fun <A> sequence_1(a: List<Option<A>>): Option<List<A>> =
                a.foldRight<Option<A>, Option<List<A>>>(Some(List.nil<A>()))
                { x, y -> map2(x,y) { a1, b1 -> a1 cons b1 } }

        fun <A, B> traverse(a: List<A>, f: (A) -> Option<B>): Option<List<B>> =
                when(a) {
                    is List.Nil -> Some(List.Nil)
                    is List.Cons -> map2(f(a.head), traverse(a.tail, f)) { a1, b1 -> a1 cons b1 }
                }

        fun <A, B> traverse_1(a: List<A>, f: (A) -> Option<B>): Option<List<B>> =
                a.foldRight<A, Option<List<B>>>(Some(List.nil<B>()))
                { h, t -> map2(f(h),t) { a1, b1 -> a1 cons b1 } }

        fun <A> sequenceViaTraverse(a: List<Option<A>>): Option<List<A>> =
                traverse(a) { it }
    }
}

fun <A> Option<A>.orSome(a: () -> A): A = if (this is Option.Some) get else a()

fun <A> Option<A>.orSome(a: A): A = if (this is Option.Some) get else a

fun <T> H1<Option.T, T>.toOri(): Option<T> = this as Option<T>