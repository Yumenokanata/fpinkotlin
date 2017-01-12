package laziness

import Effect1
import head
import datastructures.List
import datastructures.cons
import errorhanding.Option
import errorhanding.orSome
import monad.H1
import tail
import java.util.*

/**
 * Created by yume on 17-1-9.
 */
sealed class Stream<out A> : H1<Stream.T, A> {
    object T

    abstract val head: () -> A
    abstract val tail: () -> Stream<A>

    object Empty : Stream<Nothing>() {
        override val head: Nothing
            get() = throw Error("head on empty stream")
        override val tail: () -> Stream<Nothing>
            get() = throw Error("tail on empty stream")
    }

    data class Cons<A>(val h: () -> A, val t: () -> Stream<A>) : Stream<A>() {
        override val head: () -> A
            get() = h
        override val tail: () -> Stream<A>
            get() = t
    }

    val isEmpty
        get() = this is Empty

    val isNotEmpty
        get() = !isEmpty

    // The natural recursive solution
    fun toListRecursive(): List<A> = when(this) {
        is Cons -> h() cons t().toListRecursive()
        else -> List.Nil
    }

    /*
    The above solution will stack overflow for large streams, since it's
    not tail-recursive. Here is a tail-recursive implementation. At each
    step we cons onto the front of the `acc` list, which will result in the
    reverse of the stream. Then at the end we reverse the result to get the
    correct order again.
    */
    fun toList(): List<A> = go(this, List.Nil).reverse()

    private tailrec fun go(s: Stream<A>, acc: List<A>): List<A> = when(s) {
        is Cons -> go(s.t(), s.h().cons(acc))
        else -> acc
    }

    /*
    In order to avoid the `reverse` at the end, we could write it using a
    mutable list buffer and an explicit loop instead. Note that the mutable
    list buffer never escapes our `toList` method, so this function is
    still _pure_.
    */
    fun toListFast(): List<A> {
        val buf = LinkedList<A>()
        tailrec fun go(s: Stream<A>): LinkedList<A> = when(s) {
            is Cons -> {
                buf += s.h()
                go(s.t())
            }
            else -> buf
        }
        return List.iterableList(go(this))
    }

    /*
      Create a new Stream<A> from taking the n first elements from this. We can achieve that by recursively
      calling take on the invoked tail of a cons cell. We make sure that the tail is not invoked unless
      we need to, by handling the special case where n == 1 separately. If n == 0, we can avoid looking
      at the stream at all.
    */
    fun take(n: Int): Stream<A> = when {
        this is Cons && n > 1 -> cons(h, { t().take(n - 1) })
        this is Cons && n == 1 -> cons(h, { empty() })
        else -> empty()
    }

    /*
    It's a common Scala style to write method calls without `.` notation, as in `t() takeWhile f`.
    */
    fun takeWhile(f: (A) -> Boolean): Stream<A> = when {
        this is Cons && f(h()) -> cons(h, { t().takeWhile(f) })
        else -> empty()
    }

    fun <B> foldRight(z: () -> B, f: (A, () -> B) -> B): B = // The arrow `->` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
            when(this) {
                is Cons -> f(h(), { t().foldRight(z, f) }) // If `f` doesn't evaluate its second argument, the recursion never occurs.
                else -> z()
            }

    fun <B> foldLeft(b: B, f: (B, A) -> B): B {
        var x = b

        var xs = this
        while (!xs.isEmpty) {
            x = f(x, xs.head())
            xs = xs.tail()
        }

        return x
    }

    fun exists(p: (A) -> Boolean): Boolean =
            foldRight({ false }) { a, b -> p(a) || b() } // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

    /*
    Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a nonmatching element is found.
    */
    fun forAll(f: (A) -> Boolean): Boolean =
            foldRight({ true }) { a, b -> f(a) && b() }

    fun takeWhile_1(f: (A) -> Boolean): Stream<A> =
            foldRight({ empty<A>() }) { h, t ->
                if (f(h)) cons({ h }, t)
                else      empty()
            }

    fun headOption(): Option<A> =
            foldRight({ Option.none<A>() }) { h, _ -> Option.Some(h) }

    fun <B> map(f: (A) -> B): Stream<B> =
            foldRight({ empty<B>() }) { h, t -> cons({ f(h) }, t) }

    fun filter(f: (A) -> Boolean): Stream<A> =
            foldRight({ empty<A>() }) { h, t ->
                if (f(h)) cons({ h }, t)
                else t()
            }

    fun <B> flatMap(f: (A) -> Stream<B>): Stream<B> =
            foldRight({ empty<B>() }) { h, t -> f(h).append(t)}

    fun <B> mapViaUnfold(f: (A) -> B): Stream<B> =
            unfold(this) { s ->
                when(s) {
                    is Cons -> Option.Some(f(s.h()) to s.t())
                    else -> Option.none<Pair<B, Stream<A>>>()
                }
            }

    fun takeViaUnfold(n: Int): Stream<A> =
            unfold(this to n) { (s, num) ->
                when {
                    s is Cons && num == 1 -> Option.Some(s.h() to (empty<A>() to 0))
                    s is Cons && num > 1 -> Option.Some(s.h() to (s.t() to n -1))
                    else -> Option.none<Pair<A, Pair<Stream<A>, Int>>>()
                }
            }

    fun takeWhileViaUnfold(f: (A) -> Boolean): Stream<A> =
            unfold(this) { s ->
                when {
                    s is Cons && f(s.h()) -> Option.Some(s.h() to s.t())
                    else -> Option.none<Pair<A, Stream<A>>>()
                }
            }

    fun <B, C> zipWith(s2: Stream<B>, f: (A, B) -> C): Stream<C> =
            unfold(this to s2) { ps ->
                val (st1, st2) = ps
                when {
                    st1 is Cons && st2 is Cons -> Option.Some(f(st1.h(), st2.h()) to (st1.t() to st2.t()))
                    else -> Option.none<Pair<C, Pair<Stream<A>, Stream<B>>>>()
                }
            }

    // special case of `zipWith`
    fun <B> zip(s2: Stream<B>): Stream<Pair<A,B>> =
            zipWith(s2){ a, b -> a to b }

    fun <B> zipAll(s2: Stream<B>): Stream<Pair<Option<A>, Option<B>>> =
            zipWithAll(s2) { it }

    fun <B, C> zipWithAll(s2: Stream<B>, f: (Pair<Option<A>, Option<B>>) -> C): Stream<C> =
    unfold(this to s2) { (st1, st2) ->
        when {
            st1 is Empty && st2 is Empty -> Option.none<Pair<C, Pair<Stream<A>, Stream<B>>>>()
            st1 is Cons && st2 is Empty -> Option.Some(f(Option.Some(st1.h()) to Option.none()) to (st1.t() to empty<B>()))
            st1 is Empty && st2 is Cons -> Option.Some(f(Option.none<A>() to Option.Some(st2.h())) to (empty<A>() to st2.t()))
            st1 is Cons && st2 is Cons -> Option.Some(f(Option.Some(st1.h()) to Option.Some(st2.h())) to (st1.t() to st2.t()))
            else -> throw Exception("Impossible")
        }
    }

    /*
    `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that `s2` is exhausted. If `s` is exhausted first, or we find an element that doesn't match, we terminate early. Using non-strictness, we can compose these three separate logical steps--the zipping, the termination when the second stream is exhausted, and the termination if a nonmatching element is found or the first stream is exhausted.
    */
    fun <A> startsWith(s: Stream<A>): Boolean =
            zipAll(s).takeWhile { it.second is Option.Some }.forAll { (a1, a2) -> a1 == a2 }

    /*
    The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
    */
    fun tails(): Stream<Stream<A>> =
            unfold(this) { a ->
                when {
                    a is Empty -> Option.none<Pair<Stream<A>, Stream<A>>>()
                    else -> Option.Some(a to a.drop(1))
                }
            }.append { apply(empty()) }

    fun <A> hasSubsequence(s: Stream<A>): Boolean =
            tails().exists { it.startsWith(s) }

    /*
    The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.

    The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
    */
    fun <B> scanRight(z: B, f: (A, () -> B) -> B): Stream<B> =
            foldRight({ z to apply(z) }) { a, p0 ->
                // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
                val p1 = lazy(p0)
                val b2 = f(a, { p1.value.first })
                b2 to cons({ b2 }, { p1.value.second })
            }.second

    /**
     * Performs a side-effect for each element of this stream.

     * @param f The side-effect to perform for the given element.
     */
    fun foreachDoEffect(f: Effect1<A>) {
        var xs = this
        while (xs.isNotEmpty) {
            f(xs.head())
            xs = xs.tail()
        }
    }

    /**
     * Returns the element at the given index if it exists, fails otherwise.

     * @param i The index at which to get the element to return.
     * *
     * @return The element at the given index if it exists, fails otherwise.
     */
    fun index(i: Int): A {
        if (i < 0)
            throw error("index $i out of range on stream")
        else {
            var xs = this
            for (c in 0..i - 1) {
                if (xs.isEmpty)
                    throw error("index $i out of range on stream")
                xs = xs.tail()
            }

            if (xs.isEmpty)
                throw error("index $i out of range on stream")

            return xs.head()
        }
    }

    /**
     * The length of this stream. This function will not terminate for an infinite stream.

     * @return The length of this stream.
     */
    fun length(): Int {
        // we're using an iterative approach here as the previous implementation (toList().length()) took
        // very long even for some 10000 elements.
        var xs = this
        var i = 0
        while (!xs.isEmpty) {
            xs = xs.tail()
            i += 1
        }
        return i
    }

    /**
     * Returns the position of the first element matching the given predicate, if any.

     * @param p A predicate to match.
     * *
     * @return the position of the first element matching the given predicate, if any.
     */
    fun indexOf(p: (A) -> Boolean): Option<Int> =
            zipIndex().find({ p2 -> p(p2.first) }).map { it.second }

    /**
     * Zips this stream with the index of its element as a pair.

     * @return A new stream with the same length as this stream.
     */
    fun zipIndex(): Stream<Pair<A, Int>> = zipWith(range(0), { a, b -> a to b })

    /**
     * Returns an infinite stream of integers from the given `from` value (inclusive).

     * @param from The minimum value for the stream (inclusive).
     * *
     * @return A stream of integers from the given `from` value (inclusive).
     */
    fun range(from: Int): Stream<Int> = Stream.cons({ from }, { range(from + 1) })

    /**
     * Reverse this stream in constant stack space.

     * @return A new stream that is the reverse of this one.
     */
    fun reverse(): Stream<A> =
            foldLeft(Stream.empty<A>(), { ls, a -> Stream.cons({ a }, { ls }) })

    /**
     * Returns a stream of all infixes of this stream. A stream is considered to contain itself.

     * @return a stream of the infixes of this stream.
     */
    fun substreams(): Stream<Stream<A>> = tails().flatMap { it.inits() }


    /**
     * Returns a stream of all prefixes of this stream. A stream is considered a prefix of itself in tnis context.

     * @return a stream of the prefixes of this stream, starting with the stream itself.
     */
    fun inits(): Stream<Stream<A>> {
        val nil = Stream.cons({ empty<A>() }, { empty() })
        return if (isEmpty) nil else nil.append { tail().inits().map { Stream.cons({ head() }, { it }) } }
    }

    companion object {
        fun <A> cons(hd: () -> A, tl: () -> Stream<A>): Stream<A> {
            val head = lazy(hd)
            val tail = lazy(tl)
            return Cons({ head.value }, { tail.value })
        }

        fun <A> empty(): Stream<A> = Empty

        fun <A : B, B> Stream<A>.append(s: () -> Stream<B>): Stream<B> =
                foldRight(s) { h, t -> cons({ h }, t) }

        fun <A : B, B> Stream<A>.append(s: Stream<B>): Stream<B> =
                foldRight({ s }) { h, t -> cons({ h }, t) }

        fun <A> apply(vararg args: A): Stream<A> =
        if (args.isEmpty()) empty()
        else cons({ args.head }, { apply(*args.tail) })

        fun ones(): Stream<Int> = Stream.cons({ 1 }, { ones() })

        // This is more efficient than `cons(a, constant(a))` since it's just
        // one object referencing itself.
        fun <A> constant(a: A): Stream<A> = Cons({ a }, { constant(a) })

        fun from(n: Int): Stream<Int> = cons({ n }, { from(n+1) })

        fun <A> repeat(a: A): Stream<A> = Cons({ a }, { repeat(a) })

        val fibs = {
            fun go(f0: Int, f1: Int): Stream<Int> =
                    cons({ f0 }, { go(f1, f0+f1) })
            go(0, 1)
        }

        fun <A, S> unfold(z: S, f: (S) -> Option<Pair<A, S>>): Stream<A> {
            val option = f(z)
            return when {
                option is Option.Some -> {
                    val h = option.get.first
                    val s = option.get.second
                    cons({ h }, { unfold(s, f) })
                }
                else -> empty()
            }
        }

        /*
          Create a new Stream<A> from this, but ignore the n first elements. This can be achieved by recursively calling
          drop on the invoked tail of a cons cell. Note that the implementation is also tail recursive.
        */
        tailrec fun <A> Stream<A>.drop(n: Int): Stream<A> = when {
            this is Cons && n > 0 -> t().drop(n - 1)
            else -> this
        }

        tailrec fun <A> Stream<A>.find(f: (A) -> Boolean): Option<A> = when(this) {
            is Empty -> Option.none()
            is Cons -> if (f(h())) Option.Some(h()) else t().find(f)
        }

        /*
        The below two implementations use `fold` and `map` functions in the Option class to implement unfold, thereby doing away with the need to manually pattern match as in the above solution.
         */
        fun <A, S> unfoldViaFold(z: S, f: (S) -> Option<Pair<A, S>>): Stream<A> =
                f(z).option(empty<A>()) { (a, s) -> cons({ a }, { unfold(s, f) }) }

        fun <A, S> unfoldViaMap(z: S, f: (S) -> Option<Pair<A, S>>): Stream<A> =
                f(z).map { (a, s) -> cons({ a }, { unfold(s, f) }) }.orSome { empty<A>() }

        /*
        Scala provides shorter syntax when the first action of a function literal is to match on an expression.  The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p -> p match { case (f0,f1) -> ... }`, but we avoid having to choose a name for `p`, only to pattern match on it.
        */
        val fibsViaUnfold =
                unfold(0 to 1) { (f0, f1) -> Option.Some(f0 to (f1 to f0 + f1)) }

        fun fromViaUnfold(n: Int) =
                unfold(n) { n -> Option.Some(n to n + 1) }

        fun <A> constantViaUnfold(a: A) =
                unfold(a) { Option.Some(a to a) }

        // could also of course be implemented as constant(1)
        val onesViaUnfold = unfold(1) { Option.Some(1 to 1) }

        /**
         * Returns a string from the given stream of characters. The inverse of this function is [ ][.fromString].

         * @param cs The stream of characters to produce the string from.
         * *
         * @return A string from the given stream of characters.
         */
        fun asString(cs: Stream<Char>): String {
            val sb = StringBuilder()
            cs.foreachDoEffect({ sb.append(it) })
            return sb.toString()
        }

//        /**
//         * Returns a stream of characters from the given string. The inverse of this function is [ ][.asString].
//
//         * @param s The string to produce the stream of characters from.
//         * *
//         * @return A stream of characters from the given string.
//         */
//        fun fromString(s: String): Stream<Char> {
//            return LazyString.str(s).toStream()
//        }
    }
}

fun <A> H1<Stream.T, A>.toOri(): Stream<A> = this as Stream<A>