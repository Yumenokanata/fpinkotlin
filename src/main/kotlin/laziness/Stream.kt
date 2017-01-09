package laziness

import fj.data.List
import fj.data.Option
import head
import monad.cons
import tail
import java.util.*

/**
 * Created by yume on 17-1-9.
 */
sealed class Stream<A> {

    object Empty : Stream<Nothing>()
    data class Cons<A>(val h: () -> A, val t: () -> Stream<A>) : Stream<A>()

    // The natural recursive solution
    fun toListRecursive(): List<A> = when(this) {
        is Cons -> h() cons t().toListRecursive()
        else -> List.nil()
    }

    /*
    The above solution will stack overflow for large streams, since it's
    not tail-recursive. Here is a tail-recursive implementation. At each
    step we cons onto the front of the `acc` list, which will result in the
    reverse of the stream. Then at the end we reverse the result to get the
    correct order again.
    */
    fun toList(): List<A> = go(this, List.nil()).reverse()

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
            foldRight({ Option.none<A>() }) { h, _ -> Option.some(h) }

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
                    is Cons -> Option.some(f(s.h()) to s.t())
                    else -> Option.none()
                }
            }

    fun takeViaUnfold(n: Int): Stream<A> =
    unfold(this to n) { (s, num) ->
        when {
            s is Cons && num == 1 -> Option.some(s.h() to (empty<A>() to 0))
            s is Cons && num > 1 -> Option.some(s.h() to (s.t() to n -1))
            else -> Option.none()
        }
    }

    fun takeWhileViaUnfold(f: (A) -> Boolean): Stream<A> =
    unfold(this) { s ->
        when {
            s is Cons && f(s.h()) -> Option.some(s.h() to s.t())
            else -> Option.none()
        }
    }

    fun <B, C> zipWith(s2: Stream<B>, f: (A, B) -> C): Stream<C> =
            unfold(this to s2) { ps ->
                val (st1, st2) = ps
                when {
                    st1 is Cons && st2 is Cons -> Option.some(f(st1.h(), st2.h()) to (st1.t() to st2.t()))
                    else -> Option.none()
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
            st1 is Empty && st2 is Empty -> Option.none()
            st1 is Cons && st2 is Empty -> Option.some(f(Option.some(st1.h()) to Option.none()) to (st1.t() to empty<B>()))
            st1 is Empty && st2 is Cons -> Option.some(f(Option.none<A>() to Option.some(st2.h())) to (empty<A>() to st2.t()))
            st1 is Cons && st2 is Cons -> Option.some(f(Option.some(st1.h()) to Option.some(st2.h())) to (st1.t() to st2.t()))
            else -> throw Exception("Impossible")
        }
    }

    /*
    `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that `s2` is exhausted. If `s` is exhausted first, or we find an element that doesn't match, we terminate early. Using non-strictness, we can compose these three separate logical steps--the zipping, the termination when the second stream is exhausted, and the termination if a nonmatching element is found or the first stream is exhausted.
    */
    fun <A> startsWith(s: Stream<A>): Boolean =
            zipAll(s).takeWhile { it.second.isSome }.forAll { (a1, a2) -> a1 == a2 }

    /*
    The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
    */
    fun tails(): Stream<Stream<A>> =
            unfold(this) { a ->
                when {
                    a is Empty -> Option.none()
                    else -> Option.some(a to a.drop(1))
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

    companion object {
        fun <A> cons(hd: () -> A, tl: () -> Stream<A>): Stream<A> {
            val head = lazy(hd)
            val tail = lazy(tl)
            return Cons({ head.value }, { tail.value })
        }

        @Suppress("UNCHECKED_CAST")
        fun <A> empty(): Stream<A> = Empty as Stream<A>

        fun <A : B, B> Stream<A>.append(s: () -> Stream<B>): Stream<B> =
                foldRight(s) { h, t -> cons({ h }, t) }

        fun <A> apply(vararg args: A): Stream<A> =
        if (args.isEmpty()) empty()
        else cons({ args.head }, { apply(*args.tail) })

        fun ones(): Stream<Int> = Stream.cons({ 1 }, { ones() })

        // This is more efficient than `cons(a, constant(a))` since it's just
        // one object referencing itself.
        fun <A> constant(a: A): Stream<A> = Cons({ a }, { constant(a) })

        fun from(n: Int): Stream<Int> = cons({ n }, { from(n+1) })

        val fibs = {
            fun go(f0: Int, f1: Int): Stream<Int> =
                    cons({ f0 }, { go(f1, f0+f1) })
            go(0, 1)
        }

        fun <A, S> unfold(z: S, f: (S) -> Option<Pair<A, S>>): Stream<A> {
            val option = f(z)
            return when {
                option.isSome -> {
                    val h = option.some().first
                    val s = option.some().second
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
            is Cons -> if (f(h())) Option.some(h()) else t().find(f)
        }

        /*
        The below two implementations use `fold` and `map` functions in the Option class to implement unfold, thereby doing away with the need to manually pattern match as in the above solution.
         */
        fun <A, S> unfoldViaFold(z: S, f: (S) -> Option<Pair<A, S>>): Stream<A> =
                f(z).option(empty<A>()) { (a, s) -> cons({ a }, { unfold(s, f) }) }

        fun <A, S> unfoldViaMap(z: S, f: (S) -> Option<Pair<A, S>>): Stream<A> =
                f(z).map { (a, s) -> cons({ a }, { unfold(s, f) }) }.orSome(empty<A>())

        /*
        Scala provides shorter syntax when the first action of a function literal is to match on an expression.  The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p -> p match { case (f0,f1) -> ... }`, but we avoid having to choose a name for `p`, only to pattern match on it.
        */
        val fibsViaUnfold =
                unfold(0 to 1) { (f0, f1) -> Option.some(f0 to (f1 to f0 + f1)) }

        fun fromViaUnfold(n: Int) =
                unfold(n) { n -> Option.some(n to n + 1) }

        fun <A> constantViaUnfold(a: A) =
                unfold(a) { Option.some(a to a) }

        // could also of course be implemented as constant(1)
        val onesViaUnfold = unfold(1) { Option.some(1 to 1) }
    }
}