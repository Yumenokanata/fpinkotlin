package datastructures

import head
import monad.H1
import tail
import java.util.*

// `List` data type, parameterized on a type, `A`
sealed class List<out A> : H1<List.T, A>, Iterable<A> {
    object T

    abstract val head: A
    abstract val tail: List<A>

    object Nil : List<Nothing>() // A `List` data constructor representing the empty list
    {
        override val head: Nothing
            get() = throw Error("head on empty list") //To change initializer of created properties use File | Settings | File Templates.
        override val tail: List<Nothing>
            get() = throw Error("tail on empty list") //To change initializer of created properties use File | Settings | File Templates.
    }

    /* Another data constructor, representing nonempty lists. Note that `tail` is another `List<A>`,
    which may be `Nil` or another `Cons`.
     */
    data class Cons<out A>(override val head: A,
                           override val tail: List<A>) : List<A>()

    /*
    A natural solution is using `foldRight`, but our implementation of `foldRight` is not stack-safe. We can
    use `foldRightViaFoldLeft` to avoid the stack overflow (variation 1), but more commonly, with our current
    implementation of `List`, `map` will just be implemented using local mutation (variation 2). Again, note that the
    mutation isn't observable outside the function, since we're only mutating a buffer that we've allocated.
    */
    fun <B> map(f: (A) -> B): List<B> =
            foldRight(Nil as List<B>) { h, t -> Cons(f(h), t) }

    fun <B> map_1(f: (A) -> B): List<B> =
            foldRightViaFoldLeft(this, Nil as List<B>) { h, t -> Cons(f(h),t) }

    fun <B> map_2(f: (A) -> B): List<B> {
        val buf = LinkedList<B>()
        tailrec fun go(l: List<A>): Unit = when(l) {
            is Nil -> Unit
            is Cons -> {
                buf += f(l.head)
                go(l.tail)
            }
        }
        go(this)
        return fromList(buf) // converting from the standard Scala list to the list we've defined here
    }

    /**
     * Returns the element at the given index if it exists, fails otherwise.

     * @param i The index at which to get the element to return.
     * *
     * @return The element at the given index if it exists, fails otherwise.
     */
    fun index(i: Int): A {
        if (i < 0 || i > length() - 1)
            throw error("index " + i + " out of range on list with length " + length())
        else {
            var xs = this

            for (c in 0..i - 1) xs = xs.tail

            return xs.head
        }
    }

    override fun iterator(): Iterator<A> = toCollection().iterator()

    fun toCollection(): Collection<A> {
        return object : AbstractCollection<A>() {
            override val size: Int
                get() = length()

            override fun iterator(): MutableIterator<A> {
                return object : MutableIterator<A> {
                    override fun remove() =
                        throw UnsupportedOperationException()

                    private var xs = this@List

                    override fun hasNext(): Boolean {
                        return xs.isNotEmpty
                    }

                    override fun next(): A =
                        when(xs) {
                            is Nil -> throw NoSuchElementException()
                            is Cons -> {
                                val a = (xs as Cons).head
                                xs = (xs as Cons).tail
                                a
                            }
                        }
                }
            }
        }
    }

    fun reverse(): List<A> = foldLeft(Nil as List<A>) { ls, a -> a cons ls }

    val isSingle
        get() = this is Cons && tail is Nil

    val isEmpty
        get() = this is Nil

    val isNotEmpty
        get() = !isEmpty

    /*
    No, this is not possible! The reason is because _before_ we ever call our function, `f`, we evaluate its argument,
    which in the case of `foldRight` means traversing the list all the way to the end. We need _non-strict_ evaluation
    to support early termination---we discuss this in chapter 5.
    */

    /*
    We get back the original list! Why is that? As we mentioned earlier, one way of thinking about what `foldRight` "does"
    is it replaces the `Nil` constructor of the list with the `z` argument, and it replaces the `Cons` constructor with
    the given function, `f`. If we just supply `Nil` for `z` and `Cons` for `f`, then we get back the input list.

    foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List<Int>)(Cons(_,_))
    Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List<Int>)(Cons(_,_)))
    Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List<Int>)(Cons(_,_))))
    Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List<Int>)(Cons(_,_)))))
    Cons(1, Cons(2, Cons(3, Nil)))
    */

    fun length(): Int = foldRight(0) { _, acc -> acc + 1 }

    /**
     * Splits this list into two lists at the given index. If the index goes out of bounds, then it is
     * normalised so that this function never fails.

     * @param i The index at which to split this list in two parts.
     * *
     * @return A pair of lists split at the given index of this list.
     */
    fun splitAt(i: Int): Pair<List<A>, List<A>> {
        var c = 0
        var first = List.nil<A>()
        var second = nil<A>()
        var xs = this
        while (xs.isNotEmpty) {
            val h = xs.head
            if (c < i) {
                first = h cons first
            } else {
                second = h cons second
            }
            c++
            xs = xs.tail
        }
        return first.reverse() to second.reverse()
    }

    companion object {// `List` companion object. Contains functions for creating and working with lists.
        fun <A> nil(): List<A> = Nil

        fun sum(ints: List<Int>): Int = when(ints) { // A function that uses pattern matching to add up a list of integers
            is Nil -> 0 // The sum of the empty list is 0.
            is Cons -> ints.head + sum(ints.tail) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
        }

        fun product(ds: List<Double>): Double = when(ds) {
            is Nil -> 1.0
            is Cons -> if (ds.head == 0.0) 0.0 else ds.head * product(ds.tail)
        }

        fun <A> apply(vararg args: A): List<A> = // Variadic function syntax
                if (args.isEmpty()) Nil
                else Cons(args.head, apply(*args.tail))

        fun <A> fromList(args: kotlin.collections.List<A>): List<A> = // Variadic function syntax
                if (args.isEmpty()) Nil
                else Cons(args.head, fromList(args.tail))

        fun fromString(s: String): List<Char> {
            var cs = nil<Char>()

            for (i in s.length - 1 downTo 0)
                cs = Cons(s[i], cs)

            return cs
        }

        fun <A> iterableList(i: Iterable<A>): List<A> {
            var ls: List<A> = Nil
            for (a in i) {
                ls = Cons(a, ls)
            }
            return ls
        }

        fun <A> replicate(n: Int, a: A): List<A> {
            var list = nil<A>()
            for (i in 0..n - 1) {
                list = a cons list
            }
            return list
        }

        fun <A, B> List<A>.foldRight(z: B, f: (A, B) -> B): B = // Utility functions
                when(this) {
                    is Nil -> z
                    is Cons -> f(head, tail.foldRight(z, f))
                }

        fun sum2(ns: List<Int>) =
                ns.foldRight(0) { x, y -> x + y }

        fun product2(ns: List<Double>) =
                ns.foldRight(1.0) { a, b -> a * b } // `_ * _` is more concise notation for `(x,y) -> x * y`; see sidebar

        /*
        3. The third case is the first that matches, with `x` bound to 1 and `y` bound to 2.
        */

        /*
        Although we could return `Nil` when the input list is empty, we choose to throw an exception instead. This is
        a somewhat subjective choice. In our experience, taking the tail of an empty list is often a bug, and silently
        returning a value just means this bug will be discovered later, further from the place where it was introduced.

        It's generally good practice when pattern matching to use `_` for any variables you don't intend to use on the
        right hand side of a pattern. This makes it clear the value isn't relevant.
        */
        fun <A> tail(l: List<A>): List<A> =
                when(l) {
                    is Nil -> throw Exception("tail of empty list")
                    is Cons -> l.tail
                }

        /*
        If a function body consists solely of a match expression, we'll often put the match on the same line as the
        function signature, rather than introducing another level of nesting.
        */
        fun <A> setHead(l: List<A>, h: A): List<A> =
                when(l){
                    is Nil -> throw Exception("setHead on empty list")
                    is Cons -> Cons(h, l.tail)
                }

        /*
        Again, it's somewhat subjective whether to throw an exception when asked to drop more elements than the list
        contains. The usual funault for `drop` is not to throw an exception, since it's typically used in cases where this
        is not indicative of a programming error. If you pay attention to how you use `drop`, it's often in cases where the
        length of the input list is unknown, and the number of elements to be dropped is being computed from something else.
        If `drop` threw an exception, we'd have to first compute or check the length and only drop up to that many elements.
        */
        fun <A> drop(l: List<A>, n: Int): List<A> =
                if (n <= 0) l
                else when(l) {
                    is Nil -> Nil
                    is Cons -> drop(l.tail, n - 1)
                }

        /*
        Somewhat overkill, but to illustrate the feature we're using a _pattern guard_, to only match a `Cons` whose head
        satisfies our predicate, `f`. The syntax is to add `if <cond>` after the pattern, before the `->`, where `<cond>` can
        use any of the variables introduced by the pattern.
        */
        tailrec fun <A> dropWhile(l: List<A>, f: (A) -> Boolean): List<A> =
                when {
                    l is Cons && f(l.head) -> dropWhile(l.tail, f)
                    else -> l
                }

        /*
        Note that we're copying the entire list up until the last element. Besides being inefficient, the natural recursive
        solution will use a stack frame for each element of the list, which can lead to stack overflows for
        large lists (can you see why?). With lists, it's common to use a temporary, mutable buffer internal to the
        function (with lazy lists or streams, which we discuss in chapter 5, we don't normally do this). So long as the
        buffer is allocated internal to the function, the mutation is not observable and RT is preserved.

        Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which
        doesn't require even local mutation. We'll write a reverse function later in this chapter.
        */
        fun <A> init(l: List<A>): List<A> =
                when(l) {
                    is Nil -> throw Exception("init of empty list")
                    is Cons -> if(l.tail == Nil) Nil else Cons(l.head, init(l.tail))
                }

        fun <A> init2(l: List<A>): List<A> {
            val buf = LinkedList<A>()

            tailrec fun go(cur: List<A>): List<A> = when(cur) {
                is Nil -> throw Exception("init of empty list")
                is Cons -> if(cur.tail == Nil) fromList(buf) else {
                    buf += cur.head;
                    go(cur.tail)
                }
            }
            return go(l)
        }

        /*
        It's common practice to annotate functions you expect to be tail-recursive with the `tailrec` annotation. If the
        function is not tail-recursive, it will yield a compile error, rather than silently compiling the code and resulting
        in greater stack space usage at runtime.
        */
        tailrec fun <A, B> List<A>.foldLeft(z: B, f: (B, A) -> B): B = when(this) {
            is Nil -> z
            is Cons -> tail.foldLeft(f(z, head), f)
        }

        fun sum3(l: List<Int>) = l.foldLeft(0) { a, b -> a + b }
        fun product3(l: List<Double>) = l.foldLeft(1.0) { a, b -> a * b}

        fun <A> length2(l: List<A>): Int = l.foldLeft(0) { acc, h -> acc + 1 }

        fun <A> reverse(l: List<A>): List<A> = l.foldLeft(apply()) { acc, h -> Cons(h, acc) }

        /*
        The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack overflows
        when implementing a strict `foldRight` function as we've done in this chapter. (We'll revisit this in a later chapter,
        when we discuss laziness).

        The other implementations build up a chain of functions which, when called, results in the operations being performed
        with the correct associativity. We are calling `foldRight` with the `B` type being instantiated to `B -> B`, then
        calling the built up function with the `z` argument. Try expanding the funinitions by substituting equals for equals
        using a simple example, like `foldLeft(List(1,2,3), 0)(_ + _)` if this isn't clear. Note these implementations are
        more of theoretical interest - they aren't stack-safe and won't work for large lists.
        */
        fun <A, B> foldRightViaFoldLeft(l: List<A>, z: B, f: (A, B) -> B): B =
                reverse(l).foldLeft(z) { b, a -> f(a,b) }

        fun <A, B> foldRightViaFoldLeft_1(l: List<A>, z: B, f: (A, B) -> B): B =
                l.foldLeft({ b: B -> b }) { g, a -> { b -> g(f(a, b)) } } (z)

        fun <A, B> foldLeftViaFoldRight(l: List<A>, z: B, f: (B, A) -> B): B =
                l.foldRight({ b: B -> b }) { a, g -> { b -> g(f(b, a)) } } (z)

        fun <A> List<A>.append(a2: List<A>): List<A> =
                when(this) {
                    is List.Nil -> a2
                    is List.Cons -> List.Cons(head, tail.append(a2))
                }

        /*
        `append` simply replaces the `Nil` constructor of the first list with the second list, which is exactly the operation
        performed by `foldRight`.
        */
        fun <A> appendViaFoldRight(l: List<A>, r: List<A>): List<A> =
                l.foldRight(r) { h, l -> Cons(h, l) }

        /*
        Since `append` takes time proportional to its first argument, and this first argument never grows because of the
        right-associativity of `foldRight`, this function is linear in the total length of all lists. You may want to try
        tracing the execution of the implementation on paper to convince yourself that this works.

        Note that we're simply referencing the `append` function, without writing something like `(x,y) -> append(x,y)`
        or `append(_,_)`. In Scala there is a rather arbitrary distinction between functions funined as _methods_, which are
        introduced with the `fun` keyword, and function values, which are the first-class objects we can pass to other
        functions, put in collections, and so on. This is a case where Scala lets us pretend the distinction doesn't exist.
        In other cases, you'll be forced to write `append _` (to convert a `fun` to a function value)
        or even `(x: List<A>, y: List<A>) -> append(x,y)` if the function is polymorphic and the type arguments aren't known.
        */
        fun <A> concat(l: List<List<A>>): List<A> =
                l.foldRight(Nil as List<A>) { l1, l2 -> l1.append(l2) }

        fun add1(l: List<Int>): List<Int> =
                l.foldRight(Nil as List<Int>) { h, t -> Cons(h + 1, t) }

        fun doubleToString(l: List<Double>): List<String> =
                l.foldRight(Nil as List<String>) { h, t -> Cons(h.toString(), t) }

        /*
        The discussion about `map` also applies here.
        */
        fun <A> filter(l: List<A>, f: (A) -> Boolean): List<A> =
                l.foldRight(Nil as List<A>) { h, t -> if (f(h)) Cons(h,t) else t }

        fun <A> filter_1(l: List<A>, f: (A) -> Boolean): List<A> =
                foldRightViaFoldLeft(l, Nil as List<A>) { h, t -> if (f(h)) Cons(h,t) else t }

        fun <A> filter_2(l: List<A>, f: (A) -> Boolean): List<A> {
            val buf = LinkedList<A>()
            tailrec fun go(l: List<A>): Unit = when(l) {
                is Nil -> Unit
                is Cons -> if (f(l.head)) {
                    buf += l.head
                    go(l.tail)
                } else Unit
            }
            go(l)
            return fromList(buf) // converting from the standard Scala list to the list we've funined here
        }

        /*
        This could also be implemented directly using `foldRight`.
        */
        fun <A, B> flatMap(l: List<A>, f: (A) -> List<B>): List<B> =
                concat(l.map(f))

        fun <A> filterViaFlatMap(l: List<A>, f: (A) -> Boolean): List<A> =
                flatMap(l) { a -> if (f(a)) apply(a) else Nil }

        /*
        To match on multiple values, we can put the values into a pair and match on the pair, as shown next, and the same
        syntax extends to matching on N values (see sidebar "Pairs and tuples in Scala" for more about pair and tuple
        objects). You can also (somewhat less conveniently, but a bit more efficiently) nest pattern matches: on the
        right hand side of the `->`, simply begin another `match` expression. The inner `match` will have access to all the
        variables introduced in the outer `match`.

        The discussion about stack usage from the explanation of `map` also applies here.
        */
        fun addPairwise(a: List<Int>, b: List<Int>): List<Int> = when {
            a is Nil -> Nil
            b is Nil -> Nil
            a is Cons && b is Cons -> Cons(a.head + b.head, addPairwise(a.tail, b.tail))
            else -> throw Exception("Impossible")
        }

        /*
        This function is usually called `zipWith`. The discussion about stack usage from the explanation of `map` also
        applies here. By putting the `f` in the second argument list, Scala can infer its type from the previous argument list.
        */
        fun <A, B, C> zipWith(a: List<A>, b: List<B>, f: (A, B) -> C): List<C> = when {
            a is Nil -> Nil
            b is Nil -> Nil
            a is Cons && b is Cons -> Cons(f(a.head, b.head), zipWith(a.tail, b.tail, f))
            else -> throw Exception("Impossible")
        }

        /*
        There's nothing particularly bad about this implementation,
        except that it's somewhat monolithic and easy to get wrong.
        Where possible, we prefer to assemble functions like this using
        combinations of other functions. It makes the code more obviously
        correct and easier to read and understand. Notice that in this
        implementation we need special purpose logic to break out of our
        loops early. In Chapter 5 we'll discuss ways of composing functions
        like this from simpler components, without giving up the efficiency
        of having the resulting functions work in one pass over the data.

        It's good to specify some properties about these functions.
        For example, do you expect these expressions to be true?

        (xs append ys) startsWith xs
        xs startsWith Nil
        (xs append ys append zs) hasSubsequence ys
        xs hasSubsequence Nil

        */
        tailrec fun <A> startsWith(l: List<A>, prefix: List<A>): Boolean = when {
            prefix is Nil -> true
            l is Cons && prefix is Cons && l.head == prefix.head -> startsWith(l.tail, prefix.tail)
            else -> false
        }

        tailrec fun <A> hasSubsequence(sup: List<A>, sub: List<A>): Boolean = when {
            sub is Nil -> sub == Nil
            startsWith(sup, sub) -> true
            sub is Cons -> hasSubsequence(sub.tail, sub)
            else -> false
        }
    }
}

fun <T> H1<List.T, T>.toOri(): List<T> = this as List<T>

infix fun <A> A.cons(tail: List<A>): List<A> = List.Cons(this, tail)