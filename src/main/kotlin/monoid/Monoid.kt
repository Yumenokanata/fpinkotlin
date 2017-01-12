package monoid

import fj.Show
import fj.test.Gen
import fj.test.Property
import datastructures.List
import datastructures.List.Companion.foldRight
import errorhanding.Option
import errorhanding.orSome
import fj.test.Arbitrary
import fj.test.CheckResult
import parallelism.Par
import parallelism.Parallel
import java.util.*
import kotlin.collections.HashMap

/**
 * Created by yume on 16-12-25.
 */
interface Monoid<A> {
    fun op(a1: A, a2: A): A
    fun zero(): A
}

val stringMonoid = object : Monoid<String> {
    override fun op(a1: String, a2: String): String = a1 + a2

    override fun zero(): String = ""
}

val intAddition = object : Monoid<Int> {
    override fun op(a1: Int, a2: Int): Int = a1 + a2

    override fun zero(): Int = 0
}

val intMultiplication = object : Monoid<Int> {
    override fun op(a1: Int, a2: Int): Int = a1 * a2

    override fun zero(): Int = 1
}

val booleanOr = object : Monoid<Boolean> {
    override fun op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override fun zero(): Boolean = false
}

val booleanAnd = object : Monoid<Boolean> {
    override fun op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override fun zero(): Boolean = true
}

fun <A> optionMonoid(): Monoid<A?> =
        object : Monoid<A?> {
            override fun op(a1: A?, a2: A?): A? = a1 ?: a2

            override fun zero(): A? = null
        }

fun <A> dual(m: Monoid<A>): Monoid<A> =
        object : Monoid<A> {
            override fun op(a1: A, a2: A): A = m.op(a2, a1)

            override fun zero(): A = m.zero()
        }

fun <A> endoMonoid(): Monoid<(A) -> A> =
        object : Monoid<(A) -> A> {
            override fun op(a1: (A) -> A, a2: (A) -> A): (A) -> A = { a2(a1(it)) }

            override fun zero(): (A) -> A = { it }
        }

fun <A, B, C> tripleGen(aGen: Gen<A>, bGen: Gen<B>, cGen: Gen<C>): Gen<Triple<A, B, C>> =
        aGen.bind { x -> bGen.bind { y -> cGen.map { z -> Triple(x, y, z) } } }

fun <A> tripleGen(gen: Gen<A>): Gen<Triple<A, A, A>> =
        gen.bind { x -> gen.bind { y -> gen.map { z -> Triple(x, y, z) } } }

//monoid的结合律(associative)法则
fun <A> monoidLaws(m: Monoid<A>, gen: Gen<A>): Property =
        Property.property(
                tripleGen(gen),
                { (x, y, z) -> Property.prop(m.op(m.op(x, y), z) == m.op(x, m.op(y, z))) })

fun <A, B> foldMap(ls: List<A>, m: Monoid<B>, f: (A) -> B): B =
        ls.map { f(it) }.foldRight(m.zero(), m::op)

fun <A, B> foldRight(ls: List<A>, z: B, f: (A, B) -> B): B =
    foldMap(ls, endoMonoid<B>(), { a -> { b -> f(a, b) } })(z)

fun <A, B> foldLeft(ls: List<A>, z: B, f: (B, A) -> B): B =
        foldMap(ls, dual(endoMonoid<B>()), { a -> { b -> f(b, a) } })(z)

fun <A, B> foldMapV(ls: List<A>, m: Monoid<B>, f: (A) -> B): B =
        when {
            ls.isEmpty -> m.zero()
            ls.isSingle -> f(ls.head)
            else -> {
                val p = ls.splitAt(ls.length() / 2)
                m.op(foldMapV(p.first, m, f), foldMapV(p.second, m, f))
            }
        }

fun <A> par(m: Monoid<A>): Monoid<Par<A>> =
        object : Monoid<Par<A>> {
            override fun op(a1: Par<A>, a2: Par<A>): Par<A> = Parallel.map2(a1, a2, m::op)

            override fun zero(): Par<A> = Parallel.unit(m.zero())
        }

fun <A, B> parFoldMap(v: List<A>, m: Monoid<B>, f: (A) -> B): Par<B> =
        foldMapV(v, par(m), { Parallel.unit(f(it)) })

fun ordered(ints: List<Int>): Boolean {
    val mon = object : Monoid<Option<Triple<Int, Int, Boolean>>> {
        override fun op(a1: Option<Triple<Int, Int, Boolean>>, a2: Option<Triple<Int, Int, Boolean>>): Option<Triple<Int, Int, Boolean>> =
            when {
                a1 is Option.Some && a2 is Option.Some ->
                    Option.Some(Triple(
                            Math.min(a1.get.first, a2.get.first),
                            Math.max(a1.get.second, a2.get.second),
                            a1.get.third && a2.get.third && a1.get.second <= a2.get.first
                    ))
                a1 is Option.Some -> a1
                else -> a2
            }

        override fun zero(): Option<Triple<Int, Int, Boolean>> = Option.none()
    }

    return foldMapV(ints, mon, { i -> Option.Some(Triple(i, i, true)) }).map { it.third }.orSome { false }
}

sealed class WC {
    data class Stub(val chars: String) : WC()
    data class Part(val lStub: String, val word: Int, val rStub: String) : WC()
}

val wcMonoid = object : Monoid<WC> {
    override fun op(a1: WC, a2: WC): WC =
        when {
            a1 is WC.Part && a2 is WC.Part ->
                WC.Part(a1.lStub,
                        a1.word + a2.word +
                        if((a1.rStub + a2.lStub).isNullOrEmpty()) 0 else 1,
                        a2.rStub)
            a1 is WC.Part && a2 is WC.Stub -> a1.copy(rStub = a1.rStub + a2.chars)
            a1 is WC.Stub && a2 is WC.Part -> a2.copy(lStub = a1.chars + a2.lStub)
            a1 is WC.Stub && a2 is WC.Stub -> WC.Stub(a1.chars + a2.chars)
            else -> zero()
        }

    override fun zero(): WC = WC.Stub("")
}

fun wordCount(s: String): Int {
    fun wc(c: Char): WC =
            if (c.isWhitespace())
                WC.Part("", 0, "")
            else
                WC.Stub(c.toString())

    fun unstub(s: String) = Math.min(s.length, 1)

    val wc = foldMapV(List.fromString(s), wcMonoid, { wc(it) })

    return when(wc) {
        is WC.Stub -> unstub(wc.chars)
        is WC.Part -> unstub(wc.lStub) + wc.word + unstub(wc.rStub)
    }
}

fun wordCountLaw(): Property {
    val stringGen: Gen<Pair<String, Int>> = Arbitrary.arbList(Arbitrary.arbAlphaNumString)
            .map { it.filter { i -> !i.isNullOrBlank() } }
            .map { Show.streamShow(Show.stringShow, "", " ", "").showS(it.toStream()) to it.length() }

    return Property.property(stringGen, { (s, n) -> Property.prop(wordCount(s) == n) })
}

fun <K, V> mapMergeMonoid(v: Monoid<V>): Monoid<Map<K, V>> =
        object : Monoid<Map<K, V>> {
            override fun op(a1: Map<K, V>, a2: Map<K, V>): Map<K, V> =
                    (a1.keys + a2.keys).fold(zero()) { map, k ->
                        map.plus(k to v.op(a1[k] ?: v.zero(), a2[k] ?: v.zero()))
                    }

            override fun zero(): Map<K, V> = HashMap()
        }

fun <A, B> productMonoid(a: Monoid<A>, b: Monoid<B>): Monoid<Pair<A, B>> =
        object : Monoid<Pair<A, B>> {
            override fun op(a1: Pair<A, B>, a2: Pair<A, B>): Pair<A, B> =
                a.op(a1.first, a2.first) to b.op(a1.second, a2.second)

            override fun zero(): Pair<A, B> = a.zero() to b.zero()
        }

fun <A, B> functionMonoid(b: Monoid<B>): Monoid<(A) -> B> =
        object : Monoid<(A) -> B> {
            override fun op(a1: (A) -> B, a2: (A) -> B): (A) -> B =
                    { a ->
                        val b1 = a1(a)
                        val b2 = a2(a)
                        b.op(b1, b2)
                    }

            override fun zero(): (A) -> B = { b.zero() }
        }

fun <A> bag(ls: List<A>): Map<A, Int> {
    val mon: Monoid<Map<A, Int>> = mapMergeMonoid(intAddition)

    fun unit(a: A, n: Int): Map<A, Int> = Collections.singletonMap(a, n)

    return foldMapV(ls, mon, { unit(it, 1) })
}

