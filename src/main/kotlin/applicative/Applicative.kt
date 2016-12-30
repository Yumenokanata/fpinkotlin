package applicative

import fj.F

import fj.data.List
import fj.data.Option
import fj.data.Stream
import fj.test.Gen
import fj.test.Property
import monad.*
import java.util.*

/**
 * Created by yume on 16-12-28.
 */

fun <A, B, C> Function2<A, B, C>.curry(): (A) -> (B) -> C = { a -> { b -> this(a, b)} }

/**
 * 应用函子
 *
 * 法则：
 * 1. Left and right identity:
 *    map2(unit(Unit), fa) { _, a -> a } == fa
 *    map2(fa, unit(Unit)) { a, _ -> a } == fa
 * 2. Associativity(结合律):
 *    product(product(fa, fb), fc) == map(product(fa, product(fb, fc))) { assoc(it) }
 * 3. Naturality of product:
 *    map2(a, b, productF(f, g)) == product(map(a, f), map(b, g))
 *
 */
interface Applicative<F> : Functor<F> {
    fun <A, B, C> map2(fa: H1<F, A>, fb: H1<F, B>, f: (A, B) -> C) : H1<F, C>

    fun <A> unit(a: () -> A): H1<F, A>

    fun <A, B> apply(fab: H1<F, (A) -> B>, fa: H1<F, A>): H1<F, B> =
            map2(fab, fa) { f, a -> f(a) }


    fun <A> unit(a: A): H1<F, A> = unit { a }

    override fun <A, B> map(fa: H1<F, A>, f: (A) -> B): H1<F, B> =
            map2(fa, unit(Unit), { a, _ -> f(a) })

    //使用apply和unit实现map的练习，此时基本组合子为apply和unit
    fun <A, B> _map(fa: H1<F, A>, f: (A) -> B): H1<F, B> =
            apply(unit(f), fa)

    //使用apply和unit实现map2的练习，此时基本组合子为apply和unit
    fun <A, B, C> _map2(fa: H1<F, A>, fb: H1<F, B>, f: (A, B) -> C) : H1<F, C> =
            apply(apply(unit(f.curry()), fa), fb)

    fun <A, B> traverse(la: List<A>, f: (A) -> H1<F, B>): H1<F, List<B>> =
            la.foldLeft({ mlb, a -> map2(f(a), mlb) { h, l -> h cons l } }, unit(List.nil<B>()))

    fun <A> sequence(fas: List<H1<F, A>>): H1<F, List<A>> =
            fas.foldLeft({ mla, ma -> map2(ma, mla) { h, l -> h cons l } }, unit(List.nil<A>()))

    fun <A> replicateM(n: Int, fa: H1<F, A>): H1<F, List<A>> =
            map(fa, { a -> List.replicate(n, a) })

    fun <A, B> product(fa: H1<F, A>, fb: H1<F, B>): H1<F, Pair<A, B>> =
            map2(fa, fb) { a, b -> a to b }


    /**
     * Scala:
     * def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
     *   val self = this
     *   new Applicative[({type f[x] = (F[x], G[x])})#f] {
     *     def unit[A](a: => A) = (self.unit(a), G.unit(a))
     *     override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
     *       (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
     *   }
     * }
     */
    fun <G> product(g: Applicative<G>): Applicative<Pair<F, G>> {
        typealias FU = Pair<F, G>

        data class FApply<T>(val f: H1<F, T>, val g: H1<G, T>) : H1<Pair<F, G>, T>

        @Suppress("UNCHECKED_CAST")
        fun <T> narrow(value: H1<FU, T>): FApply<T> = value as FApply<T>

        return object : Applicative<FU> {
            override fun <A> unit(a: () -> A): H1<FU, A> =
                    FApply(this@Applicative.unit(a), g.unit(a))

            override fun <A, B> apply(fab: H1<FU, (A) -> B>, fa: H1<FU, A>): H1<FU, B> =
                    FApply(this@Applicative.apply(narrow(fab).f, narrow(fa).f),
                            g.apply(narrow(fab).g, narrow(fa).g))

            override fun <A, B, C> map2(fa: H1<FU, A>, fb: H1<FU, B>, f: (A, B) -> C): H1<FU, C> =
                    apply(map<A, (B) -> C>(fa) { a -> { b -> f(a, b) } }, fb)
        }
    }

    /**
     * Here we simply use `map2` to lift `apply` and `unit` themselves from one
     * Applicative into the other.
     * If `self` and `G` both satisfy the laws, then so does the composite.
     * The full proof can be found at
     * https://github.com/runarorama/sannanir/blob/master/Applicative.v
     *
     * Scala:
     * def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
     *   val self = this
     *   new Applicative[({type f[x] = F[G[x]]})#f] {
     *     def unit[A](a: => A) = self.unit(G.unit(a))
     *     override def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C) =
     *       self.map2(fga, fgb)(G.map2(_,_)(f))
     *   }
     * }
     */
    fun <G> compose(g: Applicative<G>): Applicative<H1<F, G>> {
        typealias FCU = H1<F, G>

        data class FCApply<T>(val f: H1<F, H1<G, T>>) : H1<H1<F, G>, T>

        @Suppress("UNCHECKED_CAST")
        fun <T> narrow(value: H1<FCU, T>): FCApply<T> = value as FCApply<T>

        return object : Applicative<FCU> {
            override fun <A> unit(a: () -> A): H1<FCU, A> =
                    FCApply(this@Applicative.unit(g.unit(a)))

            override fun <A, B, C> map2(fa: H1<FCU, A>, fb: H1<FCU, B>, f: (A, B) -> C): H1<FCU, C> =
                    FCApply(this@Applicative.map2(narrow(fa).f, narrow(fb).f) { ga, gb -> g.map2(ga, gb, f) })
        }
    }
}

fun <A, B, C> assoc(p: Pair<A, Pair<B, C>>): Pair<Pair<A, B>, C> = (p.first to p.second.first) to p.second.second

fun <I, O, I2, O2> productF(f: (I) -> O, g: (I2) -> O2): (I, I2) -> Pair<O, O2> = { i, i2 -> f(i) to g(i2) }

//应用函子的Naturality法则
fun <A, I, O, I2, O2> naturalityLaw(aGen: Gen<I>,
                                    bGen: Gen<I2>,
                                    fGen: Gen<F<I, O>>,
                                    gGen: Gen<F<I2, O2>>,
                                    apply: Applicative<A>): Property {
    val tGen = aGen.bind { a -> bGen.bind { b -> fGen.bind { f -> gGen.map { g -> Triple(a to b, f, g) } } } }
    return Property.property(tGen, { t ->
        val ma = apply.unit(t.first.first)
        val mb = apply.unit(t.first.second)
        val f = t.second
        val g = t.third
        Property.prop(
        apply.run {
            map2(ma, mb, { i, j -> productF(f.toF(), g.toF())(i, j) }) ==
                    product(map(ma, f.toF()), map(mb, g.toF()))
        }
    ) })
}


data class StreamApplicative<T>(val s: Stream<T>) : H1<StreamU, T>

object StreamU

fun <T> narrow(value: H1<StreamU, T>): StreamApplicative<T> = value as StreamApplicative<T>

val streamApplicative = object : Applicative<StreamU> {
    override fun <A, B, C> map2(fa: H1<StreamU, A>, fb: H1<StreamU, B>, f: (A, B) -> C): H1<StreamU, C> =
            StreamApplicative(narrow(fa).s.zip(narrow(fb).s).map { p -> f(p._1(), p._2()) })

    override fun <A> unit(a: () -> A): H1<StreamU, A> =
            StreamApplicative(Stream.repeat(a()))
}


sealed class Validation<out E, out A> {
    data class Failure<E>(val head: E, val tail: Vector<E> = Vector()) : Validation<E, Nothing>()
    data class Success<A>(val a: A) : Validation<Nothing, A>()
}

class ValidaApplicative<E> {
    inner class ApplyValida<R>(val r: Validation<E, R>) : H1<ValidaU, R>

    object ValidaU

    fun <A> narrow(value: H1<ValidaU, A>): ApplyValida<A> {
        @Suppress("UNCHECKED_CAST")
        return value as ApplyValida<A>
    }

    val applicative = object : Applicative<ValidaU> {
        override fun <A, B, C> map2(fa: H1<ValidaU, A>, fb: H1<ValidaU, B>, f: (A, B) -> C): H1<ValidaU, C> {
            val va = narrow(fa).r
            val vb = narrow(fb).r
            return ApplyValida(when {
                va is Validation.Success && vb is Validation.Success -> Validation.Success(f(va.a, vb.a))
                va is Validation.Failure && vb is Validation.Success -> va
                va is Validation.Success && vb is Validation.Failure -> vb
                va is Validation.Failure && vb is Validation.Failure ->
                    Validation.Failure(va.head, Vector(va.tail + arrayListOf(vb.head) + vb.tail))
                else -> throw RuntimeException()
            })
        }

        override fun <A> unit(a: () -> A): H1<ValidaU, A> = ApplyValida(Validation.Success(a()))
    }
}

/**
 * 可遍历函子
 *
 * traverse、sequence、map相互实现，所以具体实现时需要不依赖另外两个方法实现其中一个
 *
 */
interface Traverse<F> {
    fun <T, App: Applicative<T>, A, B> traverse(fa: H1<F, A>, f: (A) -> H1<T, B>, apply: App): H1<T, H1<F, B>> =
            sequence(map(fa, f), apply)

    fun <G, App: Applicative<G>, A> sequence(fga: H1<F, H1<G, A>>, apply: App): H1<G, H1<F, A>> =
            traverse(fga, { it }, apply)


    data class HTypeId<A>(val a: A) : H1<IdU, A>

    object IdU

    fun <A> narrow(value: H1<IdU, A>): HTypeId<A> = value as HTypeId<A>

    val idMonad: Monad<IdU>
        get() = object : Monad<IdU> {
            override fun <A> unit(a: () -> A): H1<IdU, A> = HTypeId(a())

            override fun <A, B> flatMap(fa: H1<IdU, A>, f: (A) -> H1<IdU, B>): H1<IdU, B> =
                    f(narrow(fa).a)
        }

    fun <A, B> map(fa: H1<F, A>, f: (A) -> B): H1<F, B> =
            narrow(traverse(fa, { a -> idMonad.unit(f(a)) }, idMonad)).a
}

val listTraverse = object : Traverse<ListU> {
    override fun <T, App : Applicative<T>, A, B> traverse(fa: H1<ListU, A>, f: (A) -> H1<T, B>, apply: App): H1<T, H1<ListU, B>> =
            apply.map(narrow(fa).l.foldRight({ a, fbs -> apply.map2(f(a), fbs, { h, l -> h cons l }) }, apply.unit(List.nil<B>())))
            { HTypeList(it) }
}

val optionTraverse = object : Traverse<OptionU> {
    override fun <T, App : Applicative<T>, A, B> traverse(fa: H1<OptionU, A>, f: (A) -> H1<T, B>, apply: App): H1<T, H1<OptionU, B>> {
        val option = narrow(fa).l
        return when {
            option.isSome -> apply.map(f(option.some()), { HTypeOption(Option.some(it)) })
            option.isNone -> apply.unit(HTypeOption(Option.none<B>()))
            else -> apply.unit(HTypeOption(Option.none<B>()))
        }
    }
}

data class ListTree<A>(val head: A, val tail: List<ListTree<A>>)

data class HTypeListTree<T>(val t: ListTree<T>) : H1<ListTreeU, T> {
    constructor(h: T, l: H1<ListU, H1<ListTreeU, T>>): this(ListTree(h, narrow(l).l.map { it.toOri() }))
}

object ListTreeU

fun <A> narrow(value: H1<ListTreeU, A>): HTypeListTree<A> = value as HTypeListTree<A>

fun <A> ListTree<A>.toHType(): H1<ListTreeU, A> = HTypeListTree(this)

fun <A> H1<ListTreeU, A>.toOri(): ListTree<A> = narrow(this).t

val treeTraverse = object : Traverse<ListTreeU> {
    override fun <T, App : Applicative<T>, A, B> traverse(fa: H1<ListTreeU, A>, f: (A) -> H1<T, B>, ap: App): H1<T, H1<ListTreeU, B>> {
        val tree = fa.toOri()
        val tail: H1<ListU, H1<ListTreeU, A>> = HTypeList(tree.tail.map { it.toHType() })
        return ap.map2(f(tree.head), listTraverse.traverse(tail, { a -> traverse(a, f, ap) }, ap))
        { h, l -> HTypeListTree(h, l) }
    }
}
