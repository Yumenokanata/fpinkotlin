package applicative

import fj.F

import datastructures.List
import datastructures.List.Companion.foldLeft
import fj.test.Gen
import fj.test.Property
import monad.*
import datastructures.cons
import datastructures.toOri
import errorhanding.Option
import laziness.Stream
import monoid.Monoid
import java.util.*

/**
 * Created by yume on 16-12-28.
 */

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
            la.foldLeft(unit(List.nil<B>()), { mlb, a -> map2(f(a), mlb) { h, l -> h cons l } })

    fun <A> sequence(fas: List<H1<F, A>>): H1<F, List<A>> =
            fas.foldLeft(unit(List.nil<A>()), { mla, ma -> map2(ma, mla) { h, l -> h cons l } })

    fun <A> replicateM(n: Int, fa: H1<F, A>): H1<F, List<A>> =
            map(fa, { a -> List.replicate(n, a) })

    fun <A, B> product(fa: H1<F, A>, fb: H1<F, B>): H1<F, Pair<A, B>> =
            map2(fa, fb) { a, b -> a to b }


    /**
     * 应用函子组合子，用于组合不同的应用函子
     *
     * Scala:
     * def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
     *   val self = this
     *   new Applicative[({type f[x] = (F[x], G[x])})#f] {
     *     def unit[A](a: => A) = (self.unit(a), G.unit(a))
     *     override def fromList[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
     *       (self.fromList(fs._1)(p._1), G.fromList(fs._2)(p._2))
     *   }
     * }
     */
    fun <G> product(g: Applicative<G>): Applicative<Pair<F, G>> {
        val pairType = HTypePairFG<F, G>()

        return object : Applicative<Pair<F, G>> {
            override fun <A> unit(a: () -> A): H1<Pair<F, G>, A> =
                    pairType.PairFG(this@Applicative.unit(a), g.unit(a))

            override fun <A, B> apply(fab: H1<Pair<F, G>, (A) -> B>, fa: H1<Pair<F, G>, A>): H1<Pair<F, G>, B> =
                    pairType.PairFG(this@Applicative.apply(pairType.narrow(fab).f, pairType.narrow(fa).f),
                            g.apply(pairType.narrow(fab).g, pairType.narrow(fa).g))

            override fun <A, B, C> map2(fa: H1<Pair<F, G>, A>, fb: H1<Pair<F, G>, B>, f: (A, B) -> C): H1<Pair<F, G>, C> =
                    apply(map<A, (B) -> C>(fa) { a -> { b -> f(a, b) } }, fb)
        }
    }

    /**
     * 应用函子组合子，用于组合不同的应用函子
     *
     * Here we simply use `map2` to lift `fromList` and `unit` themselves from one
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
        val FGtype = HTypeFG<F,G>()

        return object : Applicative<H1<F, G>> {
            override fun <A> unit(a: () -> A): H1<H1<F, G>, A> =
                    FGtype.HTypeFG(this@Applicative.unit(g.unit(a)))

            override fun <A, B, C> map2(fa: H1<H1<F, G>, A>, fb: H1<H1<F, G>, B>, f: (A, B) -> C): H1<H1<F, G>, C> =
                    FGtype.HTypeFG(this@Applicative.map2(FGtype.narrow(fa).f, FGtype.narrow(fb).f) { ga, gb -> g.map2(ga, gb, f) })
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

//Stream的应用函子
data class StreamApplicative<T>(val s: Stream<T>) : H1<StreamU, T>

object StreamU

fun <T> narrow(value: H1<StreamU, T>): StreamApplicative<T> = value as StreamApplicative<T>

val streamApplicative = object : Applicative<StreamU> {
    override fun <A, B, C> map2(fa: H1<StreamU, A>, fb: H1<StreamU, B>, f: (A, B) -> C): H1<StreamU, C> =
            StreamApplicative(narrow(fa).s.zip(narrow(fb).s).map { p -> f(p.first, p.second) })

    override fun <A> unit(a: () -> A): H1<StreamU, A> =
            StreamApplicative(Stream.repeat(a()))
}


sealed class Validation<out E, out A> {
    data class Failure<E>(val head: E, val tail: Vector<E> = Vector()) : Validation<E, Nothing>()
    data class Success<A>(val a: A) : Validation<Nothing, A>()
}

/**
 * Validation的Applicative实例可以累计失败时的错误，失败的情况下，至少会有一个error存在于列表的head，其余error累加在列表的tail。
 *
 * eg：
 *
 * data class WebForm(val name: String, val birthdate: Date, val phoneNumber: String)
 *
 * fun validName(name: String): Validation<String, String> =
 *         if (name != "") Success(name)
 *         else Failure("Name cannot be empty")
 *
 * fun validBirthdate(birthdate: String): Validation<String, Date> =
 *         try {
 *             Success((SimpleDateFormat("yyyy-MM-dd").parse(birthdate)))
 *         } catch (e: Exception) {
 *             Failure("Birthdate must be in the form yyyy-MM-dd")
 *         }
 *
 * fun validPhone(phoneNumber: String): Validation<String, String> =
 *         if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
 *         else Failure("Phone number must be 10 digits")
 *
 * fun validWebForm(name: String,
 *                  birthdate: String,
 *                  phone: String): Validation<String, String> =
 *         map3(validName(name),
 *              validBirthdate(birthdate),
 *              validPhone(phone))
 *              { n, b, p -> WebForm(n, b, p) }
 */
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
 * 范化的traverse操作就像fold一样接收一些数据结构并按顺序作用函数以制造一个结果，不同的是traverse保存了原始的结构，
 * 而foldMap丢弃了结构并以一个monoid操作的结果替代。比如：
 *
 * * List<Option<A>> -> Option<List<A>>: (使用Option作为Applicative对Traverse<List>.sequence调用)假如任何一个输入链表
 *   的元素是None，则结果返回None，否则，将返回包裹在Some里面的原始链表
 *
 * * ListTree<Option<A>> -> Option<ListTree<A>>: (使用Option作为Applicative对Traverse<ListTree>.sequence调用)假如任
 *   何一个输入树的元素是None，则结果返回None，否则，将返回包裹在Some里面的原始树
 *
 * * Map<K, Par<A>> -> Par<Map<K, A>>: (使用Par作为Applicative对Traverse<Map<K, _>>.sequence调用)产生一个并行计算去并
 *   行计算map里的所有元素
 */
interface Traverse<F> : Functor<F>, Foldable<F> {
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

            override fun <A, B> flatMap(gha: H1<IdU, A>, f: (A) -> H1<IdU, B>): H1<IdU, B> =
                    f(narrow(gha).a)
        }

    /**
     * 使用traverse可以实现map，这表明Traverse是一个函子的扩展，traverse函数是一个泛化的map
     * (正因如此，有时称它为可遍历函子)
     */
    override fun <A, B> map(fa: H1<F, A>, f: (A) -> B): H1<F, B> =
            narrow(traverse(fa, { a -> idMonad.unit(f(a)) }, idMonad)).a

    /**
     * Traverse可以继承并实现Foldable和Functor，但注意，Foldable是不可能继承函子（Functor）的
     * 问题：为什么Foldable不可能继承函子？可以想出一个不是函子的Foldable吗？
     */
    override fun <A, B> foldMap(ls: H1<F, A>, f: (A) -> B, mb: Monoid<B>): B =
            narrow(
                    traverse<ConstU<B>, Applicative<ConstU<B>>, A, Nothing>(ls, { a -> HTypeConst(f(a)) }, monoidApplicative(mb))
            ).a

    fun <S, A, B> traverseS(fa: H1<F, A>, f: (A) -> State<S, B>): State<S, H1<F, B>> {
        val stateMonad = StateMonads<S>()
        val state = traverse(fa, { stateMonad.MonadState(f(it)) }, stateMonad.monad)
        return stateMonad.narrow(state).s
    }

    fun <A> zipWithIndex_(ta: H1<F, A>): H1<F, Pair<A, Int>> =
        traverseS<Int, A, Pair<A, Int>>(ta) { a ->
            State { s -> (a to s) to s + 1 }
        }.run(0).first

    fun <A> toList_(fa: H1<F, A>): List<A> =
            traverseS<List<A>, A, A>(fa) { a ->
                State { s -> a to (a cons s) }
            }.run(List.nil()).second.reverse()

    //带状态的遍历
    fun <S, A, B> mapAccum(fa: H1<F, A>, s: S, f: (A, S) -> Pair<B, S>): Pair<H1<F, B>, S> =
            traverseS<S, A, B>(fa) { a ->
                State { s ->
                    val (b, s2) = f(a, s)
                    b to s2
                }
            }.run(s)

    fun <A> zipWithIndex(ta: H1<F, A>): H1<F, Pair<A, Int>> =
            mapAccum(ta, 0, { a, s -> (a to s) to s + 1 }).first

    override fun <A> toList(fa: H1<F, A>): List<A> =
            mapAccum<List<A>, A, A>(fa, List.nil(), { a, s -> a to (a cons s) }).second.reverse()

    fun <A> reverse(fa: H1<F, A>): H1<F, A> =
            mapAccum<List<A>, A, A>(fa, toList(fa).reverse(), { _, s -> (s.head to s.tail) }).first

    override fun <A, B> foldLeft(la: H1<F, A>, z: B, f: (B, A) -> B): B =
            mapAccum(la, z, { a, b -> f(b, a) to b }).second

    fun <A : B, B> sum(la: H1<F, A>, num: Monoid<B>): B = foldLeft(la, num.zero(), num::op)

    // 组合可遍历结构
    fun <A, B> zip(fa: H1<F, A>, fb: H1<F, B>): H1<F, Pair<A, B>> =
            mapAccum(fa, toList(fb)) { a, s ->
                when {
                    s.isNotEmpty -> (a to s.head) to s.tail
                    else -> throw java.lang.RuntimeException("zip: Incompatible shapes.")
                }
            }.first

    fun <A, B> zipL(fa: H1<F, A>, fb: H1<F, B>): H1<F, Pair<A, Option<B>>> =
            mapAccum(fa, toList(fb)) { a, s ->
                when {
                    s.isNotEmpty -> (a to Option.Some(s.head)) to s.tail
                    else -> (a to Option.none<B>()) to List.nil<B>()
                }
            }.first

    fun <A, B> zipR(fa: H1<F, A>, fb: H1<F, B>): H1<F, Pair<Option<A>, B>> =
            mapAccum(fb, toList(fa)) { b, s ->
                when {
                    s.isNotEmpty -> (Option.Some(s.head) to b) to s.tail
                    else -> (Option.none<A>() to b) to List.nil<A>()
                }
            }.first

    /**
     * 遍历融合
     * 使用可应用函子product实现两个遍历的融合。这个函数将给出两个函数f和g，只遍历一次，收集两个函数的结果
     */
    fun <G, H, A, B> fuse(fa: H1<F, A>, f: (A) -> H1<G, B>, g: (A) -> H1<H, B>,
                          ga: Applicative<G>, ha: Applicative<H>):
            Pair<H1<G, H1<F, B>>, H1<H, H1<F, B>>> {
        data class HTypePair<T>(val g: H1<G, T>, val h: H1<H,T>) : H1<Pair<G, H>, T>

        @Suppress("UNCHECKED_CAST")
        fun <T> narrow(value: H1<Pair<G, H>, T>): HTypePair<T> = value as HTypePair<T>

        val pair = narrow(traverse(fa, { a -> HTypePair(f(a), g(a)) }, ga.product(ha)))
        return pair.g to pair.h
    }

    fun <G> compose(g: Traverse<G>): Traverse<H1<F, G>> {
        val FGtype = HTypeFG<F,G>()

        return object : Traverse<H1<F, G>> {
            override fun <T, App : Applicative<T>, A, B> traverse(
                    fa: H1<H1<F, G>, A>,
                    f: (A) -> H1<T, B>,
                    apply: App): H1<T, H1<H1<F, G>, B>> =
                    apply.map(this@Traverse.traverse(FGtype.narrow(fa).f, { ga: H1<G, A> -> g.traverse(ga, f, apply) }, apply))
                    { FGtype.HTypeFG(it) }
        }
    }
}

data class HTypeConst<A, B>(val a: A) : H1<ConstU<A>, B>

class ConstU<A>

fun <A, B> narrow(value: H1<ConstU<A>, B>): HTypeConst<A, B> = value as HTypeConst<A, B>

//将一个Monoid转变成一个Applicative
fun <M> monoidApplicative(m: Monoid<M>) =
        object : Applicative<ConstU<M>> {
            override fun <A> unit(a: () -> A): H1<ConstU<M>, A> = HTypeConst(m.zero())

            override fun <A, B, C> map2(fa: H1<ConstU<M>, A>, fb: H1<ConstU<M>, B>, f: (A, B) -> C): H1<ConstU<M>, C> =
                    HTypeConst(m.op(narrow(fa).a, narrow(fb).a))
        }

//List的可遍历函子
val listTraverse = object : Traverse<List.T> {
    override fun <T, App : Applicative<T>, A, B> traverse(fa: H1<List.T, A>, f: (A) -> H1<T, B>, apply: App): H1<T, H1<List.T, B>> =
            foldRight(fa, apply.unit(List.nil<B>()), { a, fbs -> apply.map2(f(a), fbs, { h, l -> h cons l }) })
}

//Option的可遍历函子
val optionTraverse = object : Traverse<Option.T> {
    override fun <T, App : Applicative<T>, A, B> traverse(fa: H1<Option.T, A>, f: (A) -> H1<T, B>, apply: App): H1<T, H1<Option.T, B>> =
            when(fa) {
                is Option.Some -> apply.map(f(fa.get), { Option.Some(it) })
                is Option.None -> apply.unit(Option.none<B>())
                else -> apply.unit(Option.none<B>())
            }
}

//ListTree的可遍历函子
data class ListTree<A>(val head: A, val tail: List<ListTree<A>>)

data class HTypeListTree<T>(val t: ListTree<T>) : H1<ListTreeU, T> {
    constructor(h: T, l: H1<List.T, H1<ListTreeU, T>>): this(ListTree(h, l.toOri().map { it.toOri() }))
}

object ListTreeU

fun <A> narrow(value: H1<ListTreeU, A>): HTypeListTree<A> = value as HTypeListTree<A>

fun <A> ListTree<A>.toHType(): H1<ListTreeU, A> = HTypeListTree(this)

fun <A> H1<ListTreeU, A>.toOri(): ListTree<A> = narrow(this).t

val treeTraverse = object : Traverse<ListTreeU> {
    override fun <T, App : Applicative<T>, A, B> traverse(fa: H1<ListTreeU, A>, f: (A) -> H1<T, B>, ap: App): H1<T, H1<ListTreeU, B>> {
        val tree = fa.toOri()
        val tail: H1<List.T, H1<ListTreeU, A>> = tree.tail.map { it.toHType() }
        return ap.map2(f(tree.head), listTraverse.traverse(tail, { a -> traverse(a, f, ap) }, ap))
        { h, l -> HTypeListTree(h, l) }
    }
}

//Utils
fun <A, B, C> Function2<A, B, C>.curry(): (A) -> (B) -> C = { a -> { b -> this(a, b)} }
