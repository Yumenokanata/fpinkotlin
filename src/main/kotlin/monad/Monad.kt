package monad

import applicative.Applicative
import applicative.Traverse
import applicative.curry
import datastructures.List
import datastructures.List.Companion.foldLeft
import datastructures.List.Companion.foldRight
import datastructures.cons
import datastructures.toOri
import errorhanding.Either
import errorhanding.Option
import errorhanding.toOri
import fj.F
import fj.F2
import fj.Function
import fj.test.Gen
import fj.test.Property
import laziness.Stream
import laziness.toOri
import monoid.*

/**
 * Created by yume on 16-12-27.
 */

interface H1<out X, out T>

interface H2<out X, out Y, out T> : H1<H1<X, Y>, T>

class HTypeFG<F, G> {
    inner class HTypeFG<T>(val f: H1<F, H1<G, T>>) : H1<H1<F, G>, T>

    fun <T> narrow(value: H1<H1<F, G>, T>): HTypeFG<T> = value as HTypeFG<T>
}

class HTypePairFG<F, G> {
    inner class PairFG<T>(val f: H1<F, T>, val g: H1<G, T>) : H1<Pair<F, G>, T>

    fun <T> narrow(value: H1<Pair<F, G>, T>): PairFG<T> = value as PairFG<T>
}

interface Functor<F> {
    fun <A, B> map(fa: H1<F, A>, f: (A) -> B): H1<F, B>
}

interface Monad<F> : Applicative<F>, Functor<F> {
    override fun <A> unit(a: () -> A): H1<F, A>

    fun <A, B> flatMap(gha: H1<F, A>, f: (A) -> H1<F, B>): H1<F, B>

    override fun <A> unit(a: A): H1<F, A> = unit { a }

    override fun <A, B> map(fa: H1<F, A>, f: (A) -> B): H1<F, B> =
            flatMap(fa) { a -> unit { f(a) } }

    override fun <A, B, C> map2(fa: H1<F, A>, fb: H1<F, B>, f: (A, B) -> C): H1<F, C> =
            flatMap(fa) { a -> map(fb) { b -> f(a, b) } }

    override fun <A> sequence(lma: List<H1<F, A>>): H1<F, List<A>> =
            lma.foldLeft(unit { List.nil<A>() }, { mla, ma -> map2(mla, ma) { l, h -> h cons l } })

    override fun <A, B> traverse(la: List<A>, f: (A) -> H1<F, B>): H1<F, List<B>> =
            la.foldLeft(unit { List.nil<B>() }, { mla, ma -> map2(mla, f(ma)) { l, h -> h cons l } })

    override fun <A> replicateM(n: Int, ma: H1<F, A>): H1<F, List<A>> =
            flatMap(ma, { a -> unit { List.replicate(n, a) } })

    override fun <A, B> product(ma: H1<F, A>, mb: H1<F, B>): H1<F, Pair<A, B>> = map2(ma, mb) { a, b -> a to b }

    fun <A, B, C> compose(f: (A) -> H1<F, B>, g: (B) -> H1<F, C>): (A) -> H1<F, C> =
            { a -> flatMap(f(a), g) }

    fun <A> filterM(ms: List<A>, f: (A) -> H1<F, Boolean>): H1<F, List<A>> =
            ms.foldLeft(
                    unit { List.nil<A>() },
                    { la, a ->
                        compose(f, {
                            if (it)
                                map2(unit { a }, la) { h, l -> h cons l }
                            else
                                la
                        })(a) })

    //使用compose实现flatMap的练习，此时基本组合子为compose和unit
    fun <A, B> _flatMap(ma: H1<F, A>, f: (A) -> H1<F, B>): H1<F, B> =
            compose<Unit, A, B>({ ma }, f)(Unit)

    fun <A> join(mma: H1<F, H1<F, A>>): H1<F, A> = flatMap(mma, { it })

    //使用join和map实现flatMap的练习，此时基本组合子为join、map和unit
    fun <A, B> __flatMap(ma: H1<F, A>, f: (A) -> H1<F, B>): H1<F, B> = join(map(ma, f))

    fun <A> skip(a: H1<F, A>): H1<F, Unit> = as_(a, Unit)

    fun <A, B> as_(a: H1<F, A>, b: B): H1<F, B> = map(a) { b }

    fun <A> when_(b: Boolean, fa: () -> H1<F, A>): H1<F, Boolean> =
        if(b) as_(fa(), true) else unit(false)

    fun while_(ma: H1<F, Boolean>, b: H1<F, Unit>): H1<F, Unit> =
            flatMap(ma) { a -> skip(when_(a, { while_(ma, b) })) }

    //只有cond函数返回true，一直循环重复第一个参数的作用
    fun <A> doWhile(ma: H1<F, A>, cond: (A) -> H1<F, Boolean>): H1<F, Unit> =
            flatMap(ma) { a ->
                flatMap(cond(a)) { ok ->
                    if(ok)
                        doWhile(ma, cond)
                    else
                        unit(Unit)
                }
            }

    //无限重复参数的作用
    fun <A, B> forever(ma: H1<F, A>): H1<F, B> = flatMap(ma) { forever<A, B>(ma) }

    //使用函数f折叠流，组合作用并返回结果
    fun <A, B> foldM(l: Stream<A>, z: B, f: (B, A) -> H1<F, B>): H1<F, B> =
            when(l) {
                is Stream.Cons -> flatMap(f(z, l.h()), { z2 -> foldM(l.t(), z2, f) })
                else -> unit(z)
            }

    //同foldM一致，除了不返回结果
    fun <A, B> foldM_(l: Stream<A>, z: B, f: (B, A) -> H1<F, B>): H1<F, Unit> =
        skip(foldM(l, z, f))

    //对流中每个元素调用函数f并组合作用
    fun <A> foreachM(l: Stream<A>, f: (A) -> H1<F, Unit>): H1<F, Unit> =
            foldM_(l, Unit) { _, a -> f(a) }
}

// Monad composition
fun <G, H> composeM(g: Monad<G>, h: Monad<H>, t: Traverse<H>): Monad<H1<G, H>> {
    val htypeFG = HTypeFG<G, H>()
    return object : Monad<H1<G, H>> {
        override fun <A> unit(a: () -> A): H1<H1<G, H>, A> = htypeFG.HTypeFG(g.unit(h.unit(a)))

        override fun <A, B> flatMap(gha: H1<H1<G, H>, A>, f: (A) -> H1<H1<G, H>, B>): H1<H1<G, H>, B> =
                htypeFG.HTypeFG(
                        g.flatMap(htypeFG.narrow(gha).f,
                                { ha: H1<H, A> -> g.map(t.traverse(ha, { htypeFG.narrow(f(it)).f }, g), { h.join(it) }) }
                        )
                )
    }
}

//monad flatMap组合子的结合律(associative)法则
fun <M, A, B, C> monadFlatmapAssociativeLaw(gen: Gen<A>, fGen: Gen<F<A, B>>, gGen: Gen<F<B, C>>, m: Monad<M>): Property {
    val flatFGen: Gen<(A) -> H1<M, B>> = liftGenF(fGen, m)
    val flatGGen: Gen<(B) -> H1<M, C>> = liftGenF(gGen, m)
    val tGen = tripleGen(gen, flatFGen, flatGGen)
    return Property.property(tGen, { (a, f, g) -> Property.prop(
            m.flatMap(m.flatMap(m.unit(a), f), g) == m.flatMap(m.unit(a), { m.flatMap(f(it), g) })
    )})
}

fun <M, A, B> liftGenF(fGen: Gen<F<A, B>>, m: Monad<M>): Gen<(A) -> H1<M, B>> =
        fGen.map { f -> { a: A -> m.unit(f.f(a)) } }

//monad的单位元(identity)法则
fun <M, A, B> monadIdentityLaw(gen: Gen<A>, fGen: Gen<F<A, B>>, m: Monad<M>): Property {
    val flatFGen: Gen<(A) -> H1<M, B>> = liftGenF(fGen, m)
    val tGen = gen.bind { x -> flatFGen.map { y -> x to y } }
    return Property.property(tGen, { (a, f) ->
        val x = m.unit(a)
        Property.prop(
            m.flatMap(x, { m.unit(it) }) == x
            && m.flatMap(m.unit(a), f) == f(a)
    )})
}


//List Monad
val listMonad = object : Monad<List.T> {
    override fun <A> unit(a: () -> A): H1<List.T, A> =
            List.apply(a())

    override fun <A, B> flatMap(gha: H1<List.T, A>, f: (A) -> H1<List.T, B>): H1<List.T, B> =
            List.flatMap(gha.toOri()) { f(it).toOri() }
}

//Option Monad
val optionMonad = object : Monad<Option.T> {
    override fun <A> unit(a: () -> A): H1<Option.T, A> =
            Option.fromNull(a())

    override fun <A, B> flatMap(gha: H1<Option.T, A>, f: (A) -> H1<Option.T, B>): H1<Option.T, B> =
            gha.toOri().flatMap { f(it).toOri() }
}


//State Monad
data class State<S, A>(val run: (S) -> Pair<A, S>)

class StateMonads<S> {

    inner class MonadState<T>(val s: State<S, T>) : H1<StateU, T>

    object StateU

    fun <A> narrow(value: H1<StateU, A>): MonadState<A> {
        @Suppress("UNCHECKED_CAST")
        return value as MonadState<A>
    }

    val monad = object : Monad<StateU> {
        override fun <A> unit(a: () -> A): H1<StateU, A> = MonadState(State { a() to it })

        override fun <A, B> flatMap(gha: H1<StateU, A>, f: (A) -> H1<StateU, B>): H1<StateU, B> =
                MonadState(State { s ->
                    val (a, sa) = narrow(gha).s.run(s)
                    narrow(f(a)).s.run(sa)
                })
    }
}


//Reader Monad
data class Reader<R, A>(val run: (R) -> A)

class ReaderMonads<R> {
    inner class MonadReader<T>(val r: Reader<R, T>) : H1<ReaderU, T>

    object ReaderU

    fun <A> narrow(value: H1<ReaderU, A>): MonadReader<A> {
        @Suppress("UNCHECKED_CAST")
        return value as MonadReader<A>
    }

    val monad = object : Monad<ReaderU> {
        override fun <A> unit(a: () -> A): H1<ReaderU, A> = MonadReader(Reader { a() })

        override fun <A, B> flatMap(gha: H1<ReaderU, A>, f: (A) -> H1<ReaderU, B>): H1<ReaderU, B> =
                MonadReader(Reader { r ->
                    val a = narrow(gha).r.run(r)
                    narrow(f(a)).r.run(r)
                })
    }
}


//Either Monad
fun <E> eitherMonad(): Monad<H1<Either.T, E>> =
        object : Monad<H1<Either.T, E>> {
            override fun <A> unit(a: () -> A): H2<Either.T, E, A> = Either.Right(a())

            override fun <A, B> flatMap(gha: H1<H1<Either.T, E>, A>, f: (A) -> H1<H1<Either.T, E>, B>): H1<H1<Either.T, E>, B> {
                val either = gha.toOri()
                return when(either) {
                    is Either.Right -> f(either.get)
                    is Either.Left -> Either.Left(either.get)
                }
            }
        }


/**
 * foldRight、foldLeft、foldMap相互实现，所以具体实现时需要不依赖另外两个方法实现其中一个
 */
interface Foldable<F> {
    fun <A, B> foldRight(la: H1<F, A>, z: B, f: (A, B) -> B): B =
            foldMap(la, f.curry(), endoMonoid<B>())(z)

    fun <A, B> foldLeft(la: H1<F, A>, z: B, f: (B, A) -> B): B =
            foldMap(la, { a -> { b: B -> f(b, a) } }, dual(endoMonoid<B>()))(z)

    fun <A, B> foldMap(ls: H1<F, A>, f: (A) -> B, mb: Monoid<B>): B =
            foldLeft(ls, mb.zero(), { b, a -> mb.op(f(a), b) })

    fun <A> concatenate(ls: H1<F, A>, m: Monoid<A>): A =
            foldLeft(ls, m.zero(), m::op)

    fun <A> toList(fa: H1<F, A>): List<A> =
            foldLeft(fa, List.nil<A>(), { l, a -> a cons l })
}


val listFoldable = object : Foldable<List.T> {
    override fun <A, B> foldRight(la: H1<List.T, A>, z: B, f: (A, B) -> B): B =
            la.toOri().foldRight(z, f)

    override fun <A, B> foldLeft(la: H1<List.T, A>, z: B, f: (B, A) -> B): B =
            la.toOri().foldLeft(z, f)

    override fun <A, B> foldMap(ls: H1<List.T, A>, f: (A) -> B, mb: Monoid<B>): B =
            foldMapV(ls.toOri(), mb, f)

    override fun <A> toList(fa: H1<List.T, A>): List<A> = fa.toOri()
}


val streamFoldable = object : Foldable<Stream.T> {
    override fun <A, B> foldRight(la: H1<Stream.T, A>, z: B, f: (A, B) -> B): B =
            la.toOri().foldRight({ z }, { a, lb -> f(a, lb()) })

    override fun <A, B> foldLeft(la: H1<Stream.T, A>, z: B, f: (B, A) -> B): B =
            la.toOri().foldLeft(z, { b, a -> f(b, a) })
}


sealed class Tree<out A> {
    data class Leaf<A>(val value: A) : Tree<A>()
    data class Branch<A>(val left: Tree<A>, val right: Tree<A>) : Tree<A>()
}

data class HTypeTree<T>(val t: Tree<T>) : H1<TreeU, T>

object TreeU

fun <T> narrow(value: H1<TreeU, T>): HTypeTree<T> = value as HTypeTree<T>

val treeFoldable = object : Foldable<TreeU> {
    override fun <A, B> foldRight(la: H1<TreeU, A>, z: B, f: (A, B) -> B): B {
        val tree = narrow(la).t
        return when(tree) {
            is Tree.Leaf -> f(tree.value, z)
            is Tree.Branch -> foldRight(HTypeTree(tree.left), foldRight(HTypeTree(tree.right), z, f), f)
        }
    }

    override fun <A, B> foldLeft(la: H1<TreeU, A>, z: B, f: (B, A) -> B): B {
        val tree = narrow(la).t
        return when(tree) {
            is Tree.Leaf -> f(z, tree.value)
            is Tree.Branch -> foldLeft(HTypeTree(tree.left), foldLeft(HTypeTree(tree.right), z, f), f)
        }
    }

    override fun <A, B> foldMap(ls: H1<TreeU, A>, f: (A) -> B, mb: Monoid<B>): B {
        val tree = narrow(ls).t
        return when(tree) {
            is Tree.Leaf -> f(tree.value)
            is Tree.Branch -> mb.op(foldMap(HTypeTree(tree.left), f, mb), foldMap(HTypeTree(tree.right), f, mb))
        }
    }
}


val optionFoldable = object : Foldable<Option.T> {
    override fun <A, B> foldRight(la: H1<Option.T, A>, z: B, f: (A, B) -> B): B {
        val option = la.toOri()
        return when(option) {
            is Option.None -> z
            is Option.Some -> f(option.get, z)
        }
    }

    override fun <A, B> foldLeft(la: H1<Option.T, A>, z: B, f: (B, A) -> B): B {
        val option = la.toOri()
        return when(option) {
            is Option.None -> z
            is Option.Some -> f(z, option.get)
        }
    }

    override fun <A, B> foldMap(ls: H1<Option.T, A>, f: (A) -> B, mb: Monoid<B>): B {
        val option = ls.toOri()
        return when(option) {
            is Option.None -> mb.zero()
            is Option.Some -> f(option.get)
        }
    }
}


fun <A, B> Function1<A, B>.toF(): F<A, B> = object : F<A, B> {
    override fun f(a: A): B = this@toF(a)
}

fun <A, B> F<A, B>.toF(): (A) -> B = { f(it) }

