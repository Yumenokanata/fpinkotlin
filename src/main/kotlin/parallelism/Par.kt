package parallelism

import fj.F2
import fj.Monoid
import fj.Show

import fj.data.List
import fj.test.Arbitrary
import fj.test.Rand
import java.util.concurrent.*

/**
 * Created by yume on 16-12-25.
 */

typealias Par<A> = (ExecutorService) -> Future<A>

object Parallel {
    fun <A> run(es: ExecutorService, a: Par<A>): Future<A> = a(es)

    fun <A> unit(a: A): Par<A> = { UnitFuture(a) }

    private data class UnitFuture<A>(val a: A) : Future<A> {
        override fun isDone() = true

        override fun get(timeout: Long, unit: TimeUnit): A = a

        override fun get(): A = a

        override fun isCancelled(): Boolean = false

        override fun cancel(mayInterruptIfRunning: Boolean): Boolean = false
    }

    fun <A, B, C> map2(a: Par<A>, b: Par<B>, f: (A, B) -> C): Par<C> =
            { es ->
                val af = a(es)
                val bf = b(es)
                UnitFuture(f(af.get(), bf.get()))
            }

    fun <A> fork(a: () -> Par<A>): Par<A> =
            { es ->
                es.submit(object : Callable<A> {
                    override fun call(): A = a()(es).get()
                })
            }

    fun <A> lazyUnit(a: () -> A): Par<A> = fork { unit(a()) }

    fun <A, B> asyncF(f: (A) -> B): (A) -> Par<B> = { a -> lazyUnit { f(a) } }

    fun <A, B> Par<A>.map(f: (A) -> B): Par<B> = { es -> UnitFuture(f(this(es).get())) }

    fun sortPar(parList: Par<List<Int>>) = parList.map { it.sorted() }

    fun <A> sequence_simple(l: List<Par<A>>): Par<List<A>> =
        l.foldRight({ a, b -> map2(a, b, { h, l -> List.cons(h, l)}) }, unit(List.nil()))

    fun <A> sequenceRight(ls: List<Par<A>>): Par<List<A>> =
            when {
                ls.isEmpty -> unit(List.nil())
                else -> map2(ls.head(), fork { sequenceRight(ls.tail()) }, { h, l -> List.cons(h, l) })
            }

    fun <A> sequence(ls: List<Par<A>>): Par<List<A>> = fork {
        if (ls.isEmpty)
            unit(List.nil<A>())
        else if(ls.isSingle)
            ls.head().map { List.single(it) }
        else {
            val p2 = ls.splitAt(ls.length() / 2)
            map2(sequence(p2._1()), sequence(p2._2())) { l1, l2 -> l1.append(l2) }
        }
    }

    fun <A> parFilter(ls: List<A>, f: (A) -> Boolean): Par<List<A>> {
        val pars: List<Par<List<A>>> = ls.map(asyncF { if(f(it)) List.single(it) else List.nil() })
        return sequence(pars).map { it.flatten() }
    }

    fun <A> equal(e: ExecutorService, p: Par<A>, p2: Par<A>): Boolean =
            p(e).get() == p2(e).get()

    fun <A> delay(fa: () -> Par<A>): Par<A> =
            { fa()(it) }

    fun <A> choice(cond: Par<Boolean>, t: Par<A>, f: Par<A>): Par<A> =
            { es ->
                if(run(es, cond).get()) t(es)
                else f(es)
            }

    fun <A> Par<Int>.choiceN(choices: List<Par<A>>): Par<A> =
            { es -> run(es, choices.index(run(es, this).get())) }

    fun <A> Par<Boolean>.choiceViaChoiceN(ifTrue: Par<A>, ifFalse: Par<A>): Par<A> =
            map { if(it) 0 else 1 }.choiceN(List.list(ifTrue, ifFalse))

    fun <K, V> Par<K>.choiceMap(choices: Map<K, Par<V>>): Par<V> =
            { es ->
                val key = run(es, this).get()
                run(es, choices.get(key)!!)
            }

    fun <A, B> Par<A>.chooser(choices: (A) -> Par<B>): Par<B> =
            { es ->
                val k = run(es, this).get()
                run(es, choices(k))
            }

    fun <A, B> Par<A>.flatMap(f: (A) -> Par<B>): Par<B> =
            { es ->
                val k = run(es, this).get()
                run(es, f(k))
            }

    fun <A> Par<Boolean>.choiceViaFlatMap(t: Par<A>, f: Par<A>): Par<A> =
            flatMap { if(it) t else f }

    fun <A> Par<Int>.choiceNViaFlatMap(choices: List<Par<A>>): Par<A> =
            flatMap { choices.index(it) }

    fun <A> join(a: Par<Par<A>>): Par<A> =
            { es -> run(es, run(es, a).get()) }

    fun <A> joinViaFlatMap(a: Par<Par<A>>): Par<A> =
            a.flatMap { it }

    fun <A, B> Par<A>.flatMapViaJoin(f: (A) -> Par<B>): Par<B> =
            join(map(f))
}

object Examples {
    fun sum(ints: List<Int>): Int =
        if (ints.length() <= 1)
            ints.headOption().orSome(0)
        else {
            val p = ints.splitAt(ints.length() / 2)
            sum(p._1()) + sum(p._2())
        }

    fun parSum(ints: List<Int>): Par<Int> =
            if (ints.length() <= 1)
                Parallel.unit(ints.headOption().orSome(0))
            else {
                val p = ints.splitAt(ints.length() / 2)
                Parallel.map2(parSum(p._1()), parSum(p._2())) { a, b -> a + b }
            }
}

fun <A> List<List<A>>.flatten(): List<A> {
    val listMonoid = Monoid.listMonoid<A>()
    return foldLeft(listMonoid.sum(), listMonoid.zero())
}

fun timer(f: () -> Unit): Long {
    val startTime = System.currentTimeMillis()
    f()
    return System.currentTimeMillis() - startTime
}

fun main(args: Array<String>) {
    val testData = Arbitrary.arbListInteger<Any>().gen(100000, Rand.standard)
    val foldTime = timer { Show.intShow.println(testData.foldLeft(Monoid.intAdditionMonoid.sum(), Monoid.intAdditionMonoid.zero())) }
    val norTime = timer { Show.intShow.println(Examples.sum(testData)) }
    val parTime = timer {
        val es = Executors.newFixedThreadPool(8)
        Show.intShow.println(Parallel.run(es, Examples.parSum(testData)).get())
    }
    Show.stringShow.println(String.format("foldLeft: %dms\nnormal: %dms\nparallel: %dms", foldTime, norTime, parTime))
}