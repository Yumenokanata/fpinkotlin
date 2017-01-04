package parallelism

import fj.data.Either
import fj.data.List
import fj.data.Option
import monad.H1
import monad.cons
import parallelism.Nonblocking.Par
import java.util.concurrent.Callable
import java.util.concurrent.CountDownLatch
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.atomic.AtomicReference

/**
 * Created by yume on 17-1-3.
 */
object Nonblocking {
    interface Future<out A> {
        operator fun invoke(k: (A) -> Unit): Unit
    }

    interface Par<A> : (ExecutorService) -> Future<A>, H1<ParU, A>

    fun <A> Par(handler: (ExecutorService) -> Future<A>): Par<A> =
            object : Par<A> {
                override fun invoke(es: ExecutorService): Future<A> = handler(es)
            }

    object ParU

    fun <A> narrow(value: H1<ParU, A>): Par<A> = value as Par<A>

    fun <A> H1<ParU, A>.toOri(): Par<A> = narrow(this)

    object ParF {
        fun <A> run(es: ExecutorService, p: Par<A>): A {
            val ref = AtomicReference<A>()
            val latch = CountDownLatch(1)
            p(es)({ a -> ref.set(a); latch.countDown() })
            latch.await()
            return ref.get()
        }

        fun <A> unit(a: A): Par<A> = Par { es ->
            object : Future<A> {
                override fun invoke(cb: (A) -> Unit) = cb(a)
            }
        }


        /** A non-strict version of `unit` */
        fun <A> delay(a: () -> A): Par<A> = Par {
            object : Future<A> {
                override fun invoke(cb: (A) -> Unit) = cb(a())
            }
        }

        fun <A> fork(a: () -> Par<A>): Par<A> = Par { es ->
            object : Future<A> {
                override fun invoke(cb: (A) -> Unit) = eval(es){ a()(es)(cb) }
            }
        }

        /**
         * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
         * This will come in handy in Chapter 13.
         */
        fun <A> async(f: ((A) -> Unit) -> Unit): Par<A> = Par {
            object : Future<A> {
                override fun invoke(cb: (A) -> Unit) = f(cb)
            }
        }

        /**
         * Helper function, for evaluating an action
         * asynchronously, using the given `ExecutorService`.
         */
        fun eval(es: ExecutorService, r: () -> Unit): Unit =
            es.submit(object : Callable<Unit> { override fun call(): Unit = r() }).get()

        fun <A, B, C> map2(p: Par<A>, p2: Par<B>, f: (A, B) -> C): Par<C> = Par { es ->
            object : Future<C> {
                override fun invoke(cb: (C) -> Unit): Unit {
                    var ar: Option<A> = Option.none()
                    var br: Option<B> = Option.none()

                    val combiner = Actor.apply<Either<A, B>>(es, { eab ->
                        when {
                            eab == null -> {}
                            eab.isLeft -> if(br.isSome) eval(es){ (cb(f(eab.left().value(), br.some()))) }
                            else ar = Option.some(eab.left().value())
                            eab.isRight -> if(br.isSome) eval(es){ (cb(f(ar.some(), eab.right().value()))) }
                            else br = Option.some(eab.right().value())
                        }
                    })
                    p(es)({ a -> combiner(Either.left(a)) })
                    p2(es)({ b -> combiner(Either.right(b)) })
                }
            }
        }

        // specialized version of `map`
        fun <A, B> map(p: Par<A>, f: (A) -> B): Par<B> = Par { es ->
            object : Future<B> {
                override fun invoke(cb: (B) -> Unit) =
                        p(es)({ a -> eval(es) { cb(f(a)) } })
            }
        }

        fun <A> lazyUnit(a: () -> A): Par<A> =
            fork { unit(a()) }

        fun <A, B> asyncF(f: (A) -> B): (A) -> Par<B> =
                { a -> lazyUnit { f(a) }}

        fun <A> sequenceRight(ls: List<Par<A>>): Par<List<A>> =
            when {
                ls.isEmpty -> unit(List.nil())
                else -> map2(ls.head(), fork { sequence(ls.tail()) }) { h, l -> h cons l }
            }

        fun <A> sequence(ls: List<Par<A>>): Par<List<A>> = fork {
            when {
                ls.isEmpty -> unit(List.nil())
                ls.isSingle -> map(ls.head()) { List.single(it) }
                else -> {
                    val p = ls.splitAt(ls.length() / 2)
                    map2(sequence(p._1()), sequence(p._2())) { l, r -> l.append(r) }
                }
            }
        }

        fun <A, B> parMap(ls: List<A>, f: (A) -> B): Par<List<B>> =
            sequence(ls.map(asyncF(f)))

        // exercise answers

        /*
         * We can implement `choice` as a new primitive.
         *
         * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
         * some `Par`, `p`, is the idiom for running `p`, and registering
         * a callback to be invoked when its result is available. The
         * result will be bound to `result` in the function passed to
         * `p(es)`.
         *
         * If you find this code difficult to follow, you may want to
         * write down the type of each subexpression and follow the types
         * through the implementation. What is the type of `p(es)`? What
         * about `t(es)`? What about `t(es)(cb)`?
         */
        fun <A> choice(p: Par<Boolean>, t: Par<A>, f: Par<A>): Par<A> = Par { es ->
            object : Future<A> {
                override fun invoke(cb: (A) -> Unit) =
                        p(es)({ b ->
                            if(b) eval(es) { t(es)(cb) }
                            else eval(es) { f(es)(cb) }
                        })
            }
        }

        /* The code here is very similar. */
        fun <A> choiceN(p: Par<Int>, ps: List<Par<A>>): Par<A> = Par { es ->
            object : Future<A> {
                override fun invoke(cb: (A) -> Unit) =
                        p(es)({ ind -> eval(es) { ps.index(ind)(es)(cb) }})
            }
        }

        fun <A> choiceViaChoiceN(a: Par<Boolean>, ifTrue: Par<A>, ifFalse: Par<A>): Par<A> =
                choiceN(map(a){ b -> if (b) 0 else 1 }, List.list(ifTrue, ifFalse))

        fun <K, V> choiceMap(p: Par<K>, ps: Map<K, Par<V>>): Par<V> = Par { es ->
            object : Future<V> {
                override fun invoke(cb: (V) -> Unit) =
                        p(es)({ k -> ps[k]?.invoke(es)?.invoke(cb) })
            }
        }

        /* `chooser` is usually called `flatMap` or `bind`. */
        fun <A,B> chooser(p: Par<A>, f: (A) -> Par<B>): Par<B> =
                flatMap(p, f)

        fun <A,B> flatMap(p: Par<A>, f: (A) -> Par<B>): Par<B> = Par { es ->
            object : Future<B> {
                override fun invoke(cb: (B) -> Unit) =
                        p(es)({ a -> f(a)(es)(cb) })
            }
        }

        fun <A> choiceViaFlatMap(p: Par<Boolean>, f: Par<A>, t: Par<A>): Par<A> =
                flatMap(p) { b -> if (b) t else f }

        fun <A> choiceNViaFlatMap(p: Par<Int>, choices: List<Par<A>>): Par<A> =
                flatMap(p) { i -> choices.index(i) }

        fun <A> join(p: Par<Par<A>>): Par<A> = Par { es ->
            object : Future<A> {
                override fun invoke(cb: (A) -> Unit) =
                        p(es)({ p2 -> eval(es) { p2(es)(cb) } })
            }
        }

        fun <A> joinViaFlatMap(a: Par<Par<A>>): Par<A> =
                flatMap(a) { it }

        fun <A, B> flatMapViaJoin(p: Par<A>, f: (A) -> Par<B>): Par<B> =
                join(map(p, f))
    }
}