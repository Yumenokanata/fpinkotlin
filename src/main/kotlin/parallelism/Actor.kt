package parallelism

import java.util.concurrent.Callable
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference

/**
 * Created by yume on 17-1-3.
 */
class Actor<A>(val strategy: Strategy, val handler: (A?) -> Unit, val onError: (Throwable) -> Unit = {}) {
    private val tail = AtomicReference(Node<A>())
    private val suspended = AtomicInteger(1)
    private val head = AtomicReference(tail.get())

    operator fun invoke(a: A): Unit {
        val n = Node(a)
        head.getAndSet(n).lazySet(n)
        trySchedule()
    }

    fun <B> contramap(f: (B) -> A): Actor<B> =
        Actor(strategy, { b -> this(f(b!!)) }, onError)

    private fun trySchedule(): Unit {
        if(suspended.compareAndSet(1, 0)) schedule()
    }

    private fun schedule(): Unit {
        strategy.invoke { act() }
    }

    private fun act(): Unit {
        val t = tail.get()
        val n = batchHandle(t, 1024)

        if (n != t) {
            n.a = null
            tail.lazySet(n)
            schedule()
        } else {
            suspended.set(1)
            if (n.get() != null) trySchedule()
        }
    }

    tailrec private fun batchHandle(t: Node<A>, i: Int): Node<A> {
        val n = t.get()
        return if(n != null) {
            try {
                handler(n.a)
            } catch (e: Exception) {
                onError(e)
            }
            if (i > 0) batchHandle(n, i - 1) else n
        } else t
    }

    companion object {
        fun <A> apply(es: ExecutorService, handler: (A?) -> Unit, onError: (Throwable) -> Unit = {}): Actor<A> =
                Actor(Strategy.fromExecutorService(es), handler, onError)
    }
}

private class Node<A>(var a: A? = null) : AtomicReference<Node<A>>()

interface Strategy {
    operator fun <A> invoke(a: () -> A): () -> A

    companion object {
        fun fromExecutorService(es: ExecutorService): Strategy =
            object : Strategy {
                override fun <A> invoke(a: () -> A): () -> A {
                    val f = es.submit(object : Callable<A> { override fun call(): A = a() })
                    return { f.get() }
                }
            }

        /**
         * A `Strategy` which begins executing its argument immediately in the calling thread.
         */
        fun sequential(): Strategy =
                object : Strategy {
                    override fun <A> invoke(a: () -> A): () -> A = a
                }
    }
}