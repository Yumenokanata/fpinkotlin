package iomonad

import andThen
import fj.data.Either
import fj.data.Option
import fj.data.List
import monad.H1
import monad.HTypeFG
import monad.Monad
import parallelism.Nonblocking.ParF
import parallelism.Nonblocking.Par
import parallelism.Nonblocking.ParU
import parallelism.Nonblocking.toOri
import java.nio.ByteBuffer
import java.nio.channels.AsynchronousFileChannel
import java.nio.channels.CompletionHandler
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future

/**
 * Created by yume on 17-1-3.
 *
 */
typealias ConsoleIO<A> = IOF.Free<IOF.ConsoleU, A>

// So `Free[F,A]` is not really an I/O type. The interpreter `runFree` gets
// to choose how to interpret these `F` requests, and whether to do "real" I/O
// or simply convert to some pure value!

// NB: These interpretations are not stack safe for the same reason,
// can instead work with `case class ConsoleReader[A](run: String => Trampoline[A])`,
// which gives us a stack safe monad

// We conclude that a good representation of an `IOF` monad is this:
typealias IO<A> = IOF.Free<ParU, A>

typealias IOU = H1<IOF.Free.FreeU, ParU>

fun <A> IO<A>.unsafePerformIO(E: ExecutorService): A =
        ParF.run(E, IOF.run(this, IOF.parMonad).toOri())


fun <F, A> narrow(value: H1<H1<IOF.Free.FreeU, F>, A>): IOF.Free<F, A> = value as IOF.Free<F, A>

fun <F, A> H1<H1<IOF.Free.FreeU, F>, A>.toOri(): IOF.Free<F, A> = narrow(this)

object IOF {
    sealed class Free<F, A> : H1<H1<Free.FreeU, F>, A> {
        object FreeU

        fun <B> flatMap(f: (A) -> Free<F, B>): Free<F, B> =
                FlatMap(this, f)

        fun <B> map(f: (A) -> B): Free<F, B> =
            flatMap(f andThen { Return<F, B>(it) })

        @Suppress("UNCHECKED_CAST")
        fun <B> asFlatMap(): FlatMap<F, A, B> = this as Free.FlatMap<F, A, B>

        data class Return<F, A>(val a: A) : Free<F, A>()
        data class Suspend<F, A>(val s: H1<F, A>) : Free<F, A>()
        data class FlatMap<F, A, B>(val s: Free<F, A>, val f: (A) -> Free<F, B>) : Free<F, B>()
    }

    fun <F> freeMonad(): Monad<H1<Free.FreeU, F>> =
            object : Monad<H1<Free.FreeU, F>> {
                override fun <A> unit(a: () -> A): H1<H1<Free.FreeU, F>, A> = Free.Return<F, A>(a())

                override fun <A, B> flatMap(gha: H1<H1<Free.FreeU, F>, A>,
                                            f: (A) -> H1<H1<Free.FreeU, F>, B>): H1<H1<Free.FreeU, F>, B> =
                        narrow(gha).flatMap(f andThen { narrow(it) })
            }

    tailrec fun <A> runTrampoline(a: Free<F0U, A>): A =
        when(a) {
            is Free.Return -> a.a
            is Free.Suspend -> narrow(a.s).f()
            else -> {
                val flatmap = a.asFlatMap<A>()

                when(flatmap.s) {
                    is Free.Return -> runTrampoline(flatmap.f(flatmap.s.a))
                    is Free.Suspend -> runTrampoline(flatmap.f(flatmap.s.s.toOri()()))
                    else -> {
                        val flat2 = flatmap.s.asFlatMap<A>()
                        runTrampoline(flat2.s.flatMap { a0 -> flat2.f(a0).flatMap(flatmap.f) })
                    }
                }
            }
        }

    /**
     * Exercise 3: Implement a `Free` interpreter which works for any `Monad`
     *
     * Scala:
     * def run[F[_],A](a: Free[F,A])(implicit F: Monad[F]): F[A] = step(a) match {
     *   case Return(a) => F.unit(a)
     *   case Suspend(r) => r
     *   case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
     *   case _ => sys.error("Impossible, since `step` eliminates these cases")
     * }
     */
    fun <F, A> run(a: Free<F, A>, fm: Monad<F>): H1<F, A> {
        val f = step(a)
        return when(f) {
            is Free.Return -> fm.unit(f.a)
            is Free.Suspend -> f.s
            else -> {
                val flat = f.asFlatMap<A>()

                when(flat.s) {
                    is Free.Suspend -> fm.flatMap(flat.s.s) { a2 -> run(flat.f(a2), fm) }
                    else -> throw RuntimeException("Impossible, since `step` eliminates these cases")
                }
            }
        }
    }

    /**
     * return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
     *
     * Scala:
     *
     * @annotation.tailrec
     * def step[F[_],A](a: Free[F,A]): Free[F,A] = a match {
     *   case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
     *   case FlatMap(Return(x), f) => step(f(x))
     *   case _ => a
     * }
     */
    tailrec fun <F, A> step(a: Free<F, A>): Free<F, A> =
            when(a) {
                is Free.Return -> a
                is Free.Suspend -> a
                else -> {
                    val flat = a.asFlatMap<A>()

                    when(flat.s) {
                        is Free.Return -> step(flat.f(flat.s.a))
                        is Free.Suspend -> a
                        else -> {
                            val flat2 = flat.asFlatMap<A>()
                            step(flat2.s.flatMap { a2 -> flat2.f(a2).flatMap(flat.f)})
                        }
                    }
                }
            }

    sealed class Console<A> : H1<ConsoleU, A> {
        abstract fun toPar(): Par<A>
        abstract fun toThunk(): () -> A

//        // other interpreters
        abstract fun toState(): ConsoleState<A>

        abstract fun toReader(): ConsoleReader<A>

        class ReadLine : Console<Option<String>>() {
            override fun toPar(): Par<Option<String>> = ParF.lazyUnit(this::run)

            override fun toThunk(): () -> Option<String> = this::run

            fun run(): Option<String> =
                    try {
                        Option.some(readLine())
                    } catch (e: Exception) {
                        Option.none()
                    }

            override fun toState(): ConsoleState<Option<String>> = ConsoleState({ bufs ->
                when {
                    bufs.inB.isEmpty() -> Option.none<String>() to bufs
                    else -> Option.some(bufs.inB.head()) to bufs.copy(inB = bufs.inB.tail())
                }
            })

            override fun toReader(): ConsoleReader<Option<String>> = ConsoleReader { i -> Option.some(i) }
        }

        data class PrintLine(val line: String) : Console<Unit>() {
            override fun toPar(): Par<Unit> = ParF.lazyUnit { println(line) }

            override fun toThunk(): () -> Unit = { println(line) }

            override fun toState(): ConsoleState<Unit> = ConsoleState({ bufs -> Unit to bufs.copy(bufs.outB.cons(line)) })

            override fun toReader(): ConsoleReader<Unit> = ConsoleReader({ Unit })
        }

        companion object {
            fun readLn(): ConsoleIO<Option<String>> = Free.Suspend(ReadLine())

            fun printLn(line: String): ConsoleIO<Unit> = Free.Suspend(PrintLine(line))
        }
    }

    object ConsoleU

    fun <A> narrow(value: H1<ConsoleU, A>): Console<A> = value as Console<A>

    fun <A> H1<ConsoleU, A>.toOri(): Console<A> = narrow(this)


    /*
    How do we actually _run_ a `ConsoleIO` program? We don't have a `Monad[Console]`
    for calling `run`, and we can't use `runTrampoline` either since we have `Console`,
    not `Function0`. We need a way to translate from `Console` to `Function0`
    (if we want to evaluate it sequentially) or a `Par`.

    We introduce the following type to do this translation:
    */

    /* Translate between any `F[A]` to `G[A]`. */
    interface Translate<F, G> {
        operator fun <A> invoke(f: H1<F, A>): H1<G, A>
    }

    val function0Monad = object : Monad<F0U> {
        override fun <A> unit(a: () -> A): H1<F0U, A> = a.toHType()

        override fun <A, B> flatMap(gha: H1<F0U, A>, f: (A) -> H1<F0U, B>): H1<F0U, B> =
                { f(gha.toOri()()).toOri()() }.toHType()
    }

    val parMonad = object : Monad<ParU> {
        override fun <A> unit(a: () -> A): H1<ParU, A> = ParF.unit(a())

        override fun <A, B> flatMap(a: H1<ParU, A>, f: (A) -> H1<ParU, B>): H1<ParU, B> =
                ParF.fork { ParF.flatMap(a.toOri(), f andThen { it.toOri() }) }
    }

    fun <F, G, A> runFree(free: Free<F, A>, t: Translate<F, G>, g: Monad<G>): H1<G, A> {
        val s = step(free)
        return when(s) {
            is Free.Return -> g.unit(s.a)
            is Free.Suspend -> t(s.s)
            else -> {
                val flatmap = s.asFlatMap<A>()

                when(flatmap.s) {
                    is Free.Suspend -> g.flatMap(t(flatmap.s.s)) { a -> runFree(flatmap.f(a), t, g) }
                    else -> throw RuntimeException("Impossible, since `step` eliminates these cases")
                }
            }
        }
    }

    val consoleToFunction0 = object : Translate<ConsoleU, F0U> {
        override fun <A> invoke(f: H1<ConsoleU, A>): H1<F0U, A> = f.toOri().toThunk().toHType()
    }

    val consoleToPar = object : Translate<ConsoleU, ParU> {
        override fun <A> invoke(f: H1<ConsoleU, A>): H1<ParU, A> = f.toOri().toPar()
    }

    fun <A> runConsoleFunction0(a: Free<ConsoleU, A>): () -> A =
            runFree(a, consoleToFunction0, function0Monad).toOri()

    fun <A> runConsolePar(a: Free<ConsoleU, A>): Par<A> =
            runFree(a, consoleToPar, parMonad).toOri()

    /*
    The `runConsoleFunction0` implementation is unfortunately not stack safe,
    because it relies of the stack safety of the underlying monad, and the
    `Function0` monad we gave is not stack safe. To see the problem, try
    running: `freeMonad.forever(Console.printLn("Hello"))`.
    */

    // Exercise 4 (optional, hard): Implement `runConsole` using `runFree`,
    // without going through `Par`. Hint: define `translate` using `runFree`.

    fun <F, G, A> translate(f: Free<F,A>, fg: Translate<F, G>): Free<G, A> {
        val t = object : Translate<F, H1<Free.FreeU, G>> {
            override fun <A> invoke(f: H1<F, A>): H1<H1<Free.FreeU, G>, A> = Free.Suspend(fg(f))
        }
        return runFree(f, t, freeMonad<G>()).toOri()
    }

    fun <A> runConsole(a: Free<ConsoleU, A>): A =
            runTrampoline(translate(a,
                    object : Translate<ConsoleU, F0U> {
                        override fun <A> invoke(f: H1<ConsoleU, A>): H1<F0U, A> = f.toOri().toThunk().toHType()
                    }))


    /*
    There is nothing about `Free[Console,A]` that requires we interpret
    `Console` using side effects. Here are two pure ways of interpreting
    a `Free[Console,A]`.
    */
    data class Buffers(val inB: List<String>, val outB: List<String>)

    // A specialized state monad
    data class ConsoleState<A>(val run: (Buffers) -> Pair<A, Buffers>) : H1<ConsoleStateU, A> {
        fun <B> map(f: (A) -> B): ConsoleState<B> =
                ConsoleState({ s ->
                    val (a, s1) = run(s)
                    f(a) to s1
                })

        fun <B> flatMap(f: (A) -> ConsoleState<B>): ConsoleState<B> =
                ConsoleState({ s ->
                    val (a, s1) = run(s)
                    f(a).run(s1)
                })

        companion object {
            val monad = object : Monad<ConsoleStateU> {
                override fun <A> unit(a: () -> A): H1<ConsoleStateU, A> = ConsoleState({ bufs -> Pair(a(), bufs) })

                override fun <A, B> flatMap(gha: H1<ConsoleStateU, A>, f: (A) -> H1<ConsoleStateU, B>): H1<ConsoleStateU, B> =
                        gha.toOri().flatMap(f andThen { it.toOri() })
            }
        }
    }

    object ConsoleStateU

    fun <A> H1<ConsoleStateU, A>.toOri(): ConsoleState<A> = this as ConsoleState<A>


    // A specialized reader monad
    data class ConsoleReader<A>(val run: (String) -> A) : H1<ConsoleReaderU, A> {
        fun <B> map(f: (A) -> B): ConsoleReader<B> =
        ConsoleReader({ r -> f(run(r)) })

        fun <B> flatMap(f: (A) -> ConsoleReader<B>): ConsoleReader<B> =
        ConsoleReader({ r -> f(run(r)).run(r) })

        companion object {
            val monad = object : Monad<ConsoleReaderU> {
                override fun <A> unit(a: () -> A): H1<ConsoleReaderU, A> = ConsoleReader({ a() })

                override fun <A, B> flatMap(gha: H1<ConsoleReaderU, A>, f: (A) -> H1<ConsoleReaderU, B>): H1<ConsoleReaderU, B> =
                        gha.toOri().flatMap(f andThen { it.toOri() })
            }
        }
    }

    object ConsoleReaderU

    fun <A> H1<ConsoleReaderU, A>.toOri(): ConsoleReader<A> = this as ConsoleReader<A>

    val consoleToState =
            object : Translate<ConsoleU, ConsoleStateU> {
                override fun <A> invoke(f: H1<ConsoleU, A>): H1<ConsoleStateU, A> = f.toOri().toState()
            }

    val consoleToReader =
            object : Translate<ConsoleU, ConsoleReaderU> {
                override fun <A> invoke(f: H1<ConsoleU, A>): H1<ConsoleReaderU, A> = f.toOri().toReader()
            }

    /* Can interpet these as before to convert our `ConsoleIO` to a pure value that does no I/O! */
    fun <A> runConsoleReader(io: ConsoleIO<A>): ConsoleReader<A> =
            runFree(io, consoleToReader, ConsoleReader.monad).toOri()

    fun <A> runConsoleState(io: ConsoleIO<A>): ConsoleState<A> =
            runFree(io, consoleToState, ConsoleState.monad).toOri()


    /*
     * Exercise 5: Implement a non-blocking read from an asynchronous file channel.
     * We'll just give the basic idea - here, we construct a `Future`
     * by reading from an `AsynchronousFileChannel`, a `java.nio` class
     * which supports asynchronous reads.
     */

    // Provides the syntax `Async { k => ... }` for asyncronous IOF blocks.
    fun <A> Async(cb: ((A) -> Unit) -> Unit): IO<A> =
            Free.Suspend(ParF.async(cb))

    // Provides the `IOF { ... }` syntax for synchronous IOF blocks.
    fun <A> IO(a: () -> A): IO<A> = Free.Suspend(ParF.delay(a))

    fun read(file: AsynchronousFileChannel,
             fromPosition: Long,
             numBytes: Int): Par<Either<Throwable, ByteArray>> =
            ParF.async { cb: (Either<Throwable, ByteArray>) -> Unit ->
                val buf = ByteBuffer.allocate(numBytes)
                file.read(buf, fromPosition, Unit, object : CompletionHandler<Int, Unit> {
                    override fun completed(bytesRead: Int, ignore: Unit) {
                        val arr = ByteArray(bytesRead)
                        buf.slice().get(arr, 0, bytesRead)
                        cb(Either.right(arr))
                    }

                    override fun failed(err: Throwable, ignore: Unit) =
                            cb(Either.left(err))
                })
            }
}

data class HTypeF0<T>(val f: () -> T) : H1<F0U, T>

object F0U

fun <A> narrow(value: H1<F0U, A>): HTypeF0<A> = value as HTypeF0<A>

fun <A> Function0<A>.toHType(): H1<F0U, A> = HTypeF0(this)

fun <A> H1<F0U, A>.toOri(): Function0<A> = narrow(this).f
