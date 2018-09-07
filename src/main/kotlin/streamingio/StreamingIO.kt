package streamingio

import andThen
import errorhanding.Either
import errorhanding.Either.Left
import errorhanding.Either.Right
import errorhanding.Option
import errorhanding.Option.Some
import errorhanding.Option.None
import iomonad.*
import monad.H1
import parallelism.Nonblocking
import parallelism.Nonblocking.ParU
import streamingio.Process.Companion.constant
import streamingio.Process.Companion.eval
import streamingio.Process.Companion.fileW
import streamingio.Process.Companion.intersperse
import streamingio.Process.Companion.lines
import streamingio.Process.Companion.resource_
import streamingio.Process.Companion.runLog
import java.io.BufferedReader
import java.io.File
import java.io.FileReader
import java.io.FileWriter
import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet


/*
Our generalized process type is parameterized on the protocol used for
communicating with the driver. This works similarly to the `IO` type
we defined in chapter 13. The `Await` constructor emits a request of
type `F[A]`, and receives a response of type `Either[Throwable,A]`:
trait Process[F,A]
is class Await[F[_],A,O](
req: F[A],
recv: Either[Throwable,A] -> Process<F, O>) extends Process<F, O>
is class Halt[F[_],O](err: Throwable) extends Process<F, O>
is class Emit[F[_],O](head: O, tail: Process<F, O>) extends Process<F, O>
The `Await` constructor may now receive a successful result or an error.
The `Halt` constructor now has a _reason_ for termination, which may be
either normal termination indicated by the special exception `End`,
forceful terimation, indicated by the special exception `Kill`,
or some other error.
We'll use the improved `Await` and `Halt` cases together to ensure
that all resources get released, even in the event of exceptions.
 */

data class Await<F, out A, O>(
        val req: H1<F, A>,
        val recv: (Either<Throwable, A>) -> Process<F, O>) : Process<F, O>()

data class Emit<F, O>(
        val head: O,
        val tail: Process<F, O>) : Process<F, O>()

data class Halt<F, O>(val err: Throwable) : Process<F, O>()


/* Our generalized `Process` type can represent sources! */

/* Special exception indicating normal termination */
object End : Exception()

/* Special exception indicating forceful termination */
object Kill : Exception()


fun <F, O> emit(
        head: O,
        tail: Process<F, O> = Halt<F, O>(End)): Process<F, O> =
        Emit(head, tail)

fun <F, A, O> await(req: H1<F, A>, recv: (Either<Throwable, A>) -> Process<F, O>): Process<F, O> =
        Await(req, recv)

sealed class Process<F, O> {
    /*
     * Many of the same operations can be defined for this generalized
     * `Process` type, regardless of the choice of `F`.
     */

    fun <O2> map(f: (O) -> O2): Process<F, O2> = when (this) {
        is Await<F, *, O> -> await<F, Any?, O2>(req, recv andThen { it.map(f) })
        is Emit -> Try { Emit(f(head), tail.map(f)) }
        is Halt -> Halt(err)
    }

    infix fun plusP(p: () -> Process<F, O>): Process<F, O> =
            this.onHalt {
                when (it) {
                    is End -> Try(p) // we consult `p` only on normal termination
                    else -> Halt(it)
                }
            }

    /*
     * Like `plusP`, but _always_ runs `p`, even if `this` halts with an error.
     */
    fun onComplete(p: () -> Process<F, O>): Process<F, O> =
            this.onHalt {
                when (it) {
                    is End -> p().asFinalizer()
                    else -> p().asFinalizer() plusP { Halt(it) } // we always run `p`, but preserve any errors
                }
            }

    fun asFinalizer(): Process<F, O> = when (this) {
        is Emit -> Emit(head, tail.asFinalizer())
        is Halt -> Halt(err)
        is Await<F, Any?, O> -> await(req) {
            when {
                it is Either.Left && it.get == Kill -> this.asFinalizer()
                else -> recv(it)
            }
        }
    }

    fun onHalt(f: (Throwable) -> Process<F, O>): Process<F, O> = when (this) {
        is Halt -> Try { f(err) }
        is Emit -> Emit(head, tail.onHalt(f))
        is Await<F, Any?, O> -> Await(req, recv andThen { it.onHalt(f) })
    }

    /*
     * Anywhere we _call_ `f`, we catch exceptions and convert them to `Halt`.
     * See the helper function `Try` defined below.
     */
    fun <O2> flatMap(f: (O) -> Process<F, O2>): Process<F, O2> =
            when (this) {
                is Halt -> Halt(err)
                is Emit -> Try { f(head) } plusP { tail.flatMap(f) }
                is Await<F, Any?, O> ->
                    Await(req, recv andThen { it.flatMap(f) })
            }

    fun repeat(): Process<F, O> =
            this plusP this::repeat

    fun repeatNonempty(): Process<F, O> {
        val cycle = this.map { o -> Option.some(o) } plusP { emit<F, Option<O>>(Option.None).repeat() }
        // cut off the cycle when we see two `None` values in a row, as this
        // implies `this` has produced no values during an iteration
        val trimmed = cycle pipeR window2() pipeR (takeWhile {
            val (i, i2) = it
            when {
                i is Some && i.get is None && i2 is None -> false
                else -> true
            }
        })
        return trimmed.map { it.second }.flatMap {
            when (it) {
                is Option.None -> Halt(End)
                is Some -> emit<F, O>(it.get)
            }
        }
    }

    /*
     * Exercise 10: This function is defined only if given a `MonadCatch[F]`.
     * Unlike the simple `runLog` interpreter defined in the companion object
     * below, this is not tail recursive and responsibility for stack safety
     * is placed on the `Monad` instance.
     */
//    fun runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
//      def go(cur: Process<F, O>, acc: IndexedSeq[O]): F[IndexedSeq[O]] =
//        cur match {
//          is Emit(h,t) -> go(t, acc :+ h)
//          is Halt(End) -> F.unit(acc)
//          is Halt(err) -> F.fail(err)
//          is Await(req,recv) -> F.flatMap (F.attempt(req)) { e -> go(Try(recv(e)), acc) }
//        }
//      go(this, IndexedSeq())
//    }

    /*
     * We define `Process1` as a type alias - see the companion object
     * for `Process` below. Using that, we can then define `|>` once
     * more. The definition is extremely similar to our previous
     * definition. We again use the helper function, `feed`, to take
     * care of the is where `this` is emitting values while `p2`
     * is awaiting these values.
     *
     * The one subtlety is we make sure that if `p2` halts, we
     * `kill` this process, giving it a chance to run any cleanup
     * actions (like closing file handles, etc).
     */
    infix fun <O2> pipeR(p2: Process1<O, O2>): Process<F, O2> =
            when (p2) {
                is Halt -> this.kill<O2>().onHalt { e2 -> Halt<F, O2>(p2.err) plusP { Halt(e2) } }
                is Emit -> Emit(p2.head, this pipeR p2.tail)
                is Await<Is<O>.f, Any?, O2> -> when (this) {
                    is Halt -> Halt<F, O>(err) pipeR p2.recv(Left(err))
                    is Emit -> tail pipeR Try { p2.recv(Right(head)) }
                    is Await<F, Any?, O> -> await(req, recv andThen { it pipeR p2 })
                }
            }

    fun <O2> kill(): Process<F, O2> {
        tailrec fun <O2> kill(p: Process<F, O>): Process<F, O2> = when (p) {
            is Await<F, Any?, O> -> p.recv(Either.Left(Kill)).drain<O2>().onHalt {
                when (it) {
                    is Kill -> Halt(End) // we convert the `Kill` exception back to normal termination
                    else -> Halt(it)
                }
            }
            is Halt -> Halt(p.err)
            is Emit -> kill(p.tail)
        }

        return kill(this)
    }

    /** Alias for `this |> p2`. */
    fun <O2> pipe(p2: Process1<O, O2>): Process<F, O2> =
            this pipeR p2

    fun <O2> drain(): Process<F, O2> = when (this) {
        is Halt -> Halt(err)
        is Emit -> tail.drain()
        is Await<F, Any?, O> -> Await(req, recv andThen { it.drain<O2>() })
    }

    fun filter(f: (O) -> Boolean): Process<F, O> =
            this pipeR Process.filter(f)

    fun take(n: Int): Process<F, O> =
            this pipeR Process.take(n)

    fun once(): Process<F, O> = take(1)

    /*
     * Use a `Tee` to interleave or combine the outputs of `this` and
     * `p2`. This can be used for zipping, interleaving, and so forth.
     * Nothing requires that the `Tee` read elements from each
     * `Process` in lockstep. It could read fifty elements from one
     * side, then two elements from the other, then combine or
     * interleave these values in some way, etc.
     *
     * This definition uses two helper functions, `feedL` and `feedR`,
     * which feed the `Tee` in a tail-recursive loop as long as
     * it is awaiting input.
     */

    interface TeeFun<F, O, O2> {
        operator fun <T> invoke(t: Tee<O, O2, T>): Process<F, T>
    }

    infix fun <O2> tee(p2: Process<F, O2>): TeeFun<F, O, O2> = object : TeeFun<F, O, O2> {
        override fun <T> invoke(t: Tee<O, O2, T>): Process<F, T> = tee(p2, t)
    }

    fun <O2, O3> tee(p2: Process<F, O2>, t: Tee<O, O2, O3>): Process<F, O3> {
        return when (t) {
            is Halt -> this.kill<O3>().onComplete { p2.kill() }.onComplete { Halt(t.err) }
            is Emit -> Emit(t.head, (this tee p2)(t))
            is Await<T<O, O2>.f, Any?, O3> -> when (t.req.toOri().get) {
                is Left -> when (this) {
                    is Halt -> p2.kill<O3>().onComplete { Halt(err) }
                    is Emit -> (tail tee p2)(Try { t.recv(Right(head)) })
                    is Await<F, Any?, O> -> await(req, recv andThen { this2 -> (this2 tee p2)(t) })
                }
                is Right -> when (p2) {
                    is Halt -> this.kill<O3>().onComplete { Halt(p2.err) }
                    is Emit -> this.tee(p2.tail)(Try { t.recv(Right(p2.head)) })
                    is Await<F, Any?, O2> -> await(p2.req, p2.recv andThen { p3 -> (this tee p3)(t) })
                }
            }
        }
    }

    fun <O2, O3> zipWith(p2: Process<F, O2>, f: (O, O2) -> O3): Process<F, O3> =
            (this tee p2)(zipWith(f))

    fun <O2> zip(p2: Process<F, O2>): Process<F, Pair<O, O2>> =
            zipWith(p2) { o, o2 -> o to o2 }

    fun to(sink: Sink<F, O>): Process<F, Unit> =
            join((this.zipWith(sink) { o, f -> f(o) }))

    fun <O2> through(p2: Channel<F, O, O2>): Process<F, O2> =
            join(this.zipWith(p2) { o, f -> f(o) })

    companion object {
        /**
         * Helper function to safely produce `p`, or gracefully halt
         * with an error if an exception is thrown.
         */
        fun <F, O> Try(p: () -> Process<F, O>): Process<F, O> =
                try {
                    p()
                } catch (e: Throwable) {
                    Halt(e)
                }

        /*
         * Safely produce `p`, or run `cleanup` and halt gracefully with the
         * exception thrown while evaluating `p`.
         */
        fun <F, O> TryOr(p: () -> Process<F, O>, cleanup: Process<F, O>): Process<F, O> =
                try {
                    p()
                } catch (e: Throwable) {
                    cleanup plusP { Halt(e) }
                }

        /*
         * Safely produce `p`, or run `cleanup` or `fallback` if an exception
         * occurs while evaluating `p`.
         */
        fun <F, O> TryAwait(p: () -> Process<F, O>, fallback: Process<F, O>, cleanup: Process<F, O>): Process<F, O> =
                try {
                    p()
                } catch (e: Throwable) {
                    when (e) {
                        is End -> fallback
                        else -> cleanup plusP { Halt(e) }
                    }
                }

        /*
         * A `Process<F, O>` where `F` is a monad like `IO` can be thought of
         * as a source.
         */

        /*
         * Here is a simple tail recursive function to collect all the
         * output of a `Process<IO, O>`. Notice we are using the fact
         * that `IO` can be `run` to produce either a result or an
         * exception.
         */
        fun <O> runLog(src: Process<IOU, O>): IO<List<O>> = IOF.IO {
            val E = java.util.concurrent.Executors.newFixedThreadPool(4)

            fun go(cur: Process<IOU, O>, acc: List<O>): List<O> =
                    when (cur) {
                        is Emit -> go(cur.tail, acc + cur.head)
                        is Halt -> if (cur.err == End) acc else throw cur.err
                        is Await<IOU, Any?, O> -> {
                            val next = try {
                                cur.recv(Right(cur.req.toOri().unsafePerformIO(E)))
                            } catch (err: Throwable) {
                                cur.recv(Left(err))
                            }
                            go(next, acc)
                        }
                    }
            try {
                go(src, emptyList())
            } finally {
                E.shutdown()
            }
        }

        /*
         * We can write a version of collect that works for any `Monad`.
         * See the definition in the body of `Process`.
         */

        //TODO
//      val p: Process<IOU, String> =
//              await(IO(BufferedReader(FileReader("lines.txt")))) {
//                  when (it) {
//                      is Either.Right -> {
//                          val next: Process<IOU, String> = await(IO(it.get.readLine)) {
//                              is Either.Left(e) -> await(IO(b.close))(_ -> Halt(e))
//                              is Either.Right(line) -> Emit(line, next)
//                          }
//                          next
//                      }
//                      is Either.Left -> Halt(it.get)
//                  }
//              }

        /*
         * Generic combinator for producing a `Process<IO, O>` from some
         * effectful `O` source. The source is tied to some resource,
         * `R` (like a file handle) that we want to ensure is released.
         * See `lines` below for an example use.
         */
        fun <R, O> resource(acquire: IO<R>,
                            use: (R) -> Process<IOU, O>,
                            release: (R) -> Process<IOU, O>): Process<IOU, O> =
                eval(acquire).flatMap { r -> use(r).onComplete { release(r) } }

        /*
         * Like `resource`, but `release` is a single `IO` action.
         */
        fun <R, O> resource_(acquire: IO<R>,
                             use: (R) -> Process<IOU, O>,
                             release: (R) -> IO<Unit>): Process<IOU, O> =
                resource(acquire, use, release andThen { eval_<IOU, Unit, O>(it) })

        /*
         * Create a `Process<IO, O>` from the lines of a file, using
         * the `resource` combinator above to ensure the file is closed
         * when processing the stream of lines is finished.
         */
        fun lines(filename: String): Process<IOU, String> =
                resource(IOF.IO { FileReader(filename) },
                        { src ->
                            val iter by lazy { src.readLines().listIterator() } // a stateful iterator
                            fun step() = if (iter.hasNext()) Some(iter.next()) else None

                            fun lines(): Process<IOU, String> =
                                    eval(IOF.IO { step() }).flatMap {
                                        when (it) {
                                            is None -> Halt<IOU, String>(End)
                                            is Some -> Emit(it.get, lines())
                                        }
                                    }
                            lines()
                        },
                        { src -> eval_(IOF.IO { src.close() }) })

        /* Exercise 11: Implement `eval`, `eval_`, and use these to implement `lines`. */
        fun <F, A> eval(a: H1<F, A>): Process<F, A> =
                await<F, A, A>(a) {
                    when (it) {
                        is Left -> Halt(it.get)
                        is Right -> Emit(it.get, Halt(End))
                    }
                }

        /* Evaluate the action purely for its effects. */
        fun <F, A, B> eval_(a: H1<F, A>): Process<F, B> =
                eval<F, A>(a).drain<B>()

        /* Helper function with better type inference. */
        fun <A> evalIO(a: IO<A>): Process<IOU, A> = eval(a)



        /* Some helper functions to improve type inference. */

        fun <I, O> await1(recv: (I) -> Process1<I, O>,
                          fallback: () -> Process1<I, O> = { halt1<I, O>() }): Process1<I, O> =
                Await(Get<I>()) { e: Either<Throwable, I> ->
                    when {
                        e is Left && e.get == End -> fallback()
                        e is Left -> Halt(e.get)
                        e is Right -> Try { recv(e.get) }
                        else -> throw RuntimeException("Unreachable")
                    }
                }

        fun <I, O> emit1(h: O, tl: Process1<I, O> = halt1<I, O>()): Process1<I, O> =
                emit(h, tl)

        fun <I, O> halt1(): Process1<I, O> = Halt<Is<I>.f, O>(End)

        fun <I, O> lift(f: (I) -> O): Process1<I, O> =
                await1<I, O>({ i: I -> emit(f(i)) }).repeat()

        fun <I> filter(f: (I) -> Boolean): Process1<I, I> =
                await1<I, I>({ i -> if (f(i)) emit(i) else halt1() }).repeat()

        // we can define take, takeWhile, and so on as before

        fun <I> take(n: Int): Process1<I, I> =
                if (n <= 0) halt1()
                else await1<I, I>({ i -> emit(i, take(n-1)) })

        fun <I> takeWhile(f: (I) -> Boolean): Process1<I, I> =
                await1({ i ->
                    if (f(i)) emit(i, takeWhile(f))
                    else halt1()
                })

        fun <I> dropWhile(f: (I) -> Boolean): Process1<I, I> =
                await1({ i ->
                    if (f(i)) dropWhile(f)
                    else emit(i, id())
                })

        fun <I> id(): Process1<I, I> =
                await1({ i: I -> emit(i, id()) })

        fun <I> window2(): Process1<I, Pair<Option<I>, I>> {
            fun go(prev: Option<I>): Process1<I, Pair<Option<I>, I>> =
                    await1({ i -> emit1<I, Pair<Option<I>, I>>(prev to i) plusP { go(Some(i)) } })

            return go(None)
        }

        /** Emits `sep` in between each input received. */
        fun <I> intersperse(sep: I): Process1<I, I> =
                await1<I, I>({ i -> emit1<I, I>(i) plusP { id<I>().flatMap { i -> emit1<I, I>(sep) } } plusP { emit1(i) } })



        fun <I, I2, A> narrow(value: H1<T<I, I2>.f, A>): T<I, I2>.Tf<A> = value as T<I, I2>.Tf<A>

        fun <I, I2, A> H1<T<I, I2>.f, A>.toOri(): T<I, I2>.Tf<A> = narrow(this)

        /* Again some helper functions to improve type inference. */

        fun <I, I2, O> haltT(): Tee<I, I2, O> =
                Halt<T<I, I2>.f, O>(End)

        fun <I, I2, O> awaitL(recv: (I) -> Tee<I, I2, O>,
                              fallback: () -> Tee<I, I2, O> = { haltT<I, I2, O>() }): Tee<I, I2, O> =
                await<T<I, I2>.f, I, O>(L())
                {
                    when {
                        it is Left && it.get == End -> fallback()
                        it is Left -> Halt(it.get)
                        it is Right -> Try { recv(it.get) }
                        else -> throw RuntimeException("Unreachable")
                    }
                }

        fun <I, I2, O> awaitR(recv: (I2) -> Tee<I, I2, O>,
                              fallback: () -> Tee<I, I2, O> = { haltT<I, I2, O>() }): Tee<I, I2, O> =
                await<T<I, I2>.f, I2, O>(R())
                {
                    when {
                        it is Left && it.get == End -> fallback()
                        it is Left -> Halt(it.get)
                        it is Right -> Try { recv(it.get) }
                        else -> throw RuntimeException("Unreachable")
                    }
                }

        fun <I, I2, O> emitT(h: O, tl: Tee<I, I2, O> = haltT<I, I2, O>()): Tee<I, I2, O> =
                emit(h, tl)

        fun <I, I2, O> zipWith(f: (I, I2) -> O): Tee<I, I2, O> =
                awaitL<I, I2, O>({ i ->
                    awaitR({ i2 -> emitT(f(i, i2)) })
                }).repeat()

        fun <I, I2> zip(): Tee<I, I2, Pair<I, I2>> = zipWith({ i1, i2 -> i1 to i2 })

        /* Ignores all input from left. */
        fun <I, I2> passR(): Tee<I, I2, I2> = awaitR({ emitT(it, passR()) })

        /* Ignores input from the right. */
        fun <I, I2> passL(): Tee<I, I2, I> = awaitL({ emitT(it, passL()) })

        /* Alternate pulling values from the left and the right inputs. */
        fun <I> interleaveT(): Tee<I, I, I> =
                awaitL<I, I, I>({ i ->
                    awaitR({ i2 -> emitT<I, I, I>(i) plusP { emitT(i2) } })
                }).repeat()


        /* A `Sink` which writes input strings to the given file. */
        fun fileW(file: String, append: Boolean = false): Sink<IOU, String> =
                resource<FileWriter, (String) -> Process<IOU, Unit>>(
                        IOF.IO { FileWriter(file, append) },
                        { w -> constant { s: String -> eval<IOU, Unit>(IOF.IO { w.write(s) }) } },
                        { w -> eval_(IOF.IO { w.close() }) })

        /* The infinite, constant stream. */
        fun <A> constant(a: A): Process<IOU, A> =
                eval(IOF.IO{ a }).flatMap { a -> Emit(a, constant(a)) }

        /* Exercise 12: Implement `join`. Notice this is the standard monadic combinator! */
        fun <F, A> join(p: Process<F, Process<F, A>>): Process<F, A> =
                p.flatMap { pa -> pa }

    }
}


/*
 * We now have nice, resource safe effectful sources, but we don't
 * have any way to transform them or filter them. Luckily we can
 * still represent the single-input `Process` type we introduced
 * earlier, which we'll now call `Process1`.
 */

class Is<I> {
    inner class f

    inner class IsfK : H1<f, I>

    val Get = IsfK()
}

fun <I> Get(): H1<Is<I>.f, I> = Is<I>().Get

typealias Process1<I, O> = Process<Is<I>.f, O>



/*
We sometimes need to construct a `Process` that will pull values
from multiple input sources. For instance, suppose we want to
'zip' together two files, `f1.txt` and `f2.txt`, combining
corresponding lines in some way. Using the same trick we used for
`Process1`, we can create a two-input `Process` which can request
values from either the 'left' stream or the 'right' stream. We'll
call this a `Tee`, after the letter 'T', which looks like a
little diagram of two inputs being combined into one output.
 */

class T<I, I2> {
    abstract inner class Tf<X> : H1<f, X> {
        abstract val get: Either<(I) -> X, (I2) -> X>
    }

    inner class f

    val L = object : Tf<I>() {
        override val get: Either<(I) -> I, (I2) -> I> = Left({ i: I -> i })
    }
    val R = object : Tf<I2>() {
        override val get: Either<(I) -> I2, (I2) -> I2> = Right({ i: I2 -> i })
    }
}

fun <I, I2> L() = T<I, I2>().L
fun <I, I2> R() = T<I, I2>().R

typealias Tee<I, I2, O> = Process<T<I, I2>.f, O>


/*
Our `Process` type can also represent effectful sinks (like a file).
A `Sink` is simply a source of effectful functions! See the
definition of `to` in `Process` for an example of how to feed a
`Process` to a `Sink`.
 */

typealias Sink<F, O> = Process<F, (O) -> Process<F, Unit>>


    /*
     * An example use of the combinators we have so far: incrementally
     * convert the lines of a file from fahrenheit to celsius.
     */

fun fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

val converter: Process<IOU, Unit> =
        lines("fahrenheit.txt")
                .filter { line -> !line.startsWith("#") && !line.trim().isEmpty() }
                .map { line -> fahrenheitToCelsius(line.toDouble()).toString() }
                .pipe(intersperse("\n"))
                .to(fileW("celsius.txt"))
                .drain()

    /*
More generally, we can feed a `Process` through an effectful
channel which returns a value other than `Unit`.
     */

typealias Channel<F, I, O> = Process<F, (I) -> Process<F, O>>

    /*
     * Here is an example, a JDBC query runner which returns the
     * stream of rows from the result set of the query. We have
     * the channel take a `Connection -> PreparedStatement` as
     * input, so code that uses this channel does not need to be
     * responsible for knowing how to obtain a `Connection`.
     */

fun query(connection: IO<Connection>)
        : Channel<IOU, (Connection) -> PreparedStatement, Map<String, Any>> =
        resource_(connection,
                { conn ->
                    constant { q: (Connection) -> PreparedStatement ->
                        resource_(
                                IOF.IO {
                                    val rs = q(conn).executeQuery()
                                    val ncols = rs.metaData.columnCount
                                    val cols = (1..ncols).map { rs.metaData.getColumnName(it) }
                                    rs to cols
                                },
                                { (rs, cols) ->
                                    fun step() = if (!rs.next()) None
                                    else Some(cols.map { c -> c to (rs.getObject(c) as Any) }.toMap())

                                    fun rows(): Process<IOU, Map<String, Any>> =
                                            eval(IOF.IO { step() }).flatMap {
                                                when (it) {
                                                    is None -> Halt<IOU, Map<String, Any>>(End)
                                                    is Some -> Emit(it.get, rows())
                                                }
                                            }

                                    rows()
                                },
                                { p -> IOF.IO { p.first.close() } }) // close the ResultSet
                    }
                },
                { c -> IOF.IO { c.close() } })

    /*
     * We can allocate resources dynamically when defining a `Process`.
     * As an example, this program reads a list of filenames to process
     * _from another file_, opening each file, processing it and closing
     * it promptly.
     */

    val convertAll: Process<IOU, Unit> =
            fileW("celsius.txt").once().flatMap { out ->
                lines("fahrenheits.txt").flatMap { file ->
                    lines(file).map { line -> fahrenheitToCelsius(line.toDouble()) }.flatMap { celsius ->
                        out(celsius.toString())
                    }
                }
            }.drain<Unit>()


    /*
     * Just by switching the order of the `flatMap` calls, we can output
     * to multiple files.
     */
    val convertMultisink: Process<IOU, Unit> =
            lines("fahrenheits.txt").flatMap { file ->
                lines(file).map { line -> fahrenheitToCelsius(line.toDouble()) }
                        .map { it.toString() }.to(fileW("$file.celsius"))
            }.drain<Unit>()

    /*
     * We can attach filters or other transformations at any point in the
     * program, for example:
     */
    val convertMultisink2: Process<IOU, Unit> =
            lines("fahrenheits.txt").flatMap { file ->
                lines(file).filter { !it.startsWith("#") }
                        .map { line -> fahrenheitToCelsius(line.toDouble()) }
                        .filter { it > 0 }
                        .map { it.toString() }
                        .to(fileW("$file.celsius"))
            }.drain<Unit>()

fun main(arg: Array<String>) {
    val p = eval(IOF.IO { println("woot"); 1 }).repeat()
    val p2 = eval(IOF.IO { println("cleanup"); 2 }).onHalt {
        when (it) {
            is Kill -> {
                println { "cleanup was killed, instead of bring run" }
                Halt(Kill)
            }
            else -> Halt(it)
        }
    }

    println { runLog(p2.onComplete{ p2 }.onComplete{ p2 }.take(1).take(1)) }
    println { runLog(converter) }
    // println { Process.collect(Process.convertAll) }
}
