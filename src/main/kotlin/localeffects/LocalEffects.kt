package localeffects

/**
 * Created by yume on 17-1-4.
 */

abstract class ST<S, A> {
    protected abstract fun run(s: S): Pair<A, S>

    fun <B> map(f: (A) -> B): ST<S, B> = object : ST<S, B>() {
        override fun run(s: S): Pair<B, S> {
            val (a, s1) = this@ST.run(s)
            return f(a) to s1
        }
    }

    fun <B> flatMap(f: (A) -> ST<S, B>): ST<S, B> = object : ST<S, B>() {
        override fun run(s: S): Pair<B, S> {
            val (a, s1) = this@ST.run(s)
            return f(a).run(s1)
        }
    }

    companion object {
        operator fun <S, A> invoke(a: () -> A): ST<S, A> = object : ST<S, A>() {
            val memo by lazy(a)
            override fun run(s: S): Pair<A, S> = memo to s
        }

        fun <A> runST(st: RunnableST<A>): A =
                st<Unit>().run(Unit).first
    }
}

abstract class STRef<S, A> {
    protected abstract var cell: A

    fun read(): ST<S, A> = ST { cell }

    fun write(a: () -> A): ST<S, Unit> = object : ST<S, Unit>() {
        override fun run(s: S): Pair<Unit, S> {
            cell = a()
            return Unit to s
        }
    }

    companion object {
        operator fun <S, A> invoke(a: A): ST<S, STRef<S, A>> = ST { object : STRef<S, A>() {
            override var cell = a
        }}
    }
}

interface RunnableST<A> {
    operator fun <S> invoke(): ST<S, A>
}

val p = object : RunnableST<Pair<Int, Int>> {
    override fun <S> invoke(): ST<S, Pair<Int, Int>> =
            STRef<S, Int>(1).flatMap { r1 ->
                STRef<S, Int>(2).flatMap { r2 ->
                    r1.read().flatMap { x ->
                        r2.read().flatMap { y ->
                            r1.write { y + 1 }.flatMap {
                                r2.write { x + 1 }.flatMap {
                                    r1.read().flatMap { a ->
                                        r2.read().map { b ->
                                            a to b
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

}

val p2 = object : RunnableST<STRef<*, Int>> {
    override fun <S> invoke(): ST<S, STRef<*, Int>> = STRef<S, Int>(1).map { it }
}

abstract class STArray<S, A> {
    protected abstract val value: Array<A>

    val size: ST<S, Int> = ST { value.size }

    fun write(i: Int, a: A): ST<S, Unit> = object : ST<S, Unit>() {
        override fun run(s: S): Pair<Unit, S> {
            value[i] = a
            return Unit to s
        }
    }

    fun read(i: Int): ST<S, A> = ST { value[i] }

    val freeze: ST<S, List<A>> = ST { value.toList() }

    companion object {
        inline fun <S, reified A> invoke(sz: Int, v: A): ST<S, STArray<S, A>> =
                ST {
                    object : STArray<S, A>() {
                        override val value: Array<A>
                            get() = Array(sz) { v }
                    }
                }
    }
}