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
        fun <S, A> apply(a: () -> A): ST<S, A> = object : ST<S, A>() {
            val memo by lazy(a)
            override fun run(s: S): Pair<A, S> = memo to s
        }

        fun <A> runST(st: RunnableST<A>): A =
                st.apply<Unit>().run(Unit).first
    }
}

abstract class STRef<S, A> {
    protected abstract var cell: A

    fun read(): ST<S, A> = ST.apply { cell }

    fun write(a: () -> A): ST<S, Unit> = object : ST<S, Unit>() {
        override fun run(s: S): Pair<Unit, S> {
            cell = a()
            return Unit to s
        }
    }

    companion object {
        fun <S, A> apply(a: A): ST<S, STRef<S, A>> = ST.apply { object : STRef<S, A>() {
            override var cell = a
        }}
    }
}

interface RunnableST<A> {
    fun <S> apply(): ST<S, A>
}