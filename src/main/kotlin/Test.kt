/**
 * Created by yume on 16-12-27.
 */


interface A<T> {
    fun T.test1(): Int
}

interface B<T> {
    fun T.test2(): String
}

data class U(val i: Int = 1)

object G : A<U>, B<U> {
    override fun U.test1(): Int = i

    override fun U.test2(): String = i.toString()
}

fun aj() {
    val u = U()
    G.apply { u.test1() }
}