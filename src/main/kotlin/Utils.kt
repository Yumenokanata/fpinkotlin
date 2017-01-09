
import java.util.*
import java.util.Collections.emptyList

/**
 * Created by yume on 16-12-23.
 */

fun Regex.findPrefixOf(source: CharSequence): String? {
    val m = toPattern().matcher(source)
    return if (m.lookingAt())
        m.group()
    else
        null
}

fun <A, B, C> A.safeAs(success: (B) -> C, fail: C): C {
    val result: B? = this as? B
    return result?.let(success) ?: fail
}

val <A> Array<out A>.head: A
    get() = first()

val <A> Array<A>.tail: Array<A>
    get() = copyOf(this, 1)

@SuppressWarnings("SuspiciousSystemArraycopy", "unchecked", "ObjectEquality", "RedundantCast")
fun <T, U> copyOf(a: Array<U>, from: Int, len: Int, newType: Class<Array<T>>): Array<T> {
    val copy = if (newType as Any === Array<Any>::class.java)
        arrayOfNulls<Any>(len) as Array<T>
    else
        java.lang.reflect.Array.newInstance(newType.componentType, len) as Array<T>
    System.arraycopy(a, from, copy, 0,
            Integer.min(a.size, len))
    return copy
}

fun <T> copyOf(a: Array<T>, from: Int, len: Int = a.size - from): Array<T> {
    return copyOf<T, T>(a, from, len, a.javaClass)
}

val <A> MutableList<A>.head: A
    get() = first()

val <A> MutableList<A>.tail: MutableList<A>
    get() = rest(this)

fun <E, L : List<E>> rest(list: L): L {
    if (list.isEmpty())
        return list

    return list.subList(1, list.size) as L
}