
import java.lang.System.arraycopy

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

fun copyOfRange(a: CharArray, from: Int, to: Int): CharArray {
    val len = to - from
    if (len < 0)
        throw IllegalArgumentException(from.toString() + " > " + to)
    val copy = CharArray(len)
    arraycopy(a, from, copy, 0,
            Integer.min(a.size - from, len))
    return copy
}

val <A> kotlin.collections.List<A>.head: A
    get() = first()

val <A> kotlin.collections.List<A>.tail: kotlin.collections.List<A>
    get() = rest(this)

fun <E, L : kotlin.collections.List<E>> rest(list: L): L {
    if (list.isEmpty())
        return list

    return list.subList(1, list.size) as L
}