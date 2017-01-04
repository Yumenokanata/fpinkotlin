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