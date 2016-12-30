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