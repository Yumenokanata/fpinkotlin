import laziness.Stream

/**
 * Created by yume on 17-1-12.
 */

typealias Effect1<A> = (A) -> Unit


/**
 * Returns true if any element of the given stream is true.

 * @param l A stream to check for any element being true.
 * *
 * @return true if any element of the given stream is true. False otherwise.
 */
fun or(l: Stream<Boolean>): Boolean = l.foldRight({ false }) { a, b -> a or b() }