/**
 * Created by yume on 17-1-3.
 */

infix fun <P1, IP, R> Function1<P1, IP>.andThen(f: (IP) -> R): (P1) -> R = forwardCompose(f)

infix fun <P1, IP, R> Function1<P1, IP>.forwardCompose(f: (IP) -> R): (P1) -> R {
    return { p1: P1 -> f(this(p1)) }
}

infix fun <IP, R, P1> Function1<IP, R>.compose(f: (P1) -> IP): (P1) -> R {
    return { p1: P1 -> this(f(p1)) }
}