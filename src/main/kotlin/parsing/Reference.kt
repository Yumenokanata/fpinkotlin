package parsing

import findPrefixOf
import fj.data.Either

/**
 * Created by yume on 17-1-4.
 */

class MyParsers : Parsers {
    fun firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int {
        var i = 0
        while (i < s1.length && i < s2.length && i+offset < s1.length) {
            if (s1[i+offset] != s2[i]) return i
            i += 1
        }
        return if (s1.length-offset >= s2.length) -1
        else s1.length - offset
    }

    override fun <T> Parser<T>.run(input: String): Either<ParseError, T> {
        val result = this(ParseState(Location(input)))
        return when(result) {
            is Result.Success -> Either.right(result.a)
            is Result.Failure -> Either.left(result.error)
        }
    }

    override fun string(w: String): Parser<String> =
            { state ->
                val msg = "'" + w + "'"
                val i = firstNonmatchingIndex(state.loc.input, w, state.loc.offset)
                if (i == -1) // they matched
                    Result.Success(w, w.length)
                else
                    Result.Failure(state.loc.advanceBy(i).toError(msg), i != 0)
            }

    override fun <A> Parser<A>.or(p2: Parser<A>): Parser<A> =
            { s ->
                val resultX = this(s)
                when(resultX) {
                    is Result.Failure -> if(resultX.isCommitted) resultX else p2(s)
                    else -> resultX
                }
            }

    override fun <A> succeed(a: A): Parser<A> =
            { Result.Success(a, 0) }

    override fun <A> slice(p: Parser<A>): Parser<String> =
            { location ->
                val result = p(location)
                when(result) {
                    is Result.Success -> Result.Success(location.slice(result.length), result.length)
                    is Result.Failure -> Result.Failure(result.error, result.isCommitted)
                }
            }

    override fun <A, B> Parser<A>.flatMap(f: (A) -> Parser<B>): Parser<B> =
            { s ->
                val resultA = this(s)
                when(resultA) {
                    is Result.Success -> f(resultA.a)(s.advanceBy(resultA.length))
                            .addCommit(resultA.length != 0)
                            .advanceSuccess(resultA.length)
                    is Result.Failure -> Result.Failure<B>(resultA.error, resultA.isCommitted)
                }
            }

    override fun regex(r: Regex): Parser<String> =
            { state ->
                val match = r.findPrefixOf(state.input)
                if (match != null)
                    Result.Success(match, match.length)
                else
                    Result.Failure(state.loc.toError("Expected: " + r), false)
            }

    override fun <A> label(msg: String, p: Parser<A>): Parser<A> =
            { s -> p(s).mapError { it.label(msg) } }

    override fun <A> scope(msg: String, p: Parser<A>): Parser<A> =
            { s -> p(s).mapError { it.push(s.loc, msg) } }

    override fun <A> attempt(p: Parser<A>): Parser<A> =
            { s -> p(s).uncommit() }
}