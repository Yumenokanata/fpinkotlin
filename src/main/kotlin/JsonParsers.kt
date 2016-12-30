import fj.data.Either

/**
 * Created by yume on 16-12-21.
 */

sealed class JSON {
    object JNull : JSON()
    class JNumber(val num: Double) : JSON()
    class JString(val s: String) : JSON()
    class JBool(val bool: Boolean) : JSON()
    class JArray(val list: List<JSON>) : JSON()
    class JObject(val map: Map<String, JSON>) : JSON()


}

fun Location.slice(n: Int) = input.substring(offset..offset + n)

class MyParsers : Parsers {
    override fun <T> Parser<T>.run(input: String): Either<ParseError, T> {
        val result: Result<T> = this(Location(input))
        return when(result) {
            is Result.Success -> Either.right(result.a)
            is Result.Failure -> Either.left(result.error)
        }
    }

    override fun string(s: String): Parser<String> =
            { location ->
                if (location.input.substring(location.offset).startsWith(s))
                    Result.Success(s, s.length)
                else
                    Result.Failure(Location(location.input).toError("Expected: " + s))
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
                    is Result.Success -> Result.Success(location.slice(result.charsConsumed), result.charsConsumed)
                    is Result.Failure -> Result.Failure(result.error)
                }
            }

    override fun <A, B> Parser<A>.flatMap(f: (A) -> Parser<B>): Parser<B> =
            { s ->
                val resultA = this(s)
                when(resultA) {
                    is Result.Success -> f(resultA.a)(s.advanceBy(resultA.charsConsumed))
                            .addCommit(resultA.charsConsumed != 0)
                            .advanceSuccess(resultA.charsConsumed)
                    is Result.Failure -> Result.Failure<B>(resultA.error, resultA.isCommitted)
                }
            }

    override fun regex(r: Regex): Parser<String> =
            { location ->
                val match = r.findPrefixOf(location.input.substring(location.offset))
                if (match != null)
                    Result.Success(match, match.length)
                else
                    Result.Failure(Location(location.input).toError("Expected: " + r))
            }

    override fun <A> label(msg: String, p: Parser<A>): Parser<A> =
            { s -> p(s).mapError { it.label(msg) } }

    override fun <A> scope(msg: String, p: Parser<A>): Parser<A> =
            { s -> p(s).mapError { it.push(s, msg) } }

    override fun <A> attempt(p: Parser<A>): Parser<A> =
            { s -> p(s).uncommit() }
}

