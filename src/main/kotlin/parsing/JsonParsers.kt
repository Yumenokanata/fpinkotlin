package parsing

import findPrefixOf
import parsing.JsonExample.jsonTxt
import fj.data.Either
import fj.data.List
import parsing.JsonExample.malformedJson1
import parsing.JsonExample.malformedJson2

/**
 * Created by yume on 16-12-21.
 */

sealed class JSON {
    object JNull : JSON()
    data class JNumber(val num: Double) : JSON()
    data class JString(val s: String) : JSON()
    data class JBool(val bool: Boolean) : JSON()
    data class JArray(val list: List<JSON>) : JSON()
    data class JObject(val map: Map<String, JSON>) : JSON()

    companion object {
        fun Parsers.tok(s: String): Parser<String> = run { token(string(s)) }

        fun jsonParser(p: Parsers): Parser<JSON> = p.run { root(skipL(whitespace(), { obj() or array() })) }

        fun Parsers.array(): Parser<JSON> = scope("array", surround(tok("["), tok("]")) {
            sep(value(), tok(",")).map { vs -> JArray(vs) }
        })

        fun Parsers.obj(): Parser<JSON> = scope("object", surround(tok("{"), tok("}")) {
            sep(keyval(), tok(",")).map { kvs -> JObject(kvs.toMap()) }
        })

        fun Parsers.keyval(): Parser<Pair<String, JSON>> =
                product(escapedQuoted(), { skipL(tok(":"), { value() }) })

        fun Parsers.lit(): Parser<JSON> = scope("literal",
                tok("null").cast(JNull) or
                        double().map { JNumber(it) } or
                        escapedQuoted().map { JString(it) } or
                        tok("true").cast(JBool(true)) or
                        tok("false").cast(JBool(false)))

        fun Parsers.value(): Parser<JSON> = run { lit() or obj() or array() }
    }
}

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


object JsonExample {
    val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

    val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""

    val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""
}

fun <E, R> printResult(e: Either<E, R>) =
        e.either({ println(it) }, { println(it) })

fun main(args: Array<String>) {
    val p = MyParsers()
    val json: Parser<JSON> = JSON.jsonParser(p)
    printResult(p.run { json.run(jsonTxt) })
    printResult(p.run { json.run(jsonTxt) } )
    println("--")
    printResult(p.run { json.run(malformedJson1) } )
    println("--")
    printResult(p.run { json.run(malformedJson2) } )
}

