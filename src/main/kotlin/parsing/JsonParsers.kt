package parsing

import datastructures.List
import errorhanding.Either
import parsing.JsonExample.jsonTxt
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
        e.fold({ println(it) }, { println(it) })

fun main(args: Array<String>) {
    val p = MyParsers()
    val json: Parser<JSON> = JSON.jsonParser(p)
    printResult(p.run { json.run(jsonTxt) })
    printResult(p.run { json.run(jsonTxt) })
    println("--")
    printResult(p.run { json.run(malformedJson1) } )
    println("--")
    printResult(p.run { json.run(malformedJson2) } )
}

