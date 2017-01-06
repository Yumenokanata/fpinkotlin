package parsing

import fj.Ord
import fj.data.Either
import fj.data.List
import fj.data.Option
import fj.test.Gen
import fj.test.Property
import fj.test.Property.prop
import fj.test.Property.property
import monad.cons
import java.util.regex.Pattern

/**
 * Created by yume on 16-12-21.
 */

typealias Parser<T> = (ParseState) -> Result<out T>

sealed class Result<T> {
    data class Success<A>(val a: A, val length: Int) : Result<A>()
    data class Failure<A>(val error: ParseError, val isCommitted: Boolean) : Result<A>()

    fun mapError(f: (ParseError) -> ParseError): Result<T> =
            when(this) {
                is Failure -> Failure(f(error), isCommitted)
                else -> this
            }

    fun uncommit(): Result<T> =
            when(this) {
                is Failure -> Failure(error, false)
                else -> this
            }

    fun addCommit(isCommitted: Boolean): Result<T> =
            when(this) {
                is Failure -> copy(isCommitted = this.isCommitted || isCommitted)
                else -> this
            }

    fun advanceSuccess(n: Int): Result<T> =
            when(this) {
                is Success -> copy(length = length + n)
                else -> this
            }
}

interface Parsers {
    fun <T> Parser<T>.run(input: String): Either<ParseError, T>

    fun string(s: String): Parser<String>

    infix fun <A> Parser<A>.or(p2: Parser<A>): Parser<A>

    fun <A> succeed(a: A): Parser<A>

    fun <A> slice(p: Parser<A>): Parser<String>

    fun <A, B> Parser<A>.flatMap(f: (A) -> Parser<B>): Parser<B>

    fun regex(r: Regex): Parser<String>

    fun <A> label(msg: String, p: Parser<A>): Parser<A>

    fun <A> scope(msg: String, p: Parser<A>): Parser<A>

    fun <A> attempt(p: Parser<A>): Parser<A>


    fun <A, B> Parser<A>.cast(b: B): Parser<B> = slice(this).map { b }

    fun <A, B> Parser<A>.map(f: (A) -> B): Parser<B> =
            flatMap { a -> succeed(f(a)) }

    fun <A, B> product(p1: Parser<A>, p2: () -> Parser<B>): Parser<Pair<A, B>> =
            p1.flatMap { a -> p2().map { b -> Pair(a, b) } }

    fun <T> Parser<T>.many(): Parser<List<T>> =
            map2(this, { this.many() }) { f, l -> List.cons(f, l) }.or<List<T>>(succeed(List.nil()))

    fun <A> listOfN(n: Int, p: Parser<A>): Parser<List<A>> =
            when {
                n <= 0 -> succeed(List.nil())
                else -> map2(p, { listOfN(n - 1, p) }) { f, l -> List.cons(f, l) }
            }

    fun <A, B, C> map2(p1: Parser<A>, p2: () -> Parser<B>, f: (A, B) -> C): Parser<C> =
            p1.flatMap { a -> p2().map { b -> f(a, b) } }

    fun char(c: Char): Parser<Char> =
            string(c.toString()).map { it.first() }

    fun <A> Parser<A>.many1(): Parser<List<A>> =
            map2(this, { many() }) { f, l -> List.cons(f, l) }

    //Test
    fun <A> equal(p1: Parser<A>, p2: Parser<A>, inGen: Gen<String>): Property =
            property(inGen, { s -> prop(p1.run(s) == p2.run(s))})

    fun <A> mapLaw(p: Parser<A>, inGen: Gen<String>): Property =
            equal(p, p.map({ it }), inGen)

    private fun String.r(): Parser<String> = regex(this.toRegex())

    /** Sequences two parsers, ignoring the result of the first.
     * We wrap the ignored half in parsing.slice, since we don't care about its result.
     * *>
     */
    fun <A, B> skipL(start: Parser<A>, p2: () -> Parser<B>): Parser<B> =
            map2(slice(start), p2){ _, b -> b }

    /** Sequences two parsers, ignoring the result of the second.
     * We wrap the ignored half in parsing.slice, since we don't care about its result.
     * <*
     */
    fun <A, B> skipR(p: Parser<A>, end: () -> Parser<B>): Parser<A> =
            map2(p, { slice(end()) }) { a, _ -> a }

    fun <A> opt(p: Parser<A>): Parser<Option<A>> =
            p.map { Option.some(it) }.or<Option<A>>(succeed(Option.none()))

    /** Parser which consumes zero or more whitespace characters. */
    fun whitespace(): Parser<String> = regex("\\s*".toRegex())

    /** Parser which consumes 1 or more digits. */
    fun digits(): Parser<String> = regex("\\d+".toRegex())

    /** Parser which consumes reluctantly until it encounters the given string. */
    fun thru(s: String): Parser<String> = regex((".*?" + Pattern.quote(s)).toRegex())

    /** Unescaped string literals, like "foo" or "bar". */
    fun quoted(): Parser<String> = skipL(string("\""), { thru("\"").map { it.dropLast(1) } })

    /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
    fun escapedQuoted(): Parser<String> =
    // rather annoying to write, left as an exercise
    // we'll just use quoted (unescaped literals) for now
            token(label("string literal", quoted()))

    /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
     * parsing.Result is left as a string to keep full precision
     */
    fun doubleString(): Parser<String> =
            token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r())

    /** Floating point literals, converted to a `Double`. */
    fun double(): Parser<Double> =
            label("double literal", doubleString().map { it.toDouble() })

    /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
    fun <A> token(p: Parser<A>): Parser<A> =
            skipR(attempt(p), { whitespace() })

    /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
    fun <A> sep(p: Parser<A>, p2: Parser<Any>): Parser<List<A>> = // use `Parser<Any>` since don't care about result type of separator
            sep1(p,p2).or(succeed(List.nil<A>()))

    /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
    fun <A> sep1(p: Parser<A>, p2: Parser<Any>): Parser<List<A>> =
            map2(p, { skipL(p2, { p }).many() }) { h, l -> h cons l }

    /** Parses a sequence of left-associative binary operators with the same precedence. */
    fun <A> opL(p: Parser<A>, op: Parser<(A, A) -> A>): Parser<A> =
            map2(p, { product(op, { p }).many() }) { h, t -> t.foldLeft({ a, b -> b.first(a, b.second) }, h) }

    /** Wraps `p` in start/stop delimiters. */
    fun <A> surround(start: Parser<out Any>, stop: Parser<out Any>, p: () -> Parser<A>) =
            skipR(skipL(start, p), { stop })

    /** A parser that succeeds when given empty input. */
    fun eof(): Parser<String> =
            label("unexpected trailing characters", "\\Z".r())

    /** The root of the grammar, expects no further input following `p`. */
    fun <A> root(p: Parser<A>): Parser<A> =
            skipR(p, this::eof)
}

data class Location(val input: String, val offset: Int = 0) {
    val line by lazy { input.slice(0..offset).count({ it == '\n' }) + 1 }
    val col by lazy {
        val lineStart = input.slice(0..offset).lastIndexOf('\n')
        when(lineStart) {
            -1 -> offset + 1
            else -> offset - lineStart
        }
    }

    fun toError(msg: String): ParseError = ParseError(List.single(Pair(this, msg)))

    fun advanceBy(n: Int): Location = copy(offset = offset + n)

    fun currentLine(): String =
            if (input.length > 1) input.lines().drop(line - 1).first()
            else ""

    fun columnCaret() = " ".repeat(col - 1) + "^"
}

fun String.slice(from: Int, until: Int): String {
    val start = if (from < 0) 0 else from
    if (until <= start || start >= this.length)
        return ""

    val end = if (until > length) length else until
    return this.substring(start, end)
}

data class ParseState(val loc: Location) {
    fun advanceBy(numChars: Int): ParseState =
            copy(loc = loc.copy(offset = loc.offset + numChars))
    val input: String = loc.input.substring(loc.offset)
    fun slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
}

data class ParseError(val stack: List<Pair<Location, String>>) {
    fun push(loc: Location, msg: String): ParseError =
            copy(stack = List.cons(Pair(loc, msg), stack))

    fun label(s: String): ParseError =
            ParseError(latestLoc()?.let { List.single(Pair(it, s)) } ?: List.nil())

    fun latestLoc(): Location? =
            latest()?.first

    fun latest(): Pair<Location, String>? =
            stack.lastOrNull()

    /**
    Display collapsed error stack - any adjacent stack elements with the
    same location are combined on one line. For the bottommost error, we
    display the full line, with a caret pointing to the column of the error.
    Example:

    1.1 file 'companies.json'; array
    5.1 object
    5.2 key-value
    5.10 ':'

    { "MSFT" ; 24,
     */
    override fun toString() =
            if (stack.isEmpty) "no error message"
            else {
                val collapsed = collapseStack(stack)
                val context = collapsed.lastOption.map { "\n\n" + it.first.currentLine() }.orSome("") +
                        collapsed.lastOption.map { "\n" + it.first.columnCaret() }.orSome("")
                collapsed.map { (loc, msg) -> loc.line.toString() + "." + loc.col + " " + msg }.joinToString("\n") +
                        context
            }

    /* Builds a collapsed version of the given error stack -
     * messages at the same location have their messages merged,
     * separated by semicolons */
    fun collapseStack(s: List<Pair<Location, String>>): List<Pair<Location, String>> =
            s.groupBy { it.first }
                    .map { it.map { it.second }.joinToString(separator = "; ") }
                    .toList().sort(Ord.p2Ord1(Ord.intOrd.contramap { it.offset }))
                    .map { it._1() to it._2() }

    fun formatLoc(l: Location): String = l.line.toString() + "." + l.col
}

val <A> List<A>.lastOption: Option<A>
        get() = Option.fromNull(this.last())