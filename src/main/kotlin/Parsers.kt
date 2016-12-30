import fj.data.Either
import fj.data.List
import fj.test.Gen
import fj.test.Property
import fj.test.Property.prop
import fj.test.Property.property

/**
 * Created by yume on 16-12-21.
 */

typealias Parser<T> = (Location) -> Result<T>

sealed class Result<T> {
    data class Success<A>(val a: A, val charsConsumed: Int) : Result<A>()
    data class Failure<A>(val error: ParseError, val isCommitted: Boolean = true) : Result<A>()

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
                is Success -> copy(charsConsumed = charsConsumed + n)
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



    fun <A, B> Parser<A>.map(f: (A) -> B): Parser<B> =
            flatMap { a -> succeed(f(a)) }

    fun <A, B> product(p1: Parser<A>, p2: () -> Parser<B>): Parser<Pair<A, B>> =
            p1.flatMap { a -> p2().map { b -> Pair(a, b) } }

    fun <T> Parser<T>.many(): Parser<List<T>> =
            map2(this, { this.many() }) { f, l -> List.cons(f, l) } or succeed(List.nil())

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
}

data class Location(val input: String, val offset: Int = 0) {
    val line = lazy { input.slice(0..offset + 1).count({ it == '\n' }) }
    val col = lazy {
        val lineStart = input.slice(0..offset + 1).lastIndexOf('\n')
        when(lineStart) {
            -1 -> offset + 1
            else -> offset - lineStart
        }
    }

    fun toError(msg: String): ParseError = ParseError(List.single(Pair(this, msg)))

    fun advanceBy(n: Int): Location = copy(offset = offset + n)
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
}