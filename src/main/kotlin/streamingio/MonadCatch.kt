package streamingio

import errorhanding.Either
import monad.H1
import monad.Monad

interface MonadCatch<F> : Monad<F> {

    fun <A> attempt(a: H1<F, A>): H1<F, Either<Throwable, A>>

    fun <A> fail(t: Throwable): H1<F, A>
}