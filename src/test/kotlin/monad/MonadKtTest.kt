package monad

import fj.test.Arbitrary
import fj.test.CheckResult
import fj.test.Cogen
import org.junit.Test

/**
 * Created by yume on 16-12-30.
 */
class MonadKtTest {
    @Test
    fun testMonadIdentityLaw() {
        val gen = Arbitrary.arbString
        val fGen = Arbitrary.arbF(Cogen.cogenString, Arbitrary.arbInteger)
        CheckResult.summary.println(monadIdentityLaw(gen, fGen, listMonad).check())
    }
}
