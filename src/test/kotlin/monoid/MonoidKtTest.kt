package monoid

import fj.Monoid
import fj.test.Arbitrary
import fj.test.CheckResult
import org.junit.Test

import org.junit.Assert.*

/**
 * Created by yume on 16-12-30.
 */
class MonoidKtTest {
    @Test
    fun monoidLaws() {
        val gen = Arbitrary.arbInteger
        CheckResult.summary.println(monoidLaws(intAddition, gen).check())
    }

}