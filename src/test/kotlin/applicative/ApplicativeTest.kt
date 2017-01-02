package applicative

import fj.Show
import fj.data.List
import fj.data.Option
import monad.HTypeList
import org.junit.Test
import monad.*

/**
 * Created by yume on 17-1-2.
 */
class ApplicativeTest {
    @Test
    fun testZipWithIndex() {
        val list = List.list(1, 2, 3, 4, 5, 6, 7)
        Show.listShow(Show.anyShow<Pair<Int, Int>>()).println(narrow(listTraverse.zipWithIndex_(HTypeList(list))).l)
        Show.listShow(Show.anyShow<Pair<Int, Int>>()).println(narrow(listTraverse.zipWithIndex(HTypeList(list))).l)
    }

    @Test
    fun testToList() {
        val list = List.list(1, 2, 3, 4, 5, 6, 7)
        Show.listShow(Show.intShow).println(listTraverse.toList_(HTypeList(list)))
        Show.listShow(Show.intShow).println(listTraverse.toList(HTypeList(list)))
    }

    //合成List<Option<T>>类型的Traverse函子
    @Test
    fun testCompose() {
        val compose: Traverse<H1<ListU, OptionU>> = listTraverse.compose(optionTraverse)
        val data = HTypeFG<ListU, OptionU>().HTypeFG(
                List.list<H1<OptionU, Int>>(
                        Option.some(1).toHType(),
                        Option.none<Int>().toHType(),
                        Option.some(3).toHType()).toHType())
        val result = compose.toList(data)
        println(result)
    }
}
