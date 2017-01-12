package applicative

import fj.Show
import datastructures.List
import errorhanding.Option
import org.junit.Test
import monad.*

/**
 * Created by yume on 17-1-2.
 */
class ApplicativeTest {
    @Test
    fun testZipWithIndex() {
        val list = List.apply(1, 2, 3, 4, 5, 6, 7)
//        Show.listShow(Show.anyShow<Pair<Int, Int>>()).println(listTraverse.zipWithIndex_(list))
//        Show.listShow(Show.anyShow<Pair<Int, Int>>()).println(listTraverse.zipWithIndex(list))
        println(listTraverse.zipWithIndex(list))
    }

    @Test
    fun testToList() {
        val list = List.apply(1, 2, 3, 4, 5, 6, 7)
//        Show.listShow(Show.intShow).println(listTraverse.toList_(list))
//        Show.listShow(Show.intShow).println(listTraverse.toList(list))
        println(listTraverse.toList(list))
    }

    //合成List<Option<T>>类型的Traverse函子
    @Test
    fun testCompose() {
        val compose: Traverse<H1<List.T, Option.T>> = listTraverse.compose(optionTraverse)
        val data = HTypeFG<List.T, Option.T>().HTypeFG(
                List.apply<H1<Option.T, Int>>(
                        Option.some(1),
                        Option.none<Int>(),
                        Option.some(3)))
        val result = compose.toList(data)
        println(result)
    }
}
