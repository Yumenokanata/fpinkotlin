# fpinkotlin

《Scala函数式编程》这本书的Kotlin实践，在[highj](https://github.com/svn2github/highj)项目的启发下用局部临时类型的方式实现了类型构造器，
在Java的类型系统下实现了函数式范式下的几个基本抽象(函子、应用函子、Monoid、Monad等)。  

使用[functionalJava](https://github.com/functionaljava/functionaljava)作为基础库，这个库也实现了大部分函数式结构，非常有实用性和参考性，
但同网上其他以Java实现的函数式库一样，由于类型系统的问题没有抽象出Monad等高阶结构。  


> `functionalJava`是个很棒的库, 维护多年, 但受限于Java还是有不少的硬伤.  
> 而现在有了一个更先进的库: [Arrow](https://github.com/arrow-kt/arrow), 作为新兴的函数范式库, 不仅实现了Monad, 提供了高阶类型实现的简易工具, 甚至借由Kotlin的协程实现了Scala的`for`语法糖, 已经具有了相当的实用性, 同时也是个非常活跃的库, 更新非常快. 如果你对函数范式感兴趣同时又使用Kotlin作为主语言, 这是个不可错过的库  


项目会根据自己的学习进度逐步完善，目前已实现《Scala函数式编程》中的：  
 1. 第一部分 3. [函数式数据结构(List)](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/main/kotlin/datastructures)
 2. 第一部分 5. [严格求值和惰性求值](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/main/kotlin/laziness)
 3. 第二部分 7. [纯函数式的并行计算库](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/main/kotlin/parallelism/Par.kt)
 4. 第二部分 9. [语法分析组合子(示例的JSON解析器以及错误提示已完成)](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/main/kotlin/parsing.Parsers.kt)
 5. 第三部分 10.[Monoid](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/main/kotlin/monoid/Monoid.kt)
 6. 第三部分 11.[Monad](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/main/kotlin/monad/Monad.kt)
 7. 第三部分 12.[可应用和可遍历函子（带注释）](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/main/kotlin/applicative/Applicative.kt)
 7. 第四部分 15.[流式处理与增量I/O](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/main/kotlin/streamingio/StreamingIO.kt)

---

### Log

#### 2018-09-07

时隔一年多终于更新啦, 新增了《Scala函数式编程》最后一章`流式处理与增量I/O`, 这是整本书集大成的一章, 主要实现了Process流, 用不可思议的手法实现了纯函数式的通用处理流, 相信大家可以从其中的思想获益匪浅  
同时本章也对kotlin(java)泛型系统也提出了很多挑战, 本人也在完成本章代码中学习了很多高阶类型的模拟经验.  
  
另外15章的代码其实去年就完成了, 但期间丢失过一次数据, 导致这部分代码全部丢失, 一度让人完全想放弃继续完成. 不过最终这部分经验的重要性还是让笔者重新打起精神完成了这一章的代码  
  
希望这些代码能给予学习的你带来一些启发或者帮助  