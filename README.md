# fpinkotlin

《Scala函数式编程》这本书的Kotlin实践，在[highj](https://github.com/svn2github/highj)项目的启发下用局部临时类型的方式实现了类型构造器，
在Java的类型系统下实现了函数式范式下的几个基本抽象(函子、应用函子、Monoid、Monad等)。  

使用[functionalJava](https://github.com/functionaljava/functionaljava)作为基础库，这个库也实现了大部分函数式结构，非常有实用性和参考性，
但同网上其他以Java实现的函数式库一样，由于类型系统的问题没有抽象出Monad等高阶结构。  

项目会根据自己的学习进度逐步完善，目前已实现《Scala函数式编程》中的：  
 1. 第一部分 5. [严格求值和惰性求值](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/parsing.main/kotlin/laziness)
 1. 第二部分 7. [纯函数式的并行计算库](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/parsing.main/kotlin/parallelism/Par.kt)
 2. 第二部分 9. [语法分析组合子(示例的JSON解析器以及错误提示已完成)](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/parsing.main/kotlin/parsing.Parsers.kt)
 3. 第三部分 10.[Monoid](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/parsing.main/kotlin/monoid/Monoid.kt)
 4. 第三部分 11.[Monad](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/parsing.main/kotlin/monad/Monad.kt)
 5. 第三部分 12.[可应用和可遍历函子（带注释）](https://github.com/Yumenokanata/fpinkotlin/blob/master/src/parsing.main/kotlin/applicative/Applicative.kt)
