# logic-dsl
A Domain Specific Language for Zero Order Logic

# Basic Usage
1. Download or clone the project 
```
git clone https://github.com/mohammedi-haroune/logic-dsl.git
```

2. Run the provided example (below)
```
sbt run
```

Or

3. Open the project using an IDE (intellij IDEA for example) 
4. Enjoy !

# Basic Exmaple
```scala
import scala.collection.Set
import scala.language.implicitConversions
import com.usthb.logic.Literals._
import com.usthb.logic.Formula._
import com.usthb.logic.Default._
import com.usthb.logic._

object Main extends App{
  val f = P -> (Q & R) -> T
  println(s"the cnf form of $f is ${f.toCNF}")
  
  val value =
    withValues(
      P := true,
      Q := true,
    ) eval (P ⊃ Q)
  
  println(s"v = ${value}")

  val e = Set(P, Q, P -> R, (P & Q) -> V)
      
  println(s"clause = ${e.toClause}")
  
  println(s"DMACS = ${e.toDMACS}")
  
  e.write("test.cnf")
  
  val f2 = (P & True) -> (P & True) & Q | False
  
  println(s"shorthand of f2 = ${f2.shorthand}")

  val world = Set(A)
  val defaults = Array(
    (A * B) / C,
    (A * !C) / !B
  )

  val delta = Theory(world, defaults)
  println(s"extentions = ${delta.extentions}")
}

```

# Contribution
All contributions, pull requests, issues, ideas are welcome

# License
MIT
