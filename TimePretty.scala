package archontophoenix.pretty.test

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import archontophoenix.pretty._

/**
 * Timing test for prettyprinting.
 *
 * Timings should be approximately linear in the width and depth of the output.
 */
class TimeSpec extends WordSpec with ShouldMatchers {
  def powersOf10 (maxPower: Int): List[Int] =
    (1 to maxPower).foldLeft(List(1)) {
      (lst: List[Int], _: Int) => lst :+ lst.last * 10
    }
  val powersOf10To5 = powersOf10(5)
  val powersOf10To4 = powersOf10(4)
  abstract class InfiniFun (override val toString: String) {
    def apply (depth: Int, maxDepth: Int): Prettify
    def powersOf10 = powersOf10To5
  }
  class BitBucketWriter extends java.io.Writer {
    var totalLen: Long = 0
    def write(cbuf: Array[Char], off: Int, len: Int) {
      totalLen += len - off
    }
    def flush {
    }
    def close {
    }
  }
  "Linear time scaling" should {
    def timings (width: Int, depth: Int, fun: InfiniFun, run: Int) {
      ("occur at width " + width + ", depth " + depth + ", for " + fun +
          " on run " + run) in {
        val start = System.currentTimeMillis
        val out = new BitBucketWriter
        var took = 0L
        var iterations = 0L
        do {
          fun(1,depth).render(Unlimited,out)
          iterations += 1
          took = System.currentTimeMillis - start
        } while (took < 1000)
        println(
          "\nFor " + fun + " at width " + width + ", depth " + depth + ":\n" +
            out.totalLen + " chars, scaling factor:\n" +
            (took.toDouble / (iterations * width * depth)))
      }
    }
    val funs =
      List(
        new InfiniFun("ConcatRecurseLeft") {
          def apply (depth: Int, maxDepth: Int): Prettify =
            if (depth > maxDepth) Empty
            else Concat(apply(depth + 1,maxDepth)," " + depth)
        },
        new InfiniFun("ConcatRecurseRight") {
          def apply (depth: Int, maxDepth: Int): Prettify =
            if (depth > maxDepth) Empty
            else Concat(" " + depth,apply(depth + 1,maxDepth))
        },
        new InfiniFun("IndentRecurseLeft") {
          def apply (depth: Int, maxDepth: Int): Prettify =
            if (depth > maxDepth) Empty
            else Indent(apply(depth + 1,maxDepth)," " + depth)
        },
        new InfiniFun("IndentRecurseRight") {
          def apply (depth: Int, maxDepth: Int): Prettify =
            if (depth > maxDepth) Empty
            else Indent(" " + depth,apply(depth + 1,maxDepth))
        },
        new InfiniFun("BrokenRecurseMiddle") {
          def apply (depth: Int, maxDepth: Int): Prettify =
            if (depth > maxDepth) Empty
            else
              Indent(
                SoftBreak(" >"),depth.toString,apply(depth + 1,maxDepth),
                SoftBreak(" <"),depth.toString)
          override def powersOf10 = powersOf10To4
        })
    println
    for (r <- 1 to 3) {
      for (f <- funs; w <- f.powersOf10; d <- f.powersOf10) timings(w,d,f,r)
      println
    }
  }
}
