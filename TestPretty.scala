package archontophoenix.pretty

/**
 * ScalaTest-based tests for the prettyprinter in this package's superpackage.
 *
 * Also, a ScalaCheck-based generator of random expressions to test the
 * prettyprinter visually in `CheckPretty`, and a timing test in `TimeSpec`.
 */
package object test

package test {

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import archontophoenix.pretty._

/** Contains utilities for prettyprinter specs. */
abstract class PrettySpec extends WordSpec with ShouldMatchers {
  def show (sp: Prettify, lim: Limits = DefaultLimits): String =
    sp.render(lim).toString
  def show (sp: Prettify, maxWidth: Int): String = show(sp,Limits(maxWidth))
  def say (s: String) {
    println(s)
    info(s)
  }
  def sayPrettify (sp: Prettify) {
    say(sp.debugString + "\n  (" + sp.toString + "):\n" + sp.prettified + "\n")
  }
  def sayTokens (tokens: Stream[Token]) {
    var t = tokens
    do {
      say(" " + t.head)
      t = t.tail
    } while (t.head != End)
  }
  def sayNorms (norm: Norm) {
    var n = norm
    do {
      say(" " + n)
      n = n.next
    } while (n.token != End)
  }
}

class EmptySpec extends PrettySpec {
  "An Empty" should {
    "have the right toString" in {
      Empty.toString should equal ("Empty")
    }
    "have the right debugString" in {
      Empty.debugString should equal ("Empty")
    }
    "be empty" in {
      show(Empty) should equal ("")
    }
    "be empty when narrow" in {
      show(Empty,1) should equal ("")
    }
    "have a useful debugString" in {
      sayPrettify(Empty)
    }
  }
}

class ToTokenSpec extends PrettySpec {
  "A ToToken" should {
    "have the right toString" in {
      ToToken(Text("hi")).toString should equal ("ToToken(Text(hi))")
    }
    "have the right debugString" in {
      ToToken(Text("hi")).debugString should equal ("ToToken(Text(hi))")
    }
    "show its content" in {
      show(ToToken(Text("yo"))) should equal ("yo")
    }
    "have a useful debugString" in {
      sayPrettify(ToToken(Text("bye")))
    }
  }
}

class ShowSpec extends PrettySpec {
  "A Show" should {
    "have the right toString" in {
      Show("hi").toString should equal ("Show(\"hi\")")
    }
    "have the right debugString" in {
      Show("hi").debugString should equal ("Show(\"hi\")")
    }
    "act like toString for non-Prettifies" in {
      show(Show("hi")) should equal ("hi")
      show(Show(3)) should equal ("3")
    }
    "expand" in {
      val str = Show("xyz").expand(Stream.empty)
      str.length should equal (1)
      str.head match {
        case ToToken(Text(t)) =>
          t should equal ("xyz")
        case x =>
          UFail("Was not ToToken of Text: " + x)
      }
    }
    "have a useful debugString" in {
      sayPrettify(Show("abc"))
      sayPrettify(Show(3))
      sayPrettify(Show("abc\ndef"))
      sayPrettify(Show("0123456789" * 8))
    }
    "be replaced by its argument when its argument is a Pretty" in {
      show(
        Show(
          new Pretty {
            def pretty: Prettify = Concat(Show("abc"),HardBreak,Show("def"))
          })) should equal ("abc\ndef")
    }
  }
  "A Show" when {
    "empty" should {
      "expand to Empty" in {
        val str = Show("").expand(Stream.empty)
        str.length should equal (0)
      }
    }
    "it contains no newlines" should {
      "not be split when formatted narrow" in {
        show(Show("hi"),1) should equal ("hi")
      }
    }
    "it contains newlines" should {
      "be split when formatted narrow" in {
        show(Show("hi\nbye"),1) should equal ("hi\nbye")
      }
      "be split when formatted wide" in {
        show(Show("hi\nbye")) should equal ("hi\nbye")
      }
      "retain all newlines" in {
        val s = "hi\n\nthere\n\nbye\n\n"
        show(Show(s)) should equal (s)
        val ss = "\n\nhi\n\nthere\n\nbye"
        show(Show(ss)) should equal (ss)
      }
    }
  }
}

class SoftBreakSpec extends PrettySpec {
  "A SoftBreak" should {
    "complain if its whenHorizontal contains a newline" in {
      say(
        (evaluating {
          SoftBreak("\n")
        } should produce [IllegalArgumentException]).
          getMessage)
    }
    "have the right toString" in {
      SoftBreak.toString should equal ("SoftBreak(\"\")")
      SoftBreak("z").toString should equal ("SoftBreak(\"z\")")
      LeftBreak("").toString should equal ("LeftBreak(\"\")")
      AlignBreak("").toString should equal ("AlignBreak(\"\")")
    }
    "have the right debugString" in {
      SoftBreak.debugString should equal ("SoftBreak(\"\")")
      SoftBreak("z").debugString should equal ("SoftBreak(\"z\")")
    }
    "be replaced by its whenHorizontal when horizontal" in {
      show(SoftBreak) should equal ("")
      show(SoftBreak("xx")) should equal ("xx")
    }
    "be a newline when vertical" in {
      show(SoftBreak("xx"),1) should equal ("")
    }
    "not be absorbed by adjacent breaks when horizontal" in {
      show(Concat(SoftBreak("x"),SoftBreak("y"))) should equal ("xy")
      show(Concat("a",SoftBreak("x"),SoftBreak("y"),"b")) should equal ("axyb")
    }
    "be absorbed by adjacent breaks when vertical" in {
      show(Concat(SoftBreak("x"),SoftBreak("y")),1) should equal ("")
      show(Concat("a",SoftBreak("x"),SoftBreak("y"),"b"),1) should equal (
        "a\nb")
      show(Concat(HardBreak,SoftBreak("y")),1) should equal ("\n")
      show(Concat("a",HardBreak,SoftBreak("y"),"b"),1) should equal ("a\nb")
      show(Concat(SoftBreak("x"),HardBreak),1) should equal ("\n")
      show(Concat("a",SoftBreak("x"),HardBreak,"b"),1) should equal ("a\nb")
    }
    "handle VerticalStrings" in {
      def horizEqVert (s: String) =
        new SoftBreak(s) {
          override def whenVertical (
              margin: Int, column: Int, limits: Limits): Option[String] =
            Some(s)
        }
      show(Concat("aa",horizEqVert("bb"),"cc")) should equal ("aabbcc")
      show(Concat("aa",horizEqVert("bb"),"cc"),3) should equal ("aabbcc")
      show(Concat("aa",horizEqVert("bb"),"cc"),2) should equal ("aabbcc")
      show(Concat("aa",horizEqVert("bb"),"cc"),1) should equal ("aabbcc")
    }
    "not LeftBreak when the break would be right of the current margin" in {
      show(Indent(4,"a",LeftBreak("x"),"b")) should equal ("axb")
      show(Indent(4,"a",LeftBreak("x"),"b"),2) should equal ("axb")
      show(Indent(4,"a",SoftBreak("x"),"b"),2) should equal ("a\n    b")
    }
    "LeftBreak when appropriate" in {
      show(
          Indent(
            4,"aaa",Indent(4,SoftBreak,"b"),SoftBreak,"ccc")) should equal (
        "aaabccc")
      show(
          Indent(
            4,"aaa",Indent(4,SoftBreak,"b"),SoftBreak,"ccc"),6) should equal (
        "aaab\n    ccc")
      show(
          Indent(
            4,"aaa",Indent(4,SoftBreak,"b"),SoftBreak,"ccc"),3) should equal (
        "aaa\n        b\n    ccc")
      show(
          Indent(
            4,"aaa",Indent(4,LeftBreak,"b"),LeftBreak,"ccc")) should equal (
        "aaabccc")
      show(
          Indent(
            4,"aaa",Indent(4,LeftBreak,"b"),LeftBreak,"ccc"),6) should equal (
        "aaabccc")
      show(
          Indent(
            4,"aaa",Indent(4,LeftBreak,"b"),LeftBreak,"ccc"),3) should equal (
        "aaabccc")
    }
    "AlignBreak when appropriate" in {
      show(
          Indent(
            4,"aaa",Indent(4,AlignBreak,"b"),AlignBreak,"ccc")) should equal (
        "aaabccc")
      show(
          Indent(
            4,"aaa",Indent(4,AlignBreak,"b"),AlignBreak,"ccc"),6) should equal (
        "aaabccc")
      show(
          Indent(
            4,"aaa",Indent(4,AlignBreak,"b"),AlignBreak,"ccc"),3) should equal (
        "aaa     b\n    ccc")
      show(
          Indent(
            4,"a",AlignBreak,"b",AlignBreak,"c",AlignBreak,"d")) should equal (
        "abcd")
      show(
          Indent(
            4,"a",AlignBreak,"b",AlignBreak,"c",AlignBreak,"d"),
          3) should equal (
        "a   b\n    c\n    d")
      show(
          Indent(
            4,"aaaaa",AlignBreak,"b",AlignBreak,"c",AlignBreak,
            "d")) should equal (
        "aaaaabcd")
      show(
          Indent(
            4,"aaaaa",AlignBreak,"b",AlignBreak,"c",AlignBreak,"d"),
          3) should equal (
        "aaaaa\n    b\n    c\n    d")
    }
    "have a useful debugString" in {
      sayPrettify(SoftBreak)
      sayPrettify(SoftBreak("x"))
    }
  }
}

class HardBreakSpec extends PrettySpec {
  "A HardBreak" should {
    "complain if its count is not positive" in {
      say(
        (evaluating {
          HardBreak(0)
        } should produce [IllegalArgumentException]).
          getMessage)
    }
    "have the right toString" in {
      HardBreak.toString should equal ("HardBreak(1)")
      HardBreak(2).toString should equal ("HardBreak(2)")
    }
    "have the right debugString" in {
      HardBreak.debugString should equal ("HardBreak(1)")
      HardBreak(2).debugString should equal ("HardBreak(2)")
    }
    "always be rendered vertically" in {
      show(Concat("a",HardBreak)) should equal ("a\n")
      show(Concat(HardBreak,"a")) should equal ("\na")
    }
    "not be absorbed by adjacent breaks" in {
      show(Concat(HardBreak,HardBreak)) should equal ("\n\n")
      show(HardBreak(3)) should equal ("\n\n\n")
      show(Concat(HardBreak(3),HardBreak)) should equal ("\n\n\n\n")
    }
    "absorb adjacent soft breaks" in {
      show(Concat(HardBreak,SoftBreak,HardBreak)) should equal ("\n\n")
      show(Concat(HardBreak,"a",SoftBreak,HardBreak)) should equal ("\na\n")
      show(Concat(HardBreak,SoftBreak,"a",HardBreak)) should equal ("\na\n")
      show(Concat(HardBreak,SoftBreak,SoftBreak,HardBreak)) should equal (
        "\n\n")
      show(Concat(SoftBreak,HardBreak(2),SoftBreak)) should equal ("\n\n")
      show(Concat(SoftBreak,HardBreak,"a")) should equal ("\na")
      show(Concat("a",HardBreak,SoftBreak,HardBreak,"b")) should equal (
        "a\n\nb")
      show(Concat("a",SoftBreak,HardBreak(2),SoftBreak,"b")) should equal (
        "a\n\nb")
    }
    "not absorb nonadjacent breaks" in {
      show(Concat(HardBreak,"a",SoftBreak,"b",HardBreak)) should equal (
        "\na\nb\n")
      show(Concat(SoftBreak,"a",HardBreak(2),"b",SoftBreak)) should equal (
        "\na\n\nb")
      show(Group(SoftBreak,"a",HardBreak(2),"b",SoftBreak)) should equal (
        "\na\n\nb")
    }
    "have a useful debugString" in {
      sayPrettify(HardBreak)
      sayPrettify(HardBreak(2))
    }
  }
}

class WeakBreakSpec extends PrettySpec {
  "A WeakBreak" should {
    "complain if its whenHorizontal contains a newline" in {
      say(
        (evaluating {
              WeakBreak("\n").debugString
            } should produce [IllegalArgumentException]).
          getMessage)
    }
    "have the right toString" in {
      WeakBreak.toString should equal ("WeakBreak")
      val s = WeakBreak("z").toString
      s should startWith ("Group@")
    }
    "have the right debugString" in {
      WeakBreak.debugString should equal ("WeakBreak")
      WeakBreak("z").debugString should equal ("Group(SoftBreak(\"z\"))")
    }
    "be replaced by its whenHorizontal when horizontal" in {
      show(WeakBreak) should equal ("")
      show(WeakBreak("xx")) should equal ("xx")
    }
    "be a newline when vertical" in {
      show(WeakBreak("xx"),1) should equal ("")
      show(Concat(WeakBreak("xx"),"a"),1) should equal ("\na")
    }
    "not be absorbed by adjacent breaks when horizontal" in {
      show(Concat(WeakBreak("x"),WeakBreak("y"))) should equal ("xy")
      show(Concat("a",WeakBreak("x"),WeakBreak("y"),"b")) should equal ("axyb")
    }
    "be absorbed by adjacent breaks when vertical" in {
      show(Concat(WeakBreak("x"),WeakBreak("y")),1) should equal ("x")
      show(Concat(WeakBreak("xx"),WeakBreak("yy")),1) should equal ("")
      show(Concat("a",WeakBreak("x"),WeakBreak("y"),"b"),1) should equal (
        "a\nb")
      show(Concat("a",WeakBreak("x"),WeakBreak("y"),"b"),2) should equal (
        "ax\nb")
      show(Concat("aa",WeakBreak("x"),WeakBreak("yyy"),"bb"),2) should equal (
        "aa\nbb")
      show(Concat(HardBreak,WeakBreak("y")),1) should equal ("\ny")
      show(Concat(HardBreak,WeakBreak("yy")),1) should equal ("\n")
      show(Concat("a",HardBreak,WeakBreak("y"),"b"),1) should equal ("a\nb")
      show(Concat("a",WeakBreak("y"),HardBreak,"b"),1) should equal ("a\nb")
      show(Concat(WeakBreak("x"),HardBreak),1) should equal ("x\n")
      show(Concat(WeakBreak("xx"),HardBreak),1) should equal ("\n")
      show(Concat("a",WeakBreak("x"),HardBreak,"b"),2) should equal ("ax\nb")
    }
    "not break when surrounding SoftBreaks break" in {
      show(Group("abc",SoftBreak,"d",WeakBreak,"ef"),3) should equal (
        "abc\ndef")
      show(Group("abc",SoftBreak,"d",WeakBreak,"ef"),2) should equal (
        "abc\nd\nef")
      show(Group("abc",SoftBreak,"d",WeakBreak("e"),"f"),3) should equal (
        "abc\ndef")
      show(Group("abc",SoftBreak,"d",WeakBreak("e"),"f"),2) should equal (
        "abc\nd\nf")
    }
    "have a useful debugString" in {
      sayPrettify(WeakBreak)
      sayPrettify(WeakBreak("x"))
    }
  }
}

class ConcatSpec extends PrettySpec {
  "A Concat" should {
    "have the right toString" in {
      Concat("a","b").toString should startWith ("Concat@")
      Concat("a",Concat("b","c")).toString should startWith ("Concat@")
      Concat(Concat("a","b"),"c").toString should startWith ("Concat@")
    }
    "have the right debugString" in {
      Concat("a","b").debugString should equal (
        "Concat(Show(\"a\"),Show(\"b\"))")
      Concat("a",Concat("b","c")).debugString should equal (
        "Concat(Show(\"a\"),Concat(Show(\"b\"),Show(\"c\")))")
      Concat(Concat("a","b"),"c").debugString should equal (
        "Concat(Concat(Show(\"a\"),Show(\"b\")),Show(\"c\"))")
    }
    "normalize" in {
      var tokens =
        Concat(Concat("a",Concat("b","c")),Concat(Concat("d","e"),"f")).tokens
      def next (expected: String) {
        tokens.head match {
          case Text(t) =>
            t should equal (expected)
          case x =>
            UFail("Not a Text: " + x)
        }
        tokens = tokens.tail
      }
      tokens.head should equal (Text(""))
      tokens = tokens.tail
      val o = tokens.head
      tokens.head should equal (Open())
      tokens = tokens.tail
      next("a")
      next("b")
      next("c")
      next("d")
      next("e")
      next("f")
      tokens.head match {
        case Close(wo) => wo.get should equal (o)
        case x => UFail("Not a Close: " + x)
      }
      tokens = tokens.tail
      tokens.head should equal (End)
    }
    "not break vertically when it contains no breaks" in {
      show(Concat("abc","def"),7) should equal ("abcdef")
      show(Concat("abc","def"),5) should equal ("abcdef")
    }
    "break vertically at every soft break when it breaks" in {
      show(Concat(SoftBreak,"abc",SoftBreak,"def",SoftBreak)) should equal (
        "abcdef")
      (show(Concat(SoftBreak,"abc",SoftBreak,"def",SoftBreak),5)
        should equal ("\nabc\ndef"))
      (show(Concat(SoftBreak,"abc",SoftBreak,"def",SoftBreak),2)
        should equal ("\nabc\ndef"))
    }
    "indent soft breaks before weak breaks" in {
      val n1 = Concat("abc",SoftBreak,"def",WeakBreak,"ghi")
      show(n1,7) should equal ("abc\ndefghi")
      show(n1,6) should equal ("abc\ndefghi")
      show(n1,5) should equal ("abc\ndef\nghi")
      val n2 = Concat("abc",WeakBreak,"def",SoftBreak,"ghi")
      show(n2,7) should equal ("abcdef\nghi")
      show(n2,6) should equal ("abcdef\nghi")
      show(n2,5) should equal ("abc\ndef\nghi")
    }
    "not freak out when infinite" in {
      def prettyN (n: Int): Prettify =
        Concat(
          "<" + n,WeakBreak(" "),prettyN(n + 1)," +",SoftBreak(" "),
            prettyN(n + 1),WeakBreak(" "),">")
      val n1 = prettyN(1)
      val lim = Limits(maxLength = 256)
      show(n1,lim) should startWith ("<1")
      say(n1.render(lim).toString)
    }
    "have a useful debugString" in {
      sayPrettify(Concat("a","b"))
      sayPrettify(Concat(Concat("a","b"),"c"))
      sayPrettify(Concat("a",Concat("b","c")))
    }
  }
}

class GroupSpec extends PrettySpec {
  "A Group" should {
    "have the right toString" in {
      Group("a").toString should startWith ("Group@")
    }
    "have the right debugString" in {
      Group("a").debugString should equal ("Group(Show(\"a\"))")
    }
    "contain no implicit break" in {
      val n1 = Concat("abc",Group("def"))
      show(n1) should equal ("abcdef")
      show(n1,5) should equal ("abcdef")
      show(n1,4) should equal ("abcdef")
    }
    "be broken together" in {
      val n1 = Concat(Group("a",SoftBreak,"b"),Group("c",SoftBreak,"d"))
      show(n1) should equal ("abcd")
      show(n1,3) should equal ("abc\nd")
      show(n1,2) should equal ("a\nbc\nd")
      val n2 = Concat(Group("aaa",SoftBreak,"b"),Group("c",SoftBreak,"d"))
      show(n2) should equal ("aaabcd")
      show(n2,6) should equal ("aaabcd")
      show(n2,5) should equal ("aaabc\nd")
      show(n2,4) should equal ("aaa\nbcd")
      show(n2,3) should equal ("aaa\nbcd")
      show(n2,2) should equal ("aaa\nbc\nd")
      show(n2,1) should equal ("aaa\nbc\nd")
    }
  }
}

class IndentSpec extends PrettySpec {
  "An Indent" should {
    "have the right toString" in {
      Indent("a").toString should startWith ("Group@")
    }
    "have the right debugString" in {
      Indent("a").debugString should equal ("Indent(Show(\"a\"))")
    }
    "contain no implicit break" in {
      val n1 = Concat("abc",Indent("def"))
      show(n1) should equal ("abcdef")
      show(n1,5) should equal ("abcdef")
      show(n1,4) should equal ("abcdef")
    }
    "indent soft breaks" in {
      val n2 = Concat("abc",Indent("def",SoftBreak,"ghi"))
      show(n2) should equal ("abcdefghi")
      show(n2,8) should equal ("abcdef\n  ghi")
      show(n2,7) should equal ("abcdef\n  ghi")
      show(n2,6) should equal ("abcdef\n  ghi")
    }
    "indent soft breaks before weak breaks" in {
      val n1 = Indent(0,"abc",SoftBreak,"def",WeakBreak,"ghi")
      show(n1,7) should equal ("abc\ndefghi")
      show(n1,6) should equal ("abc\ndefghi")
      show(n1,5) should equal ("abc\ndef\nghi")
      val n2 = Indent(0,"abc",WeakBreak,"def",SoftBreak,"ghi")
      show(n2,7) should equal ("abcdef\nghi")
      show(n2,6) should equal ("abcdef\nghi")
      show(n2,5) should equal ("abc\ndef\nghi")
      val n3 = Group(Indent,"abc",SoftBreak,"def",WeakBreak,"ghi")
      show(n3,8) should equal ("abc\n  defghi")
      show(n3,7) should equal ("abc\n  def\n  ghi")
      val n4 = Group(Indent,"abc",WeakBreak,"def",SoftBreak,"ghi")
      show(n4,8) should equal ("abcdef\n  ghi")
      show(n4,7) should equal ("abcdef\n  ghi")
      show(n4,6) should equal ("abcdef\n  ghi")
      show(n4,5) should equal ("abc\n  def\n  ghi")
    }
    "indent all breaks or none" in {
      val n1 =
        Indent(
          0,"a",SoftBreak("x"),Indent(0,SoftBreak("y"),"b",SoftBreak("z")),"c",
          SoftBreak("xx"),"d")
      show(n1) should equal ("axybzcxxd")
      show(n1,8) should equal ("a\nybzc\nd")
      show(n1,7) should equal ("a\nybzc\nd")
      show(n1,3) should equal ("a\nb\nc\nd")
      show(n1,2) should equal ("a\nb\nc\nd")
      val n2 =
        Indent(
          0,"a",SoftBreak("x"),Indent(SoftBreak("y"),"b",SoftBreak("z")),"c",
          SoftBreak("xx"),"d")
      show(n2) should equal ("axybzcxxd")
      show(n2,8) should equal ("a\nybzc\nd")
      show(n2,7) should equal ("a\nybzc\nd")
      show(n2,3) should equal ("a\n  b\n  c\nd")
      show(n2,2) should equal ("a\n  b\n  c\nd")
      val n3 =
        Indent(
          "a",SoftBreak("x"),Indent(SoftBreak("y"),"b",SoftBreak("z")),"c",
          SoftBreak("xx"),"d")
      show(n3) should equal ("axybzcxxd")
      show(n3,8) should equal ("a\n  ybzc\n  d")
      show(n3,7) should equal ("a\n  ybzc\n  d")
      show(n3,4) should equal ("a\n    b\n    c\n  d")
      show(n3,3) should equal ("a\n    b\n    c\n  d")
    }
    "not freak out when infinite" in {
      def prettyN (n: Int): Prettify =
        Indent("<" + n,SoftBreak(" "),prettyN(n + 1),SoftBreak(" "),">")
      val n1 = prettyN(1)
      val lim = Limits(maxLength = 256)
      show(n1,lim) should startWith ("<1")
      say(n1.render(lim).toString)
    }
    "have a useful debugString" in {
      sayPrettify(Concat("abc",Indent("def")))
      sayPrettify(Indent("def",SoftBreak,"ghi"))
      sayPrettify(Concat("abc",Indent("def",SoftBreak,"ghi")))
      sayPrettify(
        Indent(
          "abcdefghijklmnopqrstuvwxyz",SoftBreak,"zyxwvutsrqponmlkjihgfedcba"))
      sayPrettify(
        Indent(
          "alphaBetaGammaDeltaEpsilonZeta",SoftBreak,
            "etaThetaIotaKappaLambdaMu",SoftBreak,"nuXiOmicronPiRhoSigma",
            SoftBreak,"tauUpsilonPhiChiPsiOmega"))
    }
  }
}

class AlignSpec extends PrettySpec {
  "An Align" should {
    "align" in {
      show(
        Group(
          "int ",
          Align(
            "i",SoftBreak(", "),
            "j",SoftBreak(", "),
            "k"))) should equal ("int i, j, k")
      show(
        Group(
          "int ",
          Align(
            "i",SoftBreak(", "),
            "j",SoftBreak(", "),
            "k")),
        10) should equal ("int i\n    j\n    k")
      show(
        Group(
          "double ",
          Align(
            "i",SoftBreak(", "),
            "j",SoftBreak(", "),
            "k"))) should equal ("double i, j, k")
      show(
        Group(
          "double ",
          Align(
            "i",SoftBreak(", "),
            "j",SoftBreak(", "),
            "k")),
        10) should equal ("double i\n       j\n       k")
    }
  }
}

} // end package test
