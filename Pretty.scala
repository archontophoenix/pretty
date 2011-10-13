package archontophoenix

/**
 * A prettyprinter: if you have a hierarchical structure that can be represented
 * textually, this package lets you lay it out properly indented, while fitting
 * the text (if possible) within a specified width.
 *
 * This package is more or less in the style of prettyprinters common in other
 * functional languages, but more full-featured than most. In contrast with the
 * `scala.text` package that ships with Scala 2.9, this package does automatic
 * normalization (moves text across group boundaries to prevent lines
 * unnecessarily running past the right margin), among other additional
 * features.
 *
 * This package prettyprints lazily. In conjunction with the depth and length
 * limits in [[archontophoenix.pretty.Limits]], this lets you prettyprint a
 * synopsis of large (even infinite) data structures without consuming too much
 * time or space.
 *
 * === Overview ===
 *
 * Make a class prettyprintable by extending the trait
 * [[archontophoenix.pretty.Pretty]] and implementing its `pretty` method. That
 * method returns a [[archontophoenix.pretty.Prettify]], which is the
 * prettyprinter's representation of the components to lay out.
 *
 * For example:
 *
 * {{{
 *  import archontophoenix.pretty._
 *
 *  case class MyPrettyInteger (i: Int) extends Pretty {
 *    def pretty: Prettify = Show(i) // shows i.toString
 *  }
 * }}}
 *
 * `Pretty` overrides `toString` to return the result of rendering the result
 * of `pretty` with the default limits; if you don't want to override `toString`
 * this way, your class should extend [[archontophoenix.pretty.PrettyLike]]
 * instead.
 *
 * To display a `Prettify` directly (instead of calling `toString` of the
 * `Pretty` that created it), call `render` on it. For example:
 *
 * {{{
 *  MyPrettyInteger(3).pretty.render(System.out)
 * }}}
 *
 * The style of this package is to use `apply` methods in top-level objects as
 * factories for the `Prettify`s that you return from `pretty`. The following
 * objects are the `Prettify` factories you'll use most frequently:
 * <ul>
 * <li> '''`Show`''': the text of the `toString` of its
 *   argument, or, if the argument is itself a `Pretty`, its `Prettify`.
 * <li> '''`SoftBreak`''': a place for a possible line break.
 *   Does not actually translate to a newline unless it's in a `Group` that
 *   would exceed the maximum allowable width if rendered unbroken. If not
 *   broken, a `SoftBreak` is the empty string by default, but you can change
 *   that. If a `SoftBreak` is adjacent to a broken break, it isn't broken (so
 *   that several `SoftBreak`s in a row never result in more than one newline).
 * <li> '''`LeftBreak`''' and
 *   '''`AlignBreak`''': two variations on `SoftBreak`;
 *   see the documentation for each of these, and the examples below, for
 *   details.
 * <li> '''`WeakBreak`''': a `SoftBreak` in its own `Group`, so that it can be
 *   broken without forcing other `SoftBreak`s in the same `Group` to break.
 * <li> '''`HardBreak`''': a line break; always rendered as a
 *   newline, no matter how far from the margin the current `Group` extends.
 * <li> '''`Group`''': a sequence of `Prettify`s in which 
 *   immediately contained `SoftBreak`s are either all rendered as newlines, or
 *   all rendered unbroken. `Group`s nested within a broken group may still be
 *   unbroken, but all `Group`s containing a broken group are also broken.
 * <li> '''`Indent`''': creates a `Group` with an indentation,
 *   so that newlines (if more text occurs after them) are followed by some
 *   spaces (the default indentation is 2, as specified in
 *   '''`DefaultLimits`''', but you can override that). The
 *   more deeply you nest `Indent`s within one another, the wider this left
 *   margin gets.
 * <li> '''`Align`''': creates a `Group` with an indentation,
 *   like `Indent`, except that instead of adding a fixed amount to the current
 *   left margin, sets the left margin to the current column; see the examples
 *   below.
 * <li> '''`Prettify`''': factory for `Traversable`s that lets you specify
 *   how to display each element and how to separate the elements.
 * </ul>
 *
 * Some more factories you might use sometimes are:
 * <ul>
 * <li> '''`Concat`''': a concatenated sequence of `Prettify`s.
 *   You can often get away without specifying a `Concat` explicitly, because
 *   most of the functions in this package that return a `Prettify` let you
 *   specify a sequence of `Prettify`s, which are automatically concatenated.
 *   However, because Scala 2.9 does not have repeated by-name parameters, you
 *   have to use an explicit `Concat` if the number of items you want to
 *   concatenate exceeds a certain number (currently eight).
 * <li> '''`Indented`''': adjusts the current margin ''without''
 *   creating a `Group`. Less useful than `Indent`, but see the example below.
 * <li> '''`Empty`''': an empty `Prettify`, rendered as the
 *   empty string.
 * </ul>
 *
 * There are two implicit conversions to `Prettify` to make life simpler:
 * <ul>
 * <li> From a `String` ''s'' to `Show(`''s''`)`.
 * <li> From an `Option` ''o'' to `Empty` when ''o'' is `None`,
 *   to `Show(`''o''`.get)` otherwise.
 * </ul>
 *
 * === Example: an Indented Paragraph ===
 *
 * Suppose you want to start the display of a paragraph with a newline (always),
 * followed by an indented first line. The words in the paragraph are separated
 * by spaces when they appear on the same line, but the paragraph is broken
 * whenever the next word would run past the right margin.
 *
 * You'll want to use the `Prettify` factory because the words in the paragraph
 * are a `Traversable`. And because you want to break between words only when
 * necessary (not between every pair of words), you'll want to separate words
 * with a `WeakBreak`:
 *
 * {{{
 *  case class Paragraph (unformatted: String) extends Pretty {
 *    val words = unformatted.split("\\s").filterNot(_.isEmpty)
 *    def pretty: Prettify =
 *      Group(Indent(HardBreak),Prettify(words,WeakBreak(" ")))
 *    def main (args: Array[String]) {
 *      pretty.render(if (args.isEmpty) 80 else args(0).toInt,System.out)
 *    }
 *  }
 *
 *  object Story extends Paragraph("""
 *      Once upon a time,
 *        in an enchanted cavern deep beneath
 *        the mountains,
 *        there lived a brilliant type theorist who loved
 *        to invent                 programming languages.""")
 * }}}
 *
 * The result of invoking `Story 40` looks like:
 *
 * {{{
 * 
 *   Once upon a time, in an enchanted
 * cavern deep beneath the mountains, there
 * lived a brilliant type theorist who
 * loved to invent programming languages.
 * }}}
 *
 * If you replaced the `WeakBreak(" ")` with a `HardBreak`, a newline would
 * separate every pair of words in the output. If you used `SoftBreak(" ")`,
 * that would still be true unless the whole paragraph managed to fit on a
 * single line, in which case it would be displayed unbroken.
 *
 * === Example: Unary and Binary Operators ===
 *
 * As a more complicated example, consider an expression language consisting of
 * identifiers, unary operators, and binary operators. The supported
 * binary operators have several different precedences. Expressions are
 * evaluated left to right, like Scala expressions.
 *
 * You want the display of expressions in this language to obey several rules:
 * <ul>
 * <li> If an expression fits entirely on a line, don't break it.
 * <li> A newline, if necessary, occurs after an operator.
 * <li> A subexpression (or rather, the part of it that occurs after a
 *   newline) is indented farther than the expression that contains it.
 * <li> Parentheses should be omitted wherever the operator precedence and
 *   evaluation order rules make them unnecessary.
 * </ul>
 *
 * You can express these rules as:
 *
 * {{{
 *  trait Expr extends Pretty
 *  
 *  case class Id (id: String) extends Expr {
 *    def pretty: Prettify = Show(id)
 *  }
 *  
 *  case class UnOp (opName: String) extends Pretty {
 *    def pretty: Prettify = Show(opName)
 *  }
 *  
 *  case class Unary (op: UnOp, arg: Expr) extends Expr {
 *    def pretty: Prettify =
 *      arg match {
 *        case b: Binary => Indent(Show(op)," (",LeftBreak,Show(b),")")
 *        case u: Unary => Concat(Show(op)," ",Show(u))
 *        case _ => Indent(Show(op),LeftBreak(" "),Show(arg))
 *      }
 *  }
 *  
 *  case class BinOp (opName: String) extends Pretty {
 *    def pretty: Prettify = Show(opName)
 *    def precedence =
 *      opName match {
 *        case "||" => 1
 *        case "&&" => 2
 *        case "<" | ">" | "<=" | ">=" => 3
 *        case "+" | "-" => 4
 *        case "*" | "/" | "%" => 5
 *      }
 *  }
 *  
 *  case class Binary (left: Expr, op: BinOp, right: Expr) extends Expr {
 *    def prettyOnLeft: Prettify = {
 *      def maybeParen (x: Expr) =
 *        x match {
 *          case b @ Binary(_,oo,_) if (op.precedence >= oo.precedence) =>
 *            Concat("(",Show(b),")")
 *          case _ =>
 *            Show(x)
 *        }
 *      left match {
 *        case b @ Binary(_,oo,_) if op.precedence == oo.precedence =>
 *          Concat(b.prettyOnLeft," ",Show(op),WeakBreak(" "),maybeParen(right))
 *        case _ =>
 *          Concat(
 *            Group(maybeParen(left)," ",Show(op),LeftBreak(" ")),
 *            Group(LeftBreak,maybeParen(right)))
 *      }
 *    }
 *    def pretty: Prettify = Indent(prettyOnLeft)
 *  }
 *  
 *  object Exprs {
 *    def exprs =
 *      List(
 *        Unary(UnOp("-"),Id("alpha")),
 *        Binary(Id("alpha"),BinOp("||"),Id("beta")),
 *        Binary(
 *          Binary(Id("alpha"),BinOp("*"),Id("beta")),
 *          BinOp("+"),Id("gamma")),
 *        Binary(
 *          Id("alpha"),BinOp("*"),
 *          Binary(Id("beta"),BinOp("+"),Id("gamma"))),
 *        Unary(
 *          UnOp("!"),
 *          Binary(
 *            Id("alpha"),BinOp("*"),
 *            Binary(Id("beta"),BinOp("+"),Id("gamma")))),
 *        Binary(
 *          Binary(
 *            Binary(
 *              Binary(Id("alpha"),BinOp("+"),Id("beta")),
 *              BinOp("+"),Id("gamma")),
 *            BinOp("+"),Id("delta")),
 *          BinOp("+"),Id("epsilon")),
 *        Binary(
 *          Binary(
 *            Binary(
 *              Binary(Id("a"),BinOp("/"),Id("b")),
 *              BinOp("+"),Id("c")),
 *            BinOp("<"),Id("d")),
 *          BinOp("&&"),Id("e")))
 *    def main (args: Array[String]) {
 *      for (x <- exprs) {
 *        println
 *        x.pretty.render(if (args.isEmpty) 80 else args(0).toInt,System.out)
 *        println
 *      }
 *    }
 *  }
 * }}}
 *
 * The logic for printing binary expressions is a bit subtle. If the left
 * operand is itself a binary operation of the same precedence, then the nested
 * expression is part of the same group, broken only at the `WeakBreak`.
 * Otherwise, the left operand and the operator are grouped together, and the
 * right operand is placed in a separate group preceded by its own break. This
 * means that the left and right sides can be broken separately, but if the
 * right side is broken, it is always preceded by a newline. Although the left
 * side ends with a break point and the right side begins with one, this never
 * results in two consecutive newlines in the output, because soft breaks
 * adjacent to a taken break are effectively absorbed.
 *
 * Running `Exprs` with no argument prints each expression on a single line:
 *
 * {{{
 *  - alpha
 *  
 *  alpha || beta
 *  
 *  alpha * beta + gamma
 *  
 *  alpha * (beta + gamma)
 *  
 *  ! (alpha * (beta + gamma))
 *  
 *  alpha + beta + gamma + delta + epsilon
 *  
 *  a / b + c < d && e
 * }}}
 *
 * Limiting the output to 20 columns shows how nested `Indent`s in the
 * `Prettify` constructed for each node result in indentation that follows the
 * rules above:
 *
 * {{{
 *  - alpha
 *  
 *  alpha || beta
 *  
 *  alpha * beta + gamma
 *  
 *  alpha *
 *    (beta + gamma)
 *  
 *  ! (
 *    alpha *
 *      (beta + gamma))
 *  
 *  alpha + beta +
 *    gamma + delta +
 *    epsilon
 *  
 *  a / b + c < d && e
 * }}}
 *
 * By tracing through the calls to `pretty` (or calling `Prettify`'s
 * `prettified` method, which you might find useful in debugging), you can see
 * that the fourth expression above is the rendering of:
 *
 * {{{
 *  Indent(
 *    Group("alpha"," ","*",LeftBreak(" ")),
 *    Group(
 *      LeftBreak,"(",
 *      Indent(
 *        Group("beta"," ","+",LeftBreak(" ")),
 *        Group(LeftBreak,"gamma")),
 *      ")"))
 * }}}
 *
 * where redundant calls to `Show` and `Concat` have been removed. The broken
 * `LeftBreak` is nested only within the outermost indent, so the resulting
 * newline is followed by a left margin of 2 spaces (the default per level of
 * indentation). As shown below, if the inner `Indent` were also broken, the
 * resulting margin would be 4 spaces.
 *
 * You can specify a width too small for the prettyprinter to fit the output. In
 * this case, it does the best job it can. For example, `Expr 5` displays:
 * {{{
 *   - alpha
 *  
 *  alpha ||
 *    beta
 *  
 *  alpha *
 *      beta +
 *    gamma
 *  
 *  alpha *
 *    (beta +
 *      gamma)
 *  
 *  ! (
 *    alpha *
 *      (beta +
 *        gamma))
 *  
 *  alpha +
 *    beta +
 *    gamma +
 *    delta +
 *    epsilon
 *  
 *  a / b +
 *        c <
 *      d &&
 *    e
 * }}}
 *
 * You might notice something curious about the last expression: even though the
 * first line (`a / b +`) is too long to fit within 5 columns, and contains a
 * legal break point, it isn't broken. This illustrates the difference between
 * `LeftBreak`, which the expression printer uses, and `SoftBreak`, which we saw
 * before: `SoftBreak` always breaks lines that don't fit, but `LeftBreak`
 * breaks only if the margin after the newline would be to the ''left'' of the
 * current column. If we had used `SoftBreak` instead, the last expression would
 * have come out as:
 *
 * {{{
 *  a /
 *          b +
 *        c <
 *      d &&
 *    e
 * }}}
 *
 * which puts `b +` to the ''right'' of the end of the preceding line. You might
 * prefer `LeftBreak` to `SoftBreak` in formatting program source code because
 * of cases like these, where `SoftBreak`'s more literal-minded interpretation
 * of the prettyprinting rules actually makes matters worse, at least in terms
 * of fitting within the specified width.
 * 
 * === Example: Do Loops ===
 *
 * Imagine that you want a `do` loop to look like this if it fits on a line:
 *
 * {{{
 *  do statement while (cond)
 * }}}
 *
 * But if it doesn't fit, it should be broken like this:
 *
 * {{{
 *  do
 *    statement
 *  while (cond)
 * }}}
 *
 * that is, so that the `while` lines up with the `do`.
 *
 * The output should be broken after the `do` if and only if it is also broken
 * before the `while`, so logically, both breaks are in the same `Group`.
 * However, each is indented at a different level. This is a case where you'll
 * want to use an `Indented`, rather than an `Indent`, because the latter
 * creates a group of its own, which would decouple the two breaks.
 *
 * A way to do this (using the `Expr` of the previous example) is:
 *
 * {{{
 *  case class Do (stmt: Expr, cond: Expr) extends Expr {
 *    def pretty: Prettify =
 *      Group(
 *        "do",Indented(LeftBreak(" ")),Show(stmt),LeftBreak(" "),
 *        "while (",Indent(Show(cond)),")")
 *  }
 * }}}
 *
 * which formats `Do(Id("statement"),Id("cond"))` as shown above.
 *
 * === Example: Aligned Declarations ===
 *
 * In many programming languages, it's common to use an indentation style that
 * aligns items in a declaration or multiway conditional, but at an indentation
 * depth that depends on how the items to be aligned are introduced. For
 * example, you might align a Java declaration of `int`s (when the whole thing
 * doesn't fit on one line) at column 4:
 *
 * {{{
 *  int alpha,
 *      beta,
 *      gamma;
 * }}}
 *
 * but of `String`s at column 7:
 *
 * {{{
 *  String sigma,
 *         tau,
 *         upsilon;
 * }}}
 *
 * because `int` plus a following space is 4 characters, but `String` plus a
 * following space is 7 characters.
 *
 * The `Align` factory in this package sets the left margin to the current
 * position, so the following program displays declarations formatted as above:
 *
 * {{{
 *  object Align1 {
 *    case class Decls (typ: String, ids: String*) extends Pretty {
 *      def pretty =
 *        Group(typ," ",Align(Prettify(ids,Concat(",",SoftBreak(" ")))),";")
 *    }
 *    val decls =
 *      List(
 *        Decls("int","alpha","beta","gamma"),
 *        Decls("string","sigma","tau","upsilon"))
 *    def main (args: Array[String]) {
 *      for (d <- decls) {
 *        println
 *        d.pretty.render(if (args.isEmpty) 80 else args(0).toInt,System.out)
 *        println
 *      }
 *    }
 *  }
 * }}}
 *
 * The appearance of the `Align` following the space after `typ` means that
 * the left margin within the `Align` is at the column just after the space.
 *
 * Another approach to aligning items uses standard margins (independent of
 * what precedes the items), but pads the first item with spaces to align it
 * with the following items if the first item would fit on the same line as
 * the preceding text. For example, with a standard indent of 8, you'd write:
 *
 * {{{
 *  int     alpha,
 *          beta,
 *          gamma;
 * }}}
 *
 * but:
 *
 * {{{
 *  BigInteger
 *          iota,
 *          kappa,
 *          lambda;
 * }}}
 *
 * The latter breaks after `BigInteger` because `BigInteger` and a following
 * space won't fit in 8 characters.
 *
 * Use the `AlignBreak` variant of `SoftBreak` to perform this type of
 * alignment:
 * 
 * {{{
 *  object Align2 {
 *    case class Decls (typ: String, ids: String*) extends Pretty {
 *      def pretty =
 *        Indent(
 *          8,typ,AlignBreak(" "),Prettify(ids,Concat(",",SoftBreak(" "))),";")
 *    }
 *    val decls =
 *      List(
 *        Decls("int","alpha","beta","gamma"),
 *        Decls("BigInteger","iota","kappa","lambda"))
 *    def main (args: Array[String]) {
 *      for (d <- decls) {
 *        println
 *        d.pretty.render(if (args.isEmpty) 80 else args(0).toInt,System.out)
 *        println
 *      }
 *    }
 *  }
 * }}}
 *
 * === Implementation Notes: Performance ===
 *
 * Rendering a `Prettify` takes time approximately proportional to the size of
 * the `Prettify` (if not cut off by [[archontophoenix.pretty.Limits]]), which
 * is the best you can do. To achieve this, a normalized token (`Norm`) caches
 * the width the current `Prettify` would have if laid out all on the same line
 * at the farthest position that the renderer has examined so far (in a
 * `Positioner`). All `Norm`s created in a single call to `render` share the
 * same `Positioner`.
 *
 * Without caching this lookahead, the algorithm would be ''O''(''n''^2^) in the
 * length of the `Prettify`. Achieving linear time can be a bit tricky without
 * using mutable variables to do the caching; see, for example, Olaf Chitil's
 * papers about using [[http://www.cs.kent.ac.uk/pubs/2005/2062/index.html Lazy
 * Dequeues]] and [[http://www.cs.kent.ac.uk/pubs/2006/2381/index.html Delimited
 * Continuations]] to do the trick.
 *
 * === Implementation Notes: Style ===
 *
 * The implementation of this package uses a mixture of styles: some code looks
 * strictly functional, whereas other places make liberal use of mutable state.
 * It's not apparent from first glance, but none of the mutable state can escape
 * from a given invocation to `Prettify.render`, so all such state is
 * effectively local.
 *
 * There are too many pieces of state information at the top level of `render`
 * to make the usual trick of wrapping the state into a tuple visually
 * appealing. In Scala, mutable local variables are a more concise alternative
 * to an explicit state object. For example, feel free to think of something
 * like:
 *
 * {{{
 *  margin = newMargin
 *  doSomething
 *  margin = yetAnotherMargin
 *  doSomethingElse
 * }}}
 *
 * as shorthand for:
 *
 * {{{
 *  val newState = previousState.copy(margin = newMargin)
 *  val newerState = doSomething(newState)
 *  val evenNewerState = newerState.copy(margin = yetAnotherMargin)
 *  val yetNewerState = doSomethingElse(evenNewerState)
 * }}}
 */
package object pretty

package pretty {

import java.lang.Appendable
import scala.annotation.tailrec

/**
 * An object that provides a `Prettify` by implementing the `pretty` method.
 *
 * A [[archontophoenix.pretty.Show]] treats a `PrettyLike` by replacing itself
 * with the `PrettyLike`'s `pretty`.
 *
 * Inherit from [[archontophoenix.pretty.Pretty]] instead if you want to
 * automatically override `toString` with a prettyprinted version.
 */
trait PrettyLike {
  /** A specification for how this object should look when prettyprinted. */
  def pretty: Prettify
}

/**
 * A [[archontophoenix.pretty.PrettyLike]] that overrides `toString` with a
 * prettyprinted rendering of the object using the default
 * [[archontophoenix.pretty.Limits]].
 *
 * Inherit from [[archontophoenix.pretty.PrettyLike]] instead if you want to
 * specify a `pretty` without overriding `toString`.
 */
trait Pretty extends PrettyLike {
  /** Overridden to render the result of `pretty` with the default limits. */
  override def toString: String = pretty.render.toString
}

/**
 * A specification for prettyprinted text.
 *
 * Typically, you return one of these from [[archontophoenix.pretty.Pretty]]'s
 * `pretty` method, if you make each prettyprintable class inherit from
 * `Pretty`. You can also compute a prettify directly and display it with one
 * of the forms of `render` in this trait.
 */
sealed trait Prettify {
  /**
   * The structure of this `Prettify` (but not prettified). 
   *
   * Use `prettified` instead, unless debugging this package.
   */
  private[pretty] def debugString: String = toString
  /**
   * Renders this specification with the given maximum width to the given
   * output.
   */
  def render [A <: Appendable] (maximumWidth: Int, out: A): A =
    render(Limits(maxWidth = maximumWidth),out)
  /**
   * Renders this specification with the given maximum width to a `String`.
   */
  def render (maximumWidth: Int): String =
    render(Limits(maxWidth = maximumWidth)).toString
  /**
   * Renders this specification with the default
   * [[archontophoenix.pretty.Limits]] to the specified output.
   */
  def render [A <: Appendable] (out: A): A = render(DefaultLimits,out)
  /**
   * Renders this specification with the default
   * [[archontophoenix.pretty.Limits]] to a `String`.
   */
  def render: String = render(DefaultLimits).toString
  /** Turns this specification into a stream of tokens. */
  private[pretty] def tokens (maxDepth: Int): Stream[Token] = {
    def unzip1 [A,B,C] (str: Stream[(A,B,C)]): Stream[A] =
      Stream.cons(str.head._1,unzip1(str.tail))
    val start: (Token,Int,Stream[Prettify]) =
      (Text(""),maxDepth,Stream(Group(this)))
    unzip1(Stream.iterate(start)(Token.next _))
  }
  /**
   * Turns this specification into a stream of tokens, with no depth limit; for
   * debugging this package.
   */
  private[pretty] def tokens: Stream[Token] = tokens(Int.MaxValue)
  def pretty: Prettify = Show(toString)
  /** The structure of this `Prettify`, prettily; useful in debugging. */
  def prettified = pretty.render.toString
  /**
   * Renders this specification with the given
   * [[archontophoenix.pretty.Limits]] to the specified output.
   */
  def render [A <: Appendable] (
      limits: Limits, out: A = new java.lang.StringBuilder): A = {
    var input: Norm = Token.normalize(tokens(limits.maxDepth))
    var lastBreak: Option[(Break,Int)] = None
    var (remWidth,remHeight,remLength) =
      (limits.maxWidth,limits.maxHeight,limits.maxLength)
    val maxWidth = remWidth
    var margins: List[Int] = List(0)
    class DoesNotFitException extends RuntimeException
    def margin = margins.head
    def column = maxWidth - remWidth
    def netRemWidth =
      remWidth -
        (lastBreak match {
            case Some((_,n)) => n
            case None => 0
          })
    def pushMargin (p: Push) {
      margins = p.indenter.newMargin(margin,column,limits) :: margins
    }
    def popMargin {
      margins = margins.tail
    }
    def putHorizontal {
      def textOut (s: String) {
        def checkWidthAndIndent (m: Int) {
          val w = m + s.length
          remWidth -= w
          remLength -= w
          if (remLength < 0)
            throw new DoesNotFitException
          Indent.indent(m,out)
          lastBreak = None
        }
        lastBreak match {
          case Some((Soft(_),m)) =>
            break(1)
            checkWidthAndIndent(m)
          case Some((Hard(_),m)) =>
            checkWidthAndIndent(m)
          case None =>
            checkWidthAndIndent(0)
        }
        out.append(s)
      }
      input.token match {
        case Text(s) =>
          textOut(s)
        case Soft(b) =>
          textOut(b.whenHorizontal)
        case Hard(_) =>
          throw new AssertionError("Can't putHorizontal(Hard)")
        case Open() =>
          var depth = 1
          do {
            input = input.next
            input.token match {
              case Open() =>
                depth += 1
              case Close(_) =>
                depth -= 1
              case _ =>
                putHorizontal
            }
          } while (depth > 0)
        case p: Push =>
          pushMargin(p)
        case Pop =>
          popMargin
        case _ =>
      }
    }
    def break (numLines: Int) {
      remHeight -= numLines
      remLength -= numLines
      if (remHeight < 0 || remLength < 0)
        throw new DoesNotFitException
      out.append("\n" * numLines)
      remWidth = maxWidth
    }
    def putVertical {
      var depth = 0
      do {
        input.token match {
          case t: Text =>
            putHorizontal
          case s @ Soft(b) =>
            b.whenVertical(margin,column,limits) match {
              case Some(ss) =>
                if (ss.contains("\n"))
                  UFail(
                    "whenVertical for SoftBreak cannot contain newline: " + ss)
                val oldInput = input
                input =
                  new Norm(new Positioner,Text(ss),throw new AssertionError)
                putHorizontal
                input = oldInput
              case None =>
                lastBreak =
                  lastBreak match {
                    case Some((h: Hard,_)) => Some((h,margin))
                    case _ => Some((s,margin))
                  }
            }
          case h: Hard =>
            break(h.count)
            lastBreak = Some(h,margin)
          case Open() =>
            if (input.fits(netRemWidth))
              putHorizontal
            else
              depth += 1
          case Close(_) =>
            depth -= 1
          case p: Push =>
            pushMargin(p)
          case Pop =>
            popMargin
        }
      } while (depth > 0 &&
        {
          input = input.next
          true
        })
    }
    try
      while (input.token != End) {
        if (input.fits(netRemWidth))
          putHorizontal
        else
          putVertical
        input = input.next
      }
    catch {
      case x: DoesNotFitException =>
        out.append("\n...")
      case xx =>
        throw xx
    }
    out
  }
}

/**
 * Factory for `Prettify`s for `Traversable`s, and home of implicit conversions.
 */
object Prettify {
  /** Converts a `String` to a `Show` that shows it. */
  implicit def stringToPrettify (s: String): Prettify = Show(s)
  /**
   * Converts an `Option` to nothing (if `None`) or the `Show` of its content
   * (otherwise).
   */
  implicit def optionToPrettify [T] (maybe: Option[T]): Prettify =
    maybe match {
      case Some(p: Prettify) => p
      case Some(t) => Show(t)
      case None => Empty
    }
  /**
   * Constructs a `Prettify` for a `Traversable`.
   *
   * The function `elem` is called for each element of `trav`. The first
   * argument to `elem` is an element of `trav`, and the second is `Some(sep)`
   * for each element except the last, for which it is `None`.
   *
   * Returns just the concatenation of the outputs from `elem`, in order; this
   * method applies no other formatting.
   */
  def apply [T] (
        trav: Traversable[T], sep: Prettify,
        elem: (T,Option[Prettify]) => Prettify):
      Prettify =
    if (trav.isEmpty)
      Empty
    else {
      val someSep = Some(sep)
      def rest (t: Traversable[T]): Prettify = {
        val (head,tail) = (t.head,t.tail)
        if (tail.isEmpty) elem(head,None)
        else Concat(elem(head,someSep),rest(tail))
      }
      rest(trav)
    }
  /**
   * Constructs a `Prettify` for a `Traversable`.
   *
   * Returns the concatenation of the `Show`s of each element of `trav`, where
   * consecutive elements are separated by `sep`.
   */
  def apply [T] (trav: Traversable[T], sep: Prettify): Prettify =
    apply(
      trav,sep,(t: T, maybeSep: Option[Prettify]) => Concat(Show(t),maybeSep))
  /**
   * Constructs a `Prettify` for a `Traversable`.
   *
   * Returns the concatenation of `elem` applied to each element of `trav`, in
   * order; no separator appears between elements.
   */
  def apply [T] (trav: Traversable[T], elem: T => Prettify): Prettify =
    apply(trav,Empty,(t: T, maybeSep: Option[Prettify]) => elem(t))
}

/** An empty `Prettify`, rendered as the empty string. */
case object Empty extends Prettify

/** Specifies a token to insert into a token stream under construction. */
private[pretty] case class ToToken (token: Token) extends Prettify {
  override def pretty: Prettify =
    Indent("ToToken(",LeftBreak,token.toString,")")
}

/**
 * A `Prettify` that displays the pretty form of its argument.
 *
 * If the argument is a `PrettyLike`, it looks as specified by its `pretty`
 * method.
 *
 * Otherwise, the argument is rendered as its `toString` (embedded newlines
 * translate to [[archontophoenix.pretty.HardBreak]]s).
 */
class Show (a: => Any) extends Prettify {
  lazy val content: Any = a
  def expand (after: => Stream[Prettify]): Stream[Prettify] =
    content match {
      case pr: PrettyLike =>
        Stream.cons(pr.pretty,after)
      case x =>
        def expand (s: String): Stream[Prettify] =
          if (s.isEmpty)
            after
          else {
            val (hardBreaks,s1) = s.span(_ == '\n')
            val (unbroken,s2) = s1.span(_ != '\n')
            if (hardBreaks.isEmpty)
              Stream.cons(ToToken(Text(unbroken)),expand(s2))
            else if (unbroken.isEmpty)
              Stream.cons(ToToken(Hard(hardBreaks.length)),expand(s2))
            else
              Stream.cons(
                ToToken(Hard(hardBreaks.length)),
                Stream.cons(ToToken(Text(unbroken)),expand(s2)))
          }
        expand(x.toString)
    }
  override def pretty: Prettify =
    content match {
      case pr: PrettyLike =>
        Indent("Show[",LeftBreak,pr.pretty.pretty,"]")
      case _ =>
        toString
    }
  private[pretty] override def debugString: String =
    content match {
      case pr: PrettyLike => "Show[" + pr.pretty.debugString + "]"
      case _ => toString
    }
  override def toString: String = "Show(\"" + content + "\")"
}

/** Factory for [[archontophoenix.pretty.Show]]s. */
object Show {
  /** Returns a [[archontophoenix.pretty.Show]] of `content`. */
  def apply (content: => Any): Show = new Show(content)
}

/**
 * A `Prettify` that renders as `whenHorizontal`, if the immediately containing
 * [[archontophoenix.pretty.Group]] all fits on one line; otherwise, renders as
 * a newline, except when the `whenVertical` method is overridden, as in
 * [[archontophoenix.pretty.LeftBreak]] or
 * [[archontophoenix.pretty.AlignBreak]].
 *
 * If a `SoftBreak` is immediately preceded or followed by another break (either
 * a `HardBreak` or a `SoftBreak`), only the first break can be rendered as a
 * newline; subsequent adjacent soft breaks are ignored. This rule allows some
 * flexibility in formatting: you can specify that a `Pretty` `P` should be
 * rendered with a break point at its start without worrying whether the
 * `Pretty` `Q` that contains `P` also specifies a break point just before
 * showing `P`.
 */
class SoftBreak (val whenHorizontal: String) extends Prettify {
  {
    if (whenHorizontal.contains("\n"))
      UFail(
        "SoftBreak whenHorizontal cannot contain newline: " + whenHorizontal)
  }
  /**
   * Returns the string used to render this break, when the current `Group` does
   * not fit on one line; if `None` (the default), renders as newline.
   *
   * The result (if non-`None`) must not contain a newline. It may depend on
   * the current `margin`, `column`, and `limits`.
   */
  def whenVertical (margin: Int, column: Int, limits: Limits): Option[String] =
    None
  override def toString: String = "SoftBreak(\"" + whenHorizontal + "\")"
  
}

/**
 * A [[archontophoenix.pretty.SoftBreak]] whose horizontal rendering is the
 * empty string; also, a factory for `SoftBreak`s.
 **/
object SoftBreak extends SoftBreak("") {
  /** Returns a `SoftBreak` with the given horizontal rendering. */
  def apply (whenHorizontal: String): SoftBreak = new SoftBreak(whenHorizontal)
  def unapply (sb: SoftBreak): Option[String] = Some(sb.whenHorizontal)
}

/**
 * `SoftBreak` variant that does ''not'' render as a newline (even if the
 * immediately enclosing [[archontophoenix.pretty.Group]] does not fit) if
 * the resulting left margin would be greater than the current column. In
 * other words, breaks the current line only if the new margin is to the
 * ''left'' of the current position.
 *
 * Often more aesthetically appealing in formatting program source code than
 * `SoftBreak`. Using this in place of `SoftBreak` can allow some output to fit
 * within the specified maximum width that otherwise would not (perhaps by
 * sacrificing a philosophically consistent indentation rule).
 */
class LeftBreak (override val whenHorizontal: String)
    extends SoftBreak(whenHorizontal) {
  /**
   * Overridden to avoid outputting a newline that puts the margin farther to
   * the right than the current column.
   */
  override def whenVertical (
      margin: Int, column: Int, limits: Limits): Option[String] =
    if (margin < column + whenHorizontal.length) None else Some(whenHorizontal)
  override def toString: String = "LeftBreak(\"" + whenHorizontal + "\")"
}

/** Factory for [[archontophoenix.pretty.LeftBreak]]s. */
object LeftBreak extends LeftBreak("") {
  /** Returns a `LeftBreak` with the given horizontal rendering. */
  def apply (whenHorizontal: String): LeftBreak = new LeftBreak(whenHorizontal)
}

/**
 * `SoftBreak` variant that does ''not'' render as a newline (even if the
 * immediately enclosing [[archontophoenix.pretty.Group]] does not fit) if
 * the current left margin is to the right of the current column; instead, pads
 * with spaces to the left margin (thereby aligning the following text to the
 * margin).
 */
class AlignBreak (override val whenHorizontal: String)
    extends SoftBreak(whenHorizontal) {
  /** Overridden to align to the current left margin, if possible. */
  override def whenVertical (
      margin: Int, column: Int, limits: Limits): Option[String] = {
    val n = margin - column - whenHorizontal.length
    if (n < 0) None else Some(whenHorizontal + " " * n)
  }
  override def toString: String = "AlignBreak(\"" + whenHorizontal + "\")"
}

/** Factory for [[archontophoenix.pretty.AlignBreak]]s. */
object AlignBreak extends AlignBreak("") {
  /** Returns an `AlignBreak` with the given horizontal rendering. */
  def apply (whenHorizontal: String): AlignBreak =
    new AlignBreak(whenHorizontal)
}

/**
 * A `Prettify` that always renders as `count` consecutive newlines.
 *
 * Absorbs adjacent soft breaks, as described in
 * [[archontophoenix.pretty.SoftBreak]]. Does not absorb adjacent hard breaks.
 */
case class HardBreak (count: Int) extends Prettify {
  {
    if (count <= 0)
      UFail("HardBreak count must be at least one: " + count)
  }
}

/**
 * A [[archontophoenix.pretty.HardBreak]] with a count of 1; that is, a single
 * newline.
 */
object HardBreak extends HardBreak(1)

/**
 * A [[archontophoenix.pretty.Group]] containing a single
 * [[archontophoenix.pretty.SoftBreak]] (whose horizontal rendering is the
 * empty string).
 *
 * Useful to express an optional break point, where the current line is broken
 * only if absolutely necessary to fit within the specified maximum width
 * (whereas if one `SoftBreak` directly contained in a `Group` is broken, then
 * all `SoftBreak`s directly contained in the same `Group` are also broken).
 */
object WeakBreak extends Group(SoftBreak) {
  /** Returns a weak break whose group has the specified `indenter`. */
  def apply (indenter: Indenter): Group = Group(indenter,SoftBreak)
  /**
   * Returns a weak break whose soft break is rendered, when in a group that
   * all fits on one line, as `whenHorizontal` (instead of the default empty
   * string).
   */
  def apply (whenHorizontal: String): Group = Group(SoftBreak(whenHorizontal))
  private[pretty] override def debugString: String = "WeakBreak"
  override def toString: String = "WeakBreak"
}

/**
 * A concatenation of `Prettify`s: `head` followed by `tail`.
 */
class Concat (hd: => Prettify, tl: => Prettify) extends Prettify {
  /** The first argument supplied when this `Concat` was constructed. */
  lazy val head: Prettify = hd
  /** The second argument supplied when this `Concat` was constructed. */
  lazy val tail: Prettify = tl
  override def toString: String =
    "Concat@" + System.identityHashCode(this).toHexString
  private[pretty] override def debugString: String =
    "Concat(" + head.debugString + "," + tail.debugString + ")"
  override def pretty: Prettify = {
    def prettyTail (pr: Prettify): Prettify =
      pr match {
        case Concat(hh,tt) => Concat(hh.pretty,",",LeftBreak,prettyTail(tt))
        case _ => pr.pretty
      }
    Indent("Concat(",LeftBreak,head.pretty,",",LeftBreak,prettyTail(tail),")")
  }
}

/**
 * A factory for [[archontophoenix.pretty.Concat]]s.
 *
 * Because Scala 2.9 does not have repeated by-name parameters, this factory
 * includes versions of `apply` for up to 8 arguments. All versions
 * concatenate their arguments, in order (by nesting `Concat`s, if necessary).
 *
 * If you need to concatenate more items than eight, do so explicitly:
 *
 * {{{
 *  Concat(
      p1,p2,p3,p4,p5,p6,p7,
      Concat(p8,p9,p10,p11,p12))
 * }}}
 */
object Concat {
  def apply (head: => Prettify, tail: => Prettify): Concat =
    new Concat(head,tail)
  def apply (head: => Prettify, t1: => Prettify, t2: => Prettify): Concat =
    apply(head,apply(t1,t2))
  def apply (
        head: => Prettify, t1: => Prettify, t2: => Prettify, t3: => Prettify):
      Concat =
    apply(head,t1,apply(t2,t3))
  def apply (
        head: => Prettify, t1: => Prettify, t2: => Prettify, t3: => Prettify,
        t4: => Prettify):
      Concat =
    apply(head,t1,t2,apply(t3,t4))
  def apply (
        head: => Prettify, t1: => Prettify, t2: => Prettify, t3: => Prettify,
        t4: => Prettify, t5: => Prettify):
      Concat =
    apply(head,t1,t2,t3,apply(t4,t5))
  def apply (
        head: => Prettify, t1: => Prettify, t2: => Prettify, t3: => Prettify,
        t4: => Prettify, t5: => Prettify, t6: => Prettify):
      Concat =
    apply(head,t1,t2,t3,t4,apply(t5,t6))
  def apply (
        head: => Prettify, t1: => Prettify, t2: => Prettify, t3: => Prettify,
        t4: => Prettify, t5: => Prettify, t6: => Prettify, t7: => Prettify):
      Concat =
    apply(head,t1,t2,t3,t4,t5,apply(t6,t7))
  def unapply (ps: Concat): Option[(Prettify,Prettify)] =
    Some((ps.head,ps.tail))
}

/**
 * A group of `Prettify`s, where the group as a whole is rendered either
 * vertically or horizontally.
 *
 * The `content` of the group is usually a series of `Prettify`s (a
 * [[archontophoenix.pretty.Concat]]), possibly containing nested groups.
 * However, you can use the factory method of this class's companion object (or
 * other factories that create `Group`s) to specify a series of items that are
 * automatically concatenated, rather than specifying the concatenation
 * explicitly.
 * 
 * If a group is rendered vertically, every `SoftBreak` that is ``immediately``
 * contained within in it is rendered as a newline (or, if the `SoftBreak`
 * overrides its `whenVertical` method, as that method specifies). `SoftBreak`s
 * in ''nested'' groups may still be rendered horizontally if those groups
 * manage to fit within the maximum width.
 *
 * If a group is rendered horizontally, every `SoftBreak` in it is rendered
 * horizontally (as its `whenHorizontal`). Every group nested within the group
 * is necessarily rendered horizontally as well.
 *
 * To decide whether to render a group horizontally or vertically:
 * <ul>
 * <li> If this group contains any [[archontophoenix.pretty.HardBreak]]s, it
 * is rendered vertically.
 * <li> Otherwise, if this whole group fits within the maximum width when every
 * soft break in it is rendered as its `whenHorizontal` version, then the group
 * is rendered horizontally.
 * <li> Otherwise, the group is rendered vertically.
 * </ul>
 */
class Group (grouped: => Prettify) extends Prettify {
  /** The argument supplied to construct this group. */
  lazy val content: Prettify = grouped
  override def toString: String =
    "Group@" + System.identityHashCode(this).toHexString
  private[pretty] override def debugString: String =
    content match {
      case Indented(indenter,indented) =>
        indenter.toString + "(" + indented.debugString + ")"
      case _ =>
        "Group(" + content.debugString + ")"
    }
  override def pretty: Prettify =
    content match {
      case Indented(indenter,indented) =>
        Indent(indenter.toString,"(",LeftBreak,indented.pretty,")")
      case _ =>
        Indent("Group(",LeftBreak,content.pretty,")")
    }
}

/**
 * A boilerplate reducer.
 *
 * Scala 2.9 does not have repeated by-name parameters. This trait can simulate
 * them by concatenating up to 8 `Prettifies`, along with an optional extra
 * argument, given forms of `apply` that take either one `Prettify` or the extra
 * argument and a `Prettify`. That happens to be a desirable format for most
 * `Prettify` factories that could reasonably take concatenated arguments.
 */
private[pretty] trait RepeatedByNamePrettifies [This,ExtraArg] {
  def apply (content: => Prettify): This
  def apply (x: ExtraArg, content: => Prettify): This
  def apply (c1: => Prettify, c2: => Prettify): This = apply(Concat(c1,c2))
  def apply (x: ExtraArg, c1: => Prettify, c2: => Prettify): This =
    apply(x,Concat(c1,c2))
  def apply (c1: => Prettify, c2: => Prettify, c3: => Prettify): This =
    apply(Concat(c1,c2,c3))
  def apply (
        x: ExtraArg, c1: => Prettify, c2: => Prettify, c3: => Prettify):
      This =
    apply(x,Concat(c1,c2,c3))
  def apply (
        c1: => Prettify, c2: => Prettify, c3: => Prettify, c4: => Prettify):
      This =
    apply(Concat(c1,c2,c3,c4))
  def apply (
        x: ExtraArg, c1: => Prettify, c2: => Prettify, c3: => Prettify,
        c4: => Prettify):
      This =
    apply(x,Concat(c1,c2,c3,c4))
  def apply (
        c1: => Prettify, c2: => Prettify, c3: => Prettify, c4: => Prettify,
        c5: => Prettify):
      This =
    apply(Concat(c1,c2,c3,c4,c5))
  def apply (
        x: ExtraArg, c1: => Prettify, c2: => Prettify, c3: => Prettify,
        c4: => Prettify, c5: => Prettify):
      This =
    apply(x,Concat(c1,c2,c3,c4,c5))
  def apply (
        c1: => Prettify, c2: => Prettify, c3: => Prettify, c4: => Prettify,
        c5: => Prettify, c6: => Prettify):
      This =
    apply(Concat(c1,c2,c3,c4,c5,c6))
  def apply (
        x: ExtraArg, c1: => Prettify, c2: => Prettify, c3: => Prettify,
        c4: => Prettify, c5: => Prettify, c6: => Prettify):
      This =
    apply(x,Concat(c1,c2,c3,c4,c5,c6))
  def apply (
        c1: => Prettify, c2: => Prettify, c3: => Prettify, c4: => Prettify,
        c5: => Prettify, c6: => Prettify, c7: => Prettify):
      This =
    apply(Concat(c1,c2,c3,c4,c5,c6,c7))
  def apply (
        x: ExtraArg, c1: => Prettify, c2: => Prettify, c3: => Prettify,
        c4: => Prettify, c5: => Prettify, c6: => Prettify, c7: => Prettify):
      This =
    apply(x,Concat(c1,c2,c3,c4,c5,c6,c7))
  def apply (
        c1: => Prettify, c2: => Prettify, c3: => Prettify, c4: => Prettify,
        c5: => Prettify, c6: => Prettify, c7: => Prettify, c8: => Prettify):
      This =
    apply(Concat(c1,c2,c3,c4,c5,c6,c7,c8))
  def apply (
        x: ExtraArg, c1: => Prettify, c2: => Prettify, c3: => Prettify,
        c4: => Prettify, c5: => Prettify, c6: => Prettify, c7: => Prettify,
        c8: => Prettify):
      This =
    apply(x,Concat(c1,c2,c3,c4,c5,c6,c7,c8))
}

/**
 * Factory for [[archontophoenix.pretty.Group]]s.
 *
 * Because Scala 2.9 does not have repeated by-name parameters, this factory
 * includes versions of `apply` for up to 8 `Prettify`s. All versions
 * concatenate their arguments, in order. Use an explicit
 * [[archontophoenix.pretty.Concat]] if you need to concatenate more items than
 * eight.
 *
 * When you create a group with a method in this factory, you may optionally
 * supply an [[archontophoenix.pretty.Indenter]] as the first argument; if
 * supplied, the group is indented as specified.
 */
object Group extends RepeatedByNamePrettifies[Group,Indenter] {
  def apply (content: => Prettify): Group = new Group(content)
  def apply (indenter: Indenter, content: => Prettify): Group =
    apply(Indented(indenter,content))
  def unapply (g: Group): Option[Prettify] = Some(g.content)
}

/**
 * A `Prettify` that specifies that its `content` is to be indented according
 * to the rule given by its `indenter`.
 *
 * Typically, you specify an indenter for a [[archontophoenix.pretty.Group]] by
 * calling a `Group` factory method (or a method from one of the other factories
 * that creates `Group`s), rather than using an `Indented` directly. However, it
 * is sometimes useful to specify an indentation rule without creating a group,
 * as in the '''Do Loop''' example in the package comment for this package.
 */
class Indented (val indenter: Indenter, indented: => Prettify)
    extends Prettify {
  /** The second argument supplied to this `Indented`'s constructor. */
  lazy val content: Prettify = indented
  override def toString: String =
    "Indented@" + System.identityHashCode(this).toHexString + "(" + indenter +
      ")"
  private[pretty] override def debugString: String =
    "Indented(" + indenter + "," + indented.debugString + ")"
  override def pretty: Prettify =
    Indent(
      "Indented(",LeftBreak,indenter.toString,",",LeftBreak,content.pretty,")")
}

/**
 * Factory for [[archontophoenix.pretty.Indented]]s.
 *
 * Because Scala 2.9 does not have repeated by-name parameters, this factory
 * includes versions of `apply` for up to 8 `Prettify`s. All versions
 * concatenate their arguments, in order. Use an explicit
 * [[archontophoenix.pretty.Concat]] if you need to concatenate more items than
 * eight.
 *
 * When you create an `Indented` with a method in this factory, you may
 * optionally supply an [[archontophoenix.pretty.Indenter]] as the first
 * argument; if supplied, the `Indented` is indented as specified. Otherwise,
 * the indenter used is the indentation supplied by the
 * [[archontophoenix.pretty.Limits]] passed to `Prettify.render`.
 */
object Indented extends RepeatedByNamePrettifies[Indented,Indenter] {
  def apply (indenter: Indenter, content: => Prettify): Indented =
    new Indented(indenter,content)
  def apply (content: => Prettify): Indented = apply(Indent,content)
  def unapply (i: Indented): Option[(Indenter,Prettify)] =
    Some((i.indenter,i.content))
}

/**
 * A rule for indentation. Used in constructing an
 * [[archontophoenix.pretty.Indented]] or a [[archontophoenix.pretty.Group]].
 */
abstract class Indenter {
  /**
   * Returns the new left margin to use within the indented region, given the
   * current `margin`, `column`, and `limits`.
   */
  def newMargin (margin: Int, column: Int, limits: Limits): Int
}

/** Companion to [[archontophoenix.pretty.Indenter]]. */
object Indenter {
  /**
   * Implicitly converts `n` to an indentation rule that increases the left
   * margin by `n` columns.
   */
  implicit def toIndenter (n: Int): Indenter = Indent(n)
}

/** An indentation rule that does not change the left margin. */
case object NoIndent extends Indenter {
  def newMargin (margin: Int, column: Int, limits: Limits): Int = margin
}

/**
 * An indentation rule that indents by the default amount, as given by the
 * current [[archontophoenix.pretty.Limits]]; also, a factory for indentation
 * rules that indent by a fixed amount; and also, a factory for indented
 * [[archontophoenix.pretty.Group]]s.
 *
 * As an `Indenter` factory, returns an indentation rule that adds the given
 * amount to the left margin.
 *
 * However, you can also specify up to 8 `Prettify`s to create a `Group`
 * indented by the default amount, or you can prefix up to 8 `Group` arguments
 * with an `int` to create a group indented by the specified number of columns.
 */
case object Indent extends Indenter with RepeatedByNamePrettifies[Group,Int] {
  def newMargin (margin: Int, column: Int, limits: Limits): Int =
    margin + limits.defaultIndent
  def apply (indent: Int): Indenter =
    new Indenter {
      def newMargin (margin: Int, column: Int, limits: Limits): Int =
        margin + indent
      override def toString: String = "Indent(" + indent + ")"
    }
  def apply (c1: => Prettify): Group = Group(Indent,c1)
  def apply (indent: Int, c1: => Prettify): Group = Group(Indent(indent),c1)
  private[pretty] val maxPredefinedIndentSizeShift = 6
  private[pretty] val maxPredefinedIndentSize =
    1 << maxPredefinedIndentSizeShift
  private[pretty] val indents =
    (0 to maxPredefinedIndentSizeShift).toIndexedSeq.map(
      (i: Int) => " " * (1 << i))
  @tailrec private[pretty] def indent (n: Int, out: Appendable) {
    if (n > maxPredefinedIndentSize) {
      out.append(indents(maxPredefinedIndentSizeShift))
      indent(n - maxPredefinedIndentSize,out)
    } else
      for (i <- 0 to maxPredefinedIndentSizeShift)
        if ((n & (1 << i)) != 0) out.append(indents(i))
  }
}

/**
 * An indentation rule that indents to the current column; also, a factory for
 * indentation rules that indent by the current column plus a fixed amount; and
 * also, a factory for indented [[archontophoenix.pretty.Group]]s whose
 * indentation rule is based on the current column (so that the resulting groups
 * are aligned to their starting column).
 *
 * As an `Indenter` factory, returns an indentation rule that adds the given
 * amount to the current column.
 *
 * However, you can also specify up to 8 `Prettify`s to create a `Group`
 * aligned to the current column, or you can prefix up to 8 `Group` arguments
 * with an `int` to create a group indented by the specified number of columns
 * with respect to the current column.
 */
case object Align extends Indenter with RepeatedByNamePrettifies[Group,Int] {
  def newMargin (margin: Int, column: Int, limits: Limits): Int = column
  def apply (indent: Int): Indenter =
    new Indenter {
      def newMargin (margin: Int, column: Int, limits: Limits): Int =
        column + indent
      override def toString: String = "Align(" + indent + ")"
    }
  def apply (c1: => Prettify): Group = Group(Align,c1)
  def apply (indent: Int, c1: => Prettify): Group = Group(apply(indent),c1)
}

/**
 * Provides limits and defaults for a call to `Prettify.render`.
 *
 * All constructor arguments have defaults. The arguments are:
 * <ul>
 * <li> `maxWidth`: default, 80 columns. The renderer tries not to make its
 *  output wider than this.
 * <li> `defaultIndent`: default, 2 columns. The default amount by which to
 *  increase the left margin when indenting.
 * <li> `maxDepth`: default, 20. Groups nested deeper than this are rendered
 *  as `...`.
 * <li> `maxHeight`: default, 1024. The maximum number of lines to display.
 *  Output is truncated if it would be longer.
 * <li> `maxLength`: default, 64k characters. The maximum number of characters
 *  to display. Output is truncated if it would be longer.
 * </ul>
 */
case class Limits (
    maxWidth: Int = 80, defaultIndent: Int = 2, maxDepth: Int = 20,
    maxHeight: Int = 1024, maxLength: Int = 64 * 1024) {
  {
    if (maxWidth <= 0 || defaultIndent <= 0 || maxDepth <= 0 ||
        maxHeight <= 0 || maxLength <= 0)
      UFail("All limits must be positive: " + this)
  }
  override def toString: String =
    "maxWidth: " + maxWidth + "; defaultIndent: " + defaultIndent +
      "; maxDepth: " + maxDepth + "; maxHeight: " + maxHeight +
      "; maxLength: " + maxLength
}

/**
 * A [[archontophoenix.pretty.Limits]] where all limits are the maximum
 * positive `int` (effectively, unlimited). However, the default indentation is
 * still 2 columns.
 */
object Unlimited
  extends Limits(Int.MaxValue,2,Int.MaxValue,Int.MaxValue,Int.MaxValue)

/**
 * A [[archontophoenix.pretty.Limits]] where all parameters have their default
 * values.
 */
object DefaultLimits extends Limits()

/**
 * The internal representation of `Prettify`s, in which `Show`s are expanded and
 * groups and indentations are delimited by explicit opening and closing tokens.
 * The explicit delimiters make it easier to normalize groups (that is, to move
 * text around to avoid overlong lines if you fail to begin a group with a
 * break; see [[http://www.cs.kent.ac.uk/pubs/2005/2062/index.html this paper]]
 * for Olaf Chitil's explanation of normalization).
 */
private[pretty] abstract class Token

/** Token functions. */
private[pretty] object Token {
  /**
   * A function passed to `Stream.iterate` and then unzipped to produce a stream
   * of `Token`s from a `Prettify` (the latter rephrased as a stream of
   * `Prettify`s for convenience).
   */
  @tailrec def next (
        lastTokenRemDepthAndNextPrettifies: (Token,Int,Stream[Prettify])):
      (Token,Int,Stream[Prettify]) = {
    val (lastToken,remDepth,nextPrettifies) = lastTokenRemDepthAndNextPrettifies
    if (nextPrettifies.isEmpty)
      (End,remDepth,Stream.empty)
    else {
      val (head,tail) = (nextPrettifies.head,nextPrettifies.tail)
      head match {
        case t: ToToken =>
          t.token match {
            case Text("") => next((lastToken,remDepth,tail))
            case c: Close => (c,remDepth + 1,tail)
            case x => (x,remDepth,tail)
          }
        case Empty =>
          next((lastToken,remDepth,tail))
        case sh: Show =>
          next((lastToken,remDepth,sh.expand(tail)))
        case b: SoftBreak =>
          (Soft(b),remDepth,tail)
        case HardBreak(n) =>
          (Hard(n),remDepth,tail)
        case Concat(h,t) =>
          next((lastToken,remDepth,Stream.cons(h,Stream.cons(t,tail))))
        case Group(content) =>
          if (remDepth - 1 <= 0)
            (Text("..."),remDepth,tail)
          else {
            val o = Open()
            val c = ToToken(Close(new java.lang.ref.WeakReference(o)))
            (o,remDepth - 1,Stream.cons(content,Stream.cons(c,tail)))
         }
        case Indented(indenter,content) =>
          (Push(indenter),remDepth,
            Stream.cons(content,Stream.cons(Pop.toToken,tail)))
      }
    }
  }
  /** Normalizes an unnormalized stream of `Token`s. */
  def normalize (
      tokens: Stream[Token], positioner: Positioner = new Positioner): Norm = {
    @tailrec def norm (
        reversedLeadingText: List[TextLike], reversedLeadingDelims: List[Delim],
        tokens: Stream[Token]): Norm = {
      def done: Norm = {
        def normList (lt: List[Token], after: => Norm): Norm =
          lt match {
            case h :: t => Norm(positioner,h,normList(t,after))
            case Nil => after
          }
        normList(
          reversedLeadingText.reverse,
          normList(
            reversedLeadingDelims.reverse,
            Norm(positioner,tokens.head,normalize(tokens.tail,positioner))))
      }
      val (head,tail) = (tokens.head,tokens.tail)
      (reversedLeadingText,reversedLeadingDelims,head) match {
        case (_,Nil,_ : TextLike) =>
          done
        case (ts,ds,t : TextLike) =>
          norm(t :: ts,ds,tail)
        case (ts,(o @ Open()) :: ds,Close(wo)) =>
          assert(wo.get eq o)
          norm(ts,ds,tail)
        case (ts,ds,d: Delim) =>
          norm(ts,d :: ds,tail)
        case _ =>
          done
      }
    }
    norm(Nil,Nil,tokens)
  }
}

/**
 * Representation of a position in a `Prettify` sequence: `fromStart` is the
 * width from the start of the sequence, if every `Prettify` were formatted
 * horizontally (hard breaks ignored); `hardCount` is the number of hard breaks
 * to this position.
 */
private[pretty] case class Pos (fromStart: Int, hardCount: Int) {
  override def toString: String = fromStart.toString + "/" + hardCount
}

/** Cache of the farthest position reached in a normalized token stream. */
private[pretty] class Positioner {
  var pos: Pos = new Pos(0,0)
  var latest: Norm = null
  override def toString: String =
    "Positioner(" + pos + "," +
      (if (latest != null) latest.token.toString else "<null>") + ")"
}

/**
 * Normalized token stream.
 *
 * Includes:
 * <ul>
 * <li> the `positioner` used during the current call to `render`, to
 *  which this token stream is effectively local;
 * <li> the current `token`;
 * <li> and the rest of the stream `after` `token`.
 * </ul>
 */
private[pretty]
class Norm (positioner: Positioner, val token: Token, after: => Norm) {
  /** The `after` argument, checked for consistency. */
  lazy val next = {
    val n = after
    assert(
      ! (n.token.isInstanceOf[Text] &&
        (token.isInstanceOf[Open] || token.isInstanceOf[Close])),
      "Normalization failure: Text should not follow Open or Close: " +
        this + ", " + n)
    n
  }
  /** The `Pos` at `token`, captured when this `Norm` is constructed. */
  val pos: Pos = {
    val oldPos = positioner.pos
    positioner.pos =
      token match {
        case Hard(n) =>
          oldPos.copy(hardCount = oldPos.hardCount + n)
        case Close(wo) =>
          val o = wo.get
          if (o != null)
            o.closePos = Some(oldPos)
          oldPos
        case _ =>
          val w = width
          if (w == 0) oldPos else oldPos.copy(fromStart = oldPos.fromStart + w)
      }
    positioner.latest = this
    oldPos
  }
  /**
   * The width of `token`, assuming it's rendered horizontally (where hard
   * breaks are treated as zero-width).
   */
  def width: Int =
    token match {
      case Text(s) => s.length
      case Soft(b) => b.whenHorizontal.length
      case _ => 0
    }
  /**
   * Whether `token` fits in the given remaining width.
   *
   * If the token is an `Open`, traverses the group to find the corresponding
   * `Close` to determine whether the whole group fits (if the closing position
   * has not yet been cached in the `Open`). This may result in extended the
   * lookahead for this token stream.
   */
  def fits (remWidth: Int): Boolean =
    token match {
      case o: Open =>
        def fits (endPos: Pos) =
          endPos.hardCount == pos.hardCount &&
            endPos.fromStart - pos.fromStart <= remWidth
        def mightFit = o.closePos.isEmpty && fits(positioner.pos)
        while (mightFit)
          positioner.latest.next
        o.closePos match {
          case Some(p) => fits(p)
          case None => false
        }
      case Hard(_) =>
        false
      case _ =>
        width <= remWidth
    }
  override def toString: String =
    (if (positioner.latest eq this) ">" else "") + token + "@" + pos
}

/** Normalized token factory. */
private[pretty] object Norm {
  def apply (positioner: Positioner, token: Token, next: => Norm): Norm =
    new Norm(positioner,token,next)
}

/** Marks the end of a token stream. */
private[pretty] case object End extends Token

/** Token that is normalized like text. */
private[pretty] abstract class TextLike extends Token

/** Text (resulting from an expanded [[archontophoenix.pretty.Show]]). */
private[pretty] case class Text (text: String) extends TextLike

/** A break (hard or soft). */
private[pretty] abstract class Break extends Token

/** A soft break. */
private[pretty] case class Soft (break: SoftBreak) extends Break

/** A hard break. */
private[pretty] case class Hard (count: Int) extends Break

/**
 * A delimiter for either a [[archontophoenix.pretty.Group]] or an
 * [[archontophoenix.pretty.Indented]].
 */
private[pretty] abstract class Delim extends Token

/**
 * Start of a [[archontophoenix.pretty.Group]].
 *
 * If the token stream has been traversed as far as the corresponding `Close`,
 * the `Pos` at that `Close` is cached here, to avoid retraversing the stream.
 */
private[pretty] case class Open () extends Delim {
  var closePos: Option[Pos] = None
  override def toString: String =
    "Open@" + System.identityHashCode(this).toHexString + ">>" + (
        closePos match {
          case Some(p) => p.toString
          case None => "?"
        })
}

/**
 * End of a [[archontophoenix.pretty.Group]].
 *
 * Points back at the corresponding `Open` so that it can update the `Open`'s
 * end position cache once this `Close`'s position is computed. The `Open` is
 * held with a `WeakReference` so that, if the `Group` is very long and the
 * `Open` has already been rendered (meaning that it is no longer needed), then
 * the `Open` can be collected.
 */
private[pretty]
case class Close (open: java.lang.ref.WeakReference[Open]) extends Delim {
  override def toString: String =
    "Close(" + {
        val o = open.get
        if (o == null) "<null>" else o.toString
      } + ")"
}

/** Start of an [[archontophoenix.pretty.Indented]]. */
private[pretty] case class Push (indenter: Indenter) extends TextLike

/** End of an [[archontophoenix.pretty.Indented]]. */
private[pretty] case object Pop extends TextLike {
  val toToken = ToToken(this)
}

/** Factory for user-level error messages. */
private[pretty] object UFail {
  def apply (msg: String, t: Throwable = null) =
    throw new IllegalArgumentException(msg,t)
}

} // end package pretty
