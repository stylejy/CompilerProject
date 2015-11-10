import org.parboiled2._

/**
  * Created by stylejy on 09/11/2015.
  * Thanks GOD for all.
  */

//Very simple matching test roule
class TestParser(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI}

  def Expression: Rule1[Seq[Object]] = rule {
    '(' ~ oneOrMore(Factor) ~ ')'
  }

  def Factor = rule { Letter | Number | Expression}

  def Letter = rule { capture(oneOrMore(CharPredicate.Alpha)) }

  def Number = rule { capture(oneOrMore(CharPredicate.Digit)) }

}

