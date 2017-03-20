package model
import Ticket._

/**
  * Data model representing a lottery ticket.
  * A lottery ticket can contain between 5 and 10 distinct numbers having values between 1 and 50 and
  * between 2 and 5 stars with values between 1 and 12.
  *
  * @param numbers the set of numbers marked on the lottery ticket
  * @param stars the set of star numbers marked on the lottery ticket
  */
case class Ticket(numbers: Set[Int], stars: Set[Int]) {

  require(numbers.size >= 5 && numbers.size <= 10,
    "Ticket must contain between 5 and 10 numbers")
  numbers.foreach({ n =>
    require(n >= 1 && n <= 50,
      "The value of each number must be between 1 and 50")
  })

  require(stars.size >= 2 && stars.size <= 5,
  "Ticket must contain between 2 and 5 stars")
  stars.foreach({ s =>
    require(s >= 1 && s <= 12, "The value of each star must be between 1 and 12")
  })

  override def toString: String = s"Ticket(numbers: ${numbers.toString}, stars: ${stars.toString})"

  /**
    * Returns an Iterator over `normal` tickets of `system` ticket
    *
    * Each ticket can be a `normal` ticket or a `system` ticket.
    * A `normal` ticket means that it contains exactly 5 numbers and 2 star numbers, while a `system` ticket has
    * more than 5 numbers or/and more that 2 stars.
    *
    * From a `system` ticket can be generated a sequence of `normal` tickets.
    * The `normal` tickets represent the association between all combination of
    * 5 numbers from all numbers a `system` ticket has, with all combinations of 2 stars from all star numbers a
    * `system` ticket has.
    *
    * e.g Considering this `system` ticket: {{{Ticket(Set(1,2,3,4,5), Set(1,2,3))}}}
    * the resulting sequence of `normal` tickets is:
    * {{{
    *   Seq(Ticket(Set(1,2,3,4,5), Set(1,2)),
    *       Ticket(Set(1,2,3,4,5), Set(1,3)),
    *       Ticket(Set(1,2,3,4,5), Set(2,3)))
    * }}}
    */
  def normalTickets: Iterator[Ticket] =
   if (isSystemTicket)
      for (i <- noComb; j <- starsComb) yield Ticket(i, j)
    else Iterator(this)

  private[model] def isSystemTicket: Boolean = numbers.size > 5 || stars.size > 2
  private[model] def starsComb = stars.toSeq.combinations(NormalTicketMaxStars)
  private[model] def noComb = numbers.toSeq.combinations(NormalTicketMaxNumbers)
}

object Ticket {
  def apply(no: Seq[Int], stars: Seq[Int]): Ticket = Ticket(no.toSet, stars.toSet)
  private val NormalTicketMaxStars = 2
  private val NormalTicketMaxNumbers = 5
}

