package model

import org.scalatest.{Matchers, WordSpec}
import TicketSpec._

import scala.collection.mutable.ArrayBuffer

class TicketSpec extends WordSpec with Matchers {
  "TicketSpec" should {
    "successfully create a ticket" in {
      val t = Ticket(validNumbersSet1, validStarsSet1)
      t.stars shouldBe validStarsSet1
      t.numbers shouldBe validNumbersSet1
    }
    "throw an exception at creation" when {
      "numbers size is less than 5" in {
        val e = intercept[IllegalArgumentException] {
          Ticket(invalidNumbersSet1, validStarsSet1)
        }
        e.getMessage should include ("Ticket must contain between 5 and 10 numbers")
      }
      "numbers size is greater than 10" in {
        val e = intercept[IllegalArgumentException] {
          Ticket(invalidNumbersSet2, validStarsSet1)
        }
        e.getMessage should include ("Ticket must contain between 5 and 10 numbers")
      }
      "one of the numbers is out of 1 to 50 range" in {
        val e = intercept[IllegalArgumentException] {
          Ticket(invalidNumbersSet3, validStarsSet1)
        }
        e.getMessage should include ("The value of each number must be between 1 and 50")
      }
      "stars size is less than 2" in {
        val e = intercept[IllegalArgumentException] {
          Ticket(validNumbersSet1, invalidStarsSet1)
        }
        e.getMessage should include ("Ticket must contain between 2 and 5 stars")
      }
      "stars size is greater than 5" in {
        val e = intercept[IllegalArgumentException] {
          Ticket(validNumbersSet1, invalidStarsSet2)
        }
        e.getMessage should include ("Ticket must contain between 2 and 5 stars")
      }
      "one of the stars values is out of 1 to 12 range" in {
        val e = intercept[IllegalArgumentException] {
          Ticket(validNumbersSet1, invalidStarsSet3)
        }
        e.getMessage should include ("The value of each star must be between 1 and 12")
      }
    }
    "correctly stringify a Ticket" in {
      validTicket.toString shouldBe "Ticket(numbers: Set(5, 1, 2, 3, 4), stars: Set(1, 2))"
    }
    "correctly generate the normal tickets" when {
      "the ticket is already a normal ticket" in {
        validTicket.normalTickets.next() shouldBe validTicket
      }
      "the ticket is a system ticket" in {
        validSystemTicket1.normalTickets.toSet shouldBe normalTicketResult
        validSystemTicket2.normalTickets.size shouldBe 2520
      }
    }
    "correctly determine if a ticket is system or normal" when {
      "the ticket is normal" in {
        validTicket.isSystemTicket shouldEqual false
      }

      "the ticket is system" in {
        validSystemTicket1.isSystemTicket shouldEqual true
      }
    }
    "correctly compute the stars combinations" in {
      validSystemTicket1.starsComb.toSet shouldBe starSet2Combinations
    }
    "correctly compute the numbers combinations" in {
      validSystemTicket3.noComb.toSet shouldBe noSet3Combinations
    }
  }
}
object TicketSpec {
  private val validNumbersSet1 = Set(1, 2, 3, 4, 5)
  private val validNumbersSet2 = Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  private val validNumbersSet3 = Set(1, 2, 3, 4, 5, 6)
  private val invalidNumbersSet1 = Set(1, 2, 3, 4)
  private val invalidNumbersSet2 = Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  private val invalidNumbersSet3 = Set(1, 2, 3, 4, 0)
  private val validStarsSet1 = Set(1, 2)
  private val validStarsSet2 = Set(1, 2, 3)
  private val validStarsSet3 = Set(1, 2, 3, 4, 5)
  private val invalidStarsSet1= Set(1)
  private val invalidStarsSet2= Set(1, 2, 3, 4, 5, 6)
  private val invalidStarsSet3= Set(1, 13)
  private val validTicket = Ticket(validNumbersSet1, validStarsSet1)
  private val validSystemTicket1 = Ticket(validNumbersSet1, validStarsSet2)
  private val validSystemTicket2 = Ticket(validNumbersSet2, validStarsSet3)
  private val validSystemTicket3= Ticket(validNumbersSet3, validStarsSet3)
  private val normalTicketResult = Set(Ticket(validNumbersSet1, Set(1, 2)), Ticket(validNumbersSet1, Set(1, 3)), Ticket(validNumbersSet1, Set(2, 3)))
  private val starSet2Combinations = Set(ArrayBuffer(1, 2), ArrayBuffer(1, 3), ArrayBuffer(2, 3))
  private val noSet3Combinations = Set(ArrayBuffer(5, 1, 6, 2, 4), ArrayBuffer(5, 1, 6, 2, 3), ArrayBuffer(5, 1, 6, 3, 4), ArrayBuffer(1, 6, 2, 3, 4), ArrayBuffer(5, 6, 2, 3, 4), ArrayBuffer(5, 1, 2, 3, 4))
}
