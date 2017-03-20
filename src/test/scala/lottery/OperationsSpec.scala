package lottery

import model._
import org.scalatest.{Matchers, WordSpec}
import Operations._
import OperationsSpec._

class OperationsSpec extends WordSpec with Matchers {
  "OperationsSpec" should {
    "correctly determine the WinningClass" when {
      "the WinningClass1 is matched" in {
        normalTicketMatch(draw1, ticket1) shouldBe Some(WinningClass1)
      }
      "the WinningClass2 is matched" in {
        normalTicketMatch(draw1, ticket4) shouldBe Some(WinningClass2)
      }
      "the WinningClass3 is matched" in {
        normalTicketMatch(draw1, ticket5) shouldBe Some(WinningClass3)
      }
      "the WinningClass4 is matched" in {
        normalTicketMatch(draw1, ticket6) shouldBe Some(WinningClass4)
      }
      "the WinningClass5 is matched" in {
        normalTicketMatch(draw1, ticket7) shouldBe Some(WinningClass5)
      }
      "the WinningClass6 is matched" in {
        normalTicketMatch(draw1, ticket8) shouldBe Some(WinningClass6)
      }
      "the WinningClass7 is matched" in {
        normalTicketMatch(draw1, ticket9) shouldBe Some(WinningClass7)
      }
      "the WinningClass8 is matched" in {
        normalTicketMatch(draw1, ticket3) shouldBe Some(WinningClass8)
      }
      "the WinningClass9 is matched" in {
        normalTicketMatch(draw1, ticket10) shouldBe Some(WinningClass9)
      }
      "the WinningClass10 is matched" in {
        normalTicketMatch(draw1, ticket11) shouldBe Some(WinningClass10)
      }
      "the WinningClass11 is matched" in {
        normalTicketMatch(draw1, ticket12) shouldBe Some(WinningClass11)
      }
      "the WinningClass12 is matched" in {
        normalTicketMatch(draw1, ticket13) shouldBe Some(WinningClass12)
      }
      "the WinningClass13 is matched" in {
        normalTicketMatch(draw1, ticket2) shouldBe Some(WinningClass13)
      }
      "no winning class is matched" in {
        normalTicketMatch(draw1, ticket14) shouldBe None
      }
    }
    "correctly compute the count for each winning class" when {
      "there is no winning class" in {
        ticketsMatchItr(draw2, Seq(systemTicket1)) shouldBe Map.empty
      }
      "one winning class found" in {
        ticketsMatchItr(draw3, Seq(systemTicket1)) shouldBe Map(WinningClass13 -> 560)
      }
      "two winning class found" in {
        ticketsMatchItr(draw4, Seq(systemTicket1)) shouldBe Map(WinningClass13 ->336, WinningClass12 ->224)
      }
      "four winning class found" in {
        ticketsMatchItr(draw5, Seq(systemTicket1)) shouldBe res4
      }
      "one winning class for two tickets" in {
        ticketsMatchItr(draw3, Seq(systemTicket1, systemTicket1)) shouldBe Map(WinningClass13 -> 1120)
      }
    }
    "correctly compute the count for each winning class using ticketsMatch1" when {
      "there is no winning class" in {
        ticketsMatchRec(draw2, List(systemTicket1)) shouldBe Map.empty
      }
      "one winning class found" in {
        ticketsMatchRec(draw3, List(systemTicket1)) shouldBe Map(WinningClass13 -> 560)
      }
      "two winning class found" in {
        ticketsMatchRec(draw4, List(systemTicket1)) shouldBe Map(WinningClass13 ->336, WinningClass12 ->224)
      }
      "four winning class found" in {
        ticketsMatchRec(draw5, List(systemTicket1)) shouldBe res4
      }
      "one winning class for two tickets" in {
        ticketsMatchRec(draw3, List(systemTicket1, systemTicket1)) shouldBe Map(WinningClass13 -> 1120)
      }
    }
  }
}
object OperationsSpec {
  private val stars2 = Set(1, 2)
  private val stars0 = Set(3, 4)
  private val stars1 = Set(1, 3)
  private val no5 = Set(1, 2, 3, 4, 5)
  private val no4 = Set(1, 2, 3, 4, 6)
  private val no3 = Set(1, 2, 3, 7, 6)
  private val no2 = Set(1, 2, 8, 7, 6)
  private val no1 = Set(1, 9, 8, 7, 6)
  private val draw1 = Draw(no5, stars2)
  private val ticket1 = Ticket(no5, stars2)
  private val ticket2 = Ticket(no2, stars0)
  private val ticket3 = Ticket(no2, stars2)
  private val ticket4 = Ticket(no5, stars1)
  private val ticket5 = Ticket(no5, stars0)
  private val ticket6 = Ticket(no4, stars2)
  private val ticket7 = Ticket(no4, stars1)
  private val ticket8 = Ticket(no4, stars0)
  private val ticket9 = Ticket(no3, stars2)
  private val ticket10 = Ticket(no3, stars1)
  private val ticket11 = Ticket(no3, stars0)
  private val ticket12 = Ticket(no1, stars2)
  private val ticket13 = Ticket(no2, stars1)
  private val ticket14 = Ticket(no1, stars1)
  private val stNo1 = Set(1,2,3,4,5,6,7,8,9,10)
  private val stStars1 = Set(1,2,3,4,5)
  private val systemTicket1 = Ticket(stNo1, stStars1)
  private val draw2 = Draw(Set(11,12,13,14,15), Set(6,7))
  private val draw3 = Draw(Set(9,10,11,12,13), Set(6,7))
  private val draw4 = Draw(Set(9,10,11,12,13), Set(5,6))
  private val draw5 = Draw(Set(9,10,11,12,13), Set(5,4))
  private val res4 = Map(WinningClass8 -> 56, WinningClass11 -> 140, WinningClass13 -> 168, WinningClass12 -> 336)
}
