package input

import input.Operations._
import input.OperationsSpec._
import model.Ticket
import org.scalatest.{Matchers, WordSpec}

class OperationsSpec extends WordSpec with Matchers {

  "OperationsSpec" should {
    "correctly load system ticket" in {
      readSystemTicketFrom("validSystemTicket.txt") shouldBe Some(validSystemTicket)
    }
    "return None" when {
      "the system ticket file doesn't exist" in {
        readSystemTicketFrom("invalidName.txt") shouldBe None
      }
      "the system ticket is incorrectly defined" in {
        readSystemTicketFrom("invalidSystemTicket.txt") shouldBe None
      }
    }

    //TODO add more unit tests
  }
}

object OperationsSpec {
  private val validSystemTicket = Ticket(Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Set(1, 2, 3, 4, 5))
}
