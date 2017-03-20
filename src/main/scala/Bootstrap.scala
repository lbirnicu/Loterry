import input.Operations._
import lottery.Operations._

object Bootstrap extends App {

  /**
    * ==========Task 2==========
    * Read the numbers of a system ticket from a file and output all the normal ticket combinations
    */
  val systemTicketFile = "systemTicket.txt"
  var i = 1
  readSystemTicketFrom(systemTicketFile).foreach(t => t.normalTickets.foreach(e => {println(s"$i $e"); i = i+1}))

  /**
    * ==========Task 3=========
    *  Read the winning numbers of a draw (5 out of 50 and 2 out of 12) from a file
    *  Read tickets that participated in the draw from a file
    *  Output an overview about winnings for the draw in a form: Winning class N - number of winning tickets X
    */
  val drawFile = "draw.txt"
  val ticketsFile = "tickets.txt"
  println(
    for {
      d  <- readDrawFrom(drawFile)
      tl = readTicketsFrom(ticketsFile)
    } yield ticketsMatchItr(d, tl)
  )
}
