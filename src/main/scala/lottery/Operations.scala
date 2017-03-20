package lottery

import model.{Draw, Ticket, WinningClass}

object Operations {
  /**
    * Computes the ticket count for matched winning classed, given a draw and list of tickets.
    * (this function represents the recursive version of the `ticketsMatchItr`)
    *
    * @param draw a [[Draw]] against which the tickets are matched
    * @param tl a list of tickets that are matched against the `draw`
    * @return some map between winning classes and the tickets count that match that winning class,
    *         None if no winning class was matched
    *         e.g {{{
    *             Some(Map(WinningClass8 -> 56, WinningClass11 -> 140, WinningClass13 -> 168, WinningClass12 -> 336))
    *         }}}
    */
  def ticketsMatchRec(draw: Draw, tl: List[Ticket]): Map[WinningClass, Int] = {

    def ticketsMatch(d: Draw, tl: List[Ticket], acc: Map[WinningClass, Int]): Map[WinningClass, Int] = tl match {
      case t :: Nil => mergeMaps(acc, processTicket(d, t.normalTickets.toList, Map.empty))
      case h :: tail => ticketsMatch(d, tail, processTicket(d, h.normalTickets.toList, acc))
    }
    ticketsMatch(draw, tl, Map.empty)
  }

  /**
    * Computes the ticket count for matched winning classed, given a draw and list of tickets.
    *
    * @param draw a [[Draw]] against which the tickets are matched
    * @param tl a list of tickets that are matched against the `draw`
    * @return some map between winning classes and the tickets count that match that winning class,
    *         None if no winning class was matched
    *         e.g {{{
    *             Some(Map(WinningClass8 -> 56, WinningClass11 -> 140, WinningClass13 -> 168, WinningClass12 -> 336))
    *         }}}
    */
  def ticketsMatchItr(draw: Draw, tl: Seq[Ticket]): collection.mutable.Map[WinningClass, Int] = {
    val m = collection.mutable.Map.empty[WinningClass, Int]
    for (t <- tl; i <- t.normalTickets) {
      normalTicketMatch(draw, i).foreach({ winningClass =>
        val c = m.getOrElse(winningClass, 0) + 1
        m += ((winningClass, c))
      })
    }
    m
  }

  private[lottery] def normalTicketMatch(draw: Draw, t: Ticket): Option[WinningClass] = {
    val no = draw.no.intersect(t.numbers).size
    val stars = draw.stars.intersect(t.stars).size
    WinningClass(no, stars)
  }

  private def mergeMaps(map1: Map[WinningClass, Int], map2: Map[WinningClass, Int]): Map[WinningClass, Int] =
    map1 ++ map2.map{ case (k,v) => k -> (v + map1.getOrElse(k, 0)) }

  private def winningMap(wco: Option[WinningClass]): Map[WinningClass, Int] =
    wco.map(wc => Map(wc -> 1)).getOrElse(Map.empty)

  private def processTicket(d: Draw, tl: List[Ticket], acc: Map[WinningClass, Int]): Map[WinningClass, Int] = tl match {
    case t :: Nil => mergeMaps(winningMap(normalTicketMatch(d, t)), acc)
    case h :: tail => processTicket(d, tail, mergeMaps(acc, winningMap(normalTicketMatch(d, h))))
  }
}
