package input

import java.io.InputStream
import model.{Draw, Ticket}

import scala.io.Source
import scala.util.Try

object Operations {

  /**
    * Reads the first data line from file `fileName` and tries to create a [[Ticket]] from it
    *
    * @param fileName the name of the file that should have a line of data in the following format
    *                 {{{1,2,3,4,5,6,7,8,9,10:1,2,3,4,5}}}.
    *                 The numbers before the colon represent the numbers of a ticket [[Ticket.numbers]].
    *                 The numbers after the colon represent the stars of a ticket [[Ticket.stars]].
    * @return some ticket if the file read was successful and the data was in the correct pattern and
    *         had the correct values for a [[Ticket]] object creation, None otherwise
    */
  def readSystemTicketFrom(fileName: String): Option[Ticket] =
    readObjectFrom[Ticket](fileName)(createObject[Ticket](_, (no, stars) => Ticket(no, stars)))

  /**
    * Reads the first data line from file `fileName` and tries to create a [[Draw]] from it
    * @param fileName the name of the file that should have a line of data in the following format
    *                 {{{1,2,3,4,5:1,2}}}.
    *                 The numbers before the colon represent the numbers of a draw [[Draw.no]].
    *                 The numbers after the colon represent the stars of a draw [[Draw.stars]].
    * @return some draw if the file read was successful and the data was in the correct pattern and
    *         had the correct values for a [[Draw]] object creation, None otherwise
    */
  def readDrawFrom(fileName: String): Option[Draw] =
    readObjectFrom[Draw](fileName)(createObject[Draw](_, (no, stars) => Draw(no, stars)))

  /**
    * Reads multiple tickets from `fileName`
    *
    * @param fileName the name of the file that should have multiple lines of data in the following format:
    *                 {{{1,2,3,4,5,6,7,8,9,10:1,2,3,4,5}}}.
    *                 The numbers before the colon represent the numbers of a ticket [[Ticket.numbers]].
    *                 The numbers after the colon represent the stars of a ticket [[Ticket.stars]].
    * @return a sequence of tickets
    */
  def readTicketsFrom(fileName: String): Seq[Ticket] = {
    val tickets = for {
      it <- getIteratorFrom(fileName).toSeq
      l  <- it
    } yield createObject(l, (no, stars) => Ticket(no, stars))
    tickets.collect({case Some(e) => e})
  }

  /**
    * Returns a String iterator for the file lines if the file exist
    *
    * @param fileName the file for which the iterator is created
    */
  private[input] def getIteratorFrom(fileName: String): Option[Iterator[String]] = {
    val stream = Option(getClass.getResourceAsStream(s"/$fileName"))
    def iterator(is: InputStream) = Source.fromInputStream(is).getLines
    stream.map(is => iterator(is))
  }

  /**
    * Reads an object of type T from the first line of file `fileName`
    *
    * @param fileName the name of the file from which the data is read
    * @param f a function that trie to create an object of type T from a string
    * @tparam T the type of the object to be created
    * @return some object if the read was successful and `f` returned some object, None otherwise
    */
  private[input] def readObjectFrom[T](fileName: String)(f: String => Option[T]): Option[T] = {
    val stream = Option(getClass.getResourceAsStream(s"/$fileName"))
    def iterator(is: InputStream) = Source.fromInputStream(is).getLines

    stream.map(inputStream =>
      iterator(inputStream)).flatMap(it =>
      f(it.next()))
  }

  /**
    * Creates an object of type T
    *
    * @param line a string that should have comma separated numbers, a colon, and then other comma separated numbers
    *            e.g. {{{1,2,3,4,5:1,2}}}
    *            The numbers before the colon represent the numbers of a ticket or a draw
    *            The numbers after the colon represent the stars of a ticket or a draw
    * @param f a functions that instantiates a T object from two sets of numbers.
    *          Considering the similarities between a [[Ticket]] and a [[Draw]], the function is intended to
    *          instantiate one of these two types of objects
    * @tparam T the type of the resulting created object
    * @return some T object if the string line was correctly defined
    *         (the pattern was correctly followed, the numbers were in the expected range and size considering the T
    *         object constraints), None otherwise
    */
  private[input] def createObject[T](line: String, f: (Set[Int], Set[Int]) => T): Option[T] = {
    Try {
      val Array(ns, ss) = line.split(":")
      val no = ns.split(",").map(n => n.toInt).toSet
      val stars = ss.split(",").map(s => s.toInt).toSet
      f(no, stars)
    }.toOption
  }
}
