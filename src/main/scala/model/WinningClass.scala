package model

sealed trait WinningClass
case object WinningClass1 extends WinningClass
case object WinningClass2 extends WinningClass
case object WinningClass3 extends WinningClass
case object WinningClass4 extends WinningClass
case object WinningClass5 extends WinningClass
case object WinningClass6 extends WinningClass
case object WinningClass7 extends WinningClass
case object WinningClass8 extends WinningClass
case object WinningClass9 extends WinningClass
case object WinningClass10 extends WinningClass
case object WinningClass11 extends WinningClass
case object WinningClass12 extends WinningClass
case object WinningClass13 extends WinningClass

object WinningClass {
  def apply(no: Int, stars: Int): Option[WinningClass] =
    (no, stars) match {
      case (5, 2) => Some(WinningClass1)
      case (5, 1) => Some(WinningClass2)
      case (5, 0) => Some(WinningClass3)
      case (4, 2) => Some(WinningClass4)
      case (4, 1) => Some(WinningClass5)
      case (4, 0) => Some(WinningClass6)
      case (3, 2) => Some(WinningClass7)
      case (2, 2) => Some(WinningClass8)
      case (3, 1) => Some(WinningClass9)
      case (3, 0) => Some(WinningClass10)
      case (1, 2) => Some(WinningClass11)
      case (2, 1) => Some(WinningClass12)
      case (2, 0) => Some(WinningClass13)
      case _      => None
    }
}