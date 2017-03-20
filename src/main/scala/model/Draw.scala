package model

/**
  * Data model representing the winning numbers and stars in a lottery draw.
  * During the draw, there can be extracted exactly 5 distinct numbers and 2 distinct stars.
  * The extracted numbers have values between 1 and 50, while the stars can have
  * values between 1 and 12.
  *
  * @param no the numbers that were extracted
  * @param stars the star numbers that were extracted
  */
case class Draw(no: Set[Int], stars: Set[Int]) {
  require(stars.size == 2, "Draw stars count must be 2!")
  require(no.size == 5)
  stars.foreach({ s =>
    require(s >= 1 && s <= 12)
  })
  no.foreach({ n =>
    require(n >= 1 && n <= 50)
  })
}