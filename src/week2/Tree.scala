package week2

/**
  * Created by irynaromanenko on 24/07/2016.
  */
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
  def contains(x: Int) = false
}

case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int): IntSet = {
    if(x < elem) NonEmpty(elem, left incl x, right)
    else if (x > elem) NonEmpty(elem, left, right incl x)
    this
  }

  def contains(x: Int) = {
    if(x < elem) left.contains(x)
    else if(x > elem) right.contains(x)
    true
  }
}
