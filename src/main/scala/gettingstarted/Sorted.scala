package gettingstarted

object Sorted {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) = {
    def isOrdered(as: Array[A]) : Boolean = {
      if(as.tail.isEmpty) true
      else {
        if(!ordered(as.head, as.tail.head)) false
        else isOrdered(as.tail)
      }
    }

    if (as.isEmpty) true else isOrdered(as)
  }


}
