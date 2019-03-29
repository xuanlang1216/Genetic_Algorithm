package genetics

import scala.collection.mutable.ListBuffer

object MergeSort {

  def mergeSort[T](inputData: List[T], comparator: (T, T) => Boolean): List[T] = {
    if (inputData.length < 2) {
      inputData
    } else {
      val mid: Int = inputData.length / 2
      val (left, right) = inputData.splitAt(mid)
      val leftSorted = mergeSort(left, comparator)
      val rightSorted = mergeSort(right, comparator)

      foreshadowMerge(leftSorted, rightSorted, comparator)
    }
  }

  // This method is given as an example, though it is very inefficient
  def merge[T](left: List[T], right: List[T], comparator: (T, T) => Boolean): List[T] = {
    var leftPointer = 0
    var rightPointer = 0

    var sortedList: List[T] = List()

    while (leftPointer < left.length && rightPointer < right.length) {
      if (comparator(left.apply(leftPointer), right.apply(rightPointer))) {
        sortedList = sortedList :+ left.apply(leftPointer)
        leftPointer += 1
      } else {
        sortedList = sortedList :+ right.apply(rightPointer)
        rightPointer += 1
      }
    }

    while (leftPointer < left.length) {
      sortedList = sortedList :+ left.apply(leftPointer)
      leftPointer += 1
    }
    while (rightPointer < right.length) {
      sortedList = sortedList :+ right.apply(rightPointer)
      rightPointer += 1
    }

    sortedList
  }

  // Same functionality as merge, but uses linked lists much more efficiently
  def foreshadowMerge[T](left: List[T], right: List[T], comparator: (T, T) => Boolean): List[T] = {

    if (left.length == 1 && right.length == 1) {
      if (comparator(left.head, right.head)) {
        List(left.head, right.head)
      } else {
        List(right.head, left.head)
      }
    } else {

      val leftIter = left.iterator
      val rightIter = right.iterator

      val sortedList: ListBuffer[T] = ListBuffer()

      var leftVal = leftIter.next
      var rightVal = rightIter.next

      var leftUpdated = false
      var rightUpdated = false

      while (leftIter.hasNext || rightIter.hasNext) {

        if (leftUpdated) {
          leftVal = leftIter.next
          leftUpdated = false
        }
        if (rightUpdated) {
          rightVal = rightIter.next
          rightUpdated = false
        }

        if (comparator(leftVal, rightVal)) {
          sortedList += leftVal
          if (!leftIter.hasNext) {
            sortedList += rightVal
            sortedList ++= rightIter
          } else {
            leftUpdated = true
          }
        } else {
          sortedList += rightVal
          if (!rightIter.hasNext) {
            sortedList += leftVal
            sortedList ++= leftIter
          } else {
            rightUpdated = true
          }
        }
      }
      sortedList.toList
    }
  }


  def selectionSort[Type](inputData: List[Type], comparator: (Type, Type) => Boolean): List[Type] = {
    var data: List[Type] = inputData
    for (i <- data.indices) {
      var minFound = data.apply(i)
      var minIndex = i
      for (j <- i until data.size) {
        val currentValue = data.apply(j)
        if (comparator(currentValue, minFound)) {
          minFound = currentValue
          minIndex = j
        }
      }
      data = data.updated(minIndex, data.apply(i))
      data = data.updated(i, minFound)
    }
    data
  }


}
