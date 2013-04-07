package kappa

import scala.collection.LinearSeqLike
import scala.collection.mutable.Builder


trait DoublyLinkedCell[Elem >: Null] {
  this: Elem =>

  var prev: Elem = null
  var next: Elem = null
}


trait DoublyLinkedList[Elem >: Null <: DoublyLinkedCell[Elem],
                       Repr <: DoublyLinkedList[Elem, Repr]]
    extends LinearSeqLike[Elem, Repr] {
  this: Repr =>

  /** First agent in the doubly-linked list representing the mixture. */
  protected var _head: Elem = null

  /** First agent in the doubly-linked list representing the mixture. */
  override def head: Elem =
    if (_head == null) throw new NoSuchElementException(
      "attempt to reference head element in empty mixture")
    else _head

  override def isEmpty: Boolean = _head == null

  /** The number of agents in this mixture. */
  protected var _length: Int = 0

  def clear {
    _head = null
    _length = 0
  }

  private class DoublyLinkedListIterator extends Iterator[Elem] {

    private var _next: Elem = _head

    def next = {
      val n = _next
      if (n != null) _next = n.next
      n
    }

    def hasNext = _next != null
  }


  /**
   * Make a copy of this doubly-linked list.
   *
   * The resulting doubly-linked list is completely independent of
   * the original and can hence be operated on without the fear of
   * destroying the original.
   *
   * @return a copy of this mixture.
   */
  def copy: DoublyLinkedList[Elem, Repr] = {
    val b = this.newBuilder
    for (u <- this)
      b += u
    b.result
  }

  /** Add a single agent to this mixture. */
  def +=(u: Elem): this.type = {

    u.prev = null
    u.next = _head
    if (_head != null)
      _head.prev = u
    _head = u
    _length += 1
    this
  }

  /** Remove a single agent from this mixture. */
  def -=(u: Elem): this.type = {

    val p = u.prev
    val n = u.next
    if (p != null) p.next = n
    if (n != null) n.prev = p
    if (_head == u) _head = n
    _length -= 1
    this
  }

  /**
    * Append another mixture to this one.
    *
    * The doubly-linked list representing the mixture `that` will be
    * appended to this mixture.  After this concatenation `that`
    * becomes invalid and should not be operated on any longer.
    *
    * @param that the mixture to append to `this`.
    * @return this mixture with `that` appended to it.
    */
  def ++=(that: DoublyLinkedList[Elem, Repr]): this.type = {
    var a = that._head
    var last: Elem = null
    while (a != null) {
      last = a
      a = a.next
    }
    if (last != null) { // check if `that` is not empty
      if (this._head != null) this._head.prev = last
      last.next = this._head
      this._head = that._head
    }
    _length += that._length
    this
  }

  /**
    * Generates and appends `x - 1` copies of this mixture to this
    * mixture.
    */
  def *=(n: Int): this.type =
    if (n <= 0) throw new IllegalArgumentException(
      "attempt to create a negative number of copies of a mixture")
    else {
      val copies = for (i <- 1 until n) yield this.copy
      for (c <- copies)
        this ++= c
      this
    }

  /**
    * Generates `x` concatenated copies of this mixture and returns
    * the result.
    */
  def *(n: Int): this.type = {
    if (n < 0) {
      throw new IllegalArgumentException(
        "attempt to create a negative number of copies of a mixture")
    } else if (n == 0) {
      this.clear
    } else {
      this *= n
    }
    this
  }


  // -- Core Seq[Mixture.Agent] API --
  @inline def apply(idx: Int): Elem = {
    if (idx >= _length) throw new IndexOutOfBoundsException
      (iterator drop idx).next
  }
  @inline override def iterator: Iterator[Elem] = new DoublyLinkedListIterator
  @inline override def length: Int = _length


  // -- Extra Seq[Mixture.Agent] API --
  @inline override def foreach[U](f: Elem => U): Unit = {
    var u = _head
    while (u != null) {
      f(u)
      u = u.next
    }
  }
}

