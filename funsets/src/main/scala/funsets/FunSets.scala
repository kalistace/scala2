package funsets


/**
  * 2. Purely Functional Sets.
  */
object FunSets {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = x => x == elem


  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = elem => s(elem) || t(elem)

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = elem => s(elem) && t(elem)

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = elem => s(elem) && !t(elem)

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = elem => s(elem) && p(elem)


  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s, a) && !p(a)) false
      else iter(a + 1)
    }

    iter(-1000)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    * SO there must be ONE that DOESNT satify p, hence the double negation*/
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, (elem => !p(elem)))

  //there is at least one man with red hair
  // BECOMES
  //it's not true that allMan haven't got red hair


  /*The solution means not all elements in s satisfies !p,
  which in turn indicates there is at least one element in s satisfies p.*/

  /*The following two statements are equivalent:

"It is not true that all men have red hair."

"There exists at least one man who does not have red hair."

Hence ¬∀x φ¬∀x φ is the same as ∃x ¬φ∃x ¬φ.

The following are equivalent:

"It is not true that some men have green hair."

"All men have non-green hair."

Hence ¬∃x φ¬∃x φ is the same as ∀x ¬φ∀x ¬φ.*/


  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set = x => exists(s, (y: Int) => f(y) == x)

  /*The map method here would return true for every given x and function f,
  if x is a result of the function f applied to the elements of the original set.

It is done by checking that if we go over the original map and apply f to every element of
 this map, at least one of them will be equal to x

 If you say: val set2 = map(set1, f),

then set2(x) will returns true if and only if there exits y in set1 such as f(y) == x

That's exactly what exists(set1, y => f(y) == x) is checking.

To put it an other way, an integer is in set2 only if you can obtain it by applying f to an element of set1

 */

  /**
    * Displays the contents of a set
    */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }
}
