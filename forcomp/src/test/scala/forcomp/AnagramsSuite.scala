package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd").toSet === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)).toSet)
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert").toSet === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)).toSet)
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")).toSet === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)).toSet)
  }

  test("sentenceOccurrences: abcd efabc baba") {
    assert(sentenceOccurrences(List("abcd", "efabc", "baba")).toSet === List(('a', 4), ('b', 4), ('c', 2), ('d', 1), ('e', 1), ('f', 1)).toSet)
  }


  test("dictionaryByOccurrences.get: eat") {
    val dictionaryByOccurenceSet = dictionaryByOccurrences.map(re => (re._1.toSet, re._2)).toMap
    assert(dictionaryByOccurenceSet.get(Set(('a', 1), ('e', 1), ('t', 1))).map(x => x.toSet) === Some(Set("ate", "eat", "tea")))
  }


  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }


  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }


  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }


  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}
