import TreeSorting._
import TreeBasic._
import TreeSplay._

object Test {
  def splayToRoot(tree:Tree, v:BigInt) = {
    require(isSorted(tree) && contains(tree, v))
    splay(tree, v)
  } ensuring {res => res match {case Node(_, x, _) => x == v}}

  def t1 = require(isSorted(Leaf))
  def test1 = t1
  def t2 = require(isSorted(Node(Leaf, 5, Leaf)))
  def test2 = t2
  def t3 = require(isSorted(Node(Node(Leaf, 2, Leaf), 5, Leaf)))
  def test3 = t3
  def t31 = require(!isSorted(Node(Node(Leaf, 6, Leaf), 5, Leaf)))
  def test31 = t31
  def t4 = require(!isSorted(Node(Node(Leaf, 2, Node(Leaf, 4, Leaf)), 3, Leaf)))
  def test4 = t4
  def t5 = require(isSorted(Node(Node(Leaf, 2, Leaf), 3, Node(Node(Leaf, 4, Leaf), 5, Leaf))))
  def test5 = t5
  def t6 = require(!isSorted(Node(Node(Leaf, 2, Leaf), 3, Node(Node(Leaf, 6, Leaf), 5, Leaf))))
  def test6 = t6
  def t7 = require(isSorted(Node(Leaf, -BigInt(5), Node(Node(Leaf, -BigInt(4), Leaf), -BigInt(2), Leaf))))
  def test7 = t7
  def tfail = require(isSorted(Node(Leaf, -BigInt(5), Node(Node(Leaf, -BigInt(6), Leaf), -BigInt(2), Leaf))))
  def testfail = tfail

  def tsplay1 = require(splay(Node(Leaf, BigInt(5), Leaf), 5) == Node(Leaf, BigInt(5), Leaf))
  def testsplay1 = tsplay1
  def tsplay2 = require(splay(Node(Node(Leaf, BigInt(1), Leaf), BigInt(5), Node(Leaf, BigInt(6), Leaf)), 5) == Node(Node(Leaf, BigInt(1), Leaf), BigInt(5), Node(Leaf, BigInt(6), Leaf)))
  def testsplay2 = tsplay2
  def tsplay3 = require(splay(Node(Node(Leaf, BigInt(4), Leaf), BigInt(5), Leaf), 4) == Node(Leaf, BigInt(4), Node(Leaf, BigInt(5), Leaf)))
  def testsplay3 = tsplay3
  def tsplay4 = require(splay(Node(Node(Leaf, BigInt(4), Node(Leaf, BigInt(5), Leaf)), BigInt(6), Leaf), 5) == Node(Node(Leaf, BigInt(4), Leaf), BigInt(5), Node(Leaf, BigInt(6), Leaf)))
  def testsplay4 = tsplay4
  def tsplay5 = require(splay(Node(Node(Node(Leaf, BigInt(1), Leaf), BigInt(2), Node(Node(Leaf, BigInt(3), Leaf), BigInt(4), Node(Leaf, BigInt(5), Leaf))), BigInt(6), Node(Node(Leaf, BigInt(7), Leaf), BigInt(8), Node(Leaf, BigInt(9), Leaf))), 5) == Node(Node(Node(Leaf, BigInt(1), Leaf), BigInt(2), Node(Node(Leaf, BigInt(3), Leaf), BigInt(4), Leaf)), BigInt(5), Node(Leaf, BigInt(6), Node(Node(Leaf, BigInt(7), Leaf), BigInt(8), Node(Leaf, BigInt(9), Leaf)))))
  def testsplay5 = tsplay5
  def tsplay6 = require(splay(Node(Node(Leaf, BigInt(-1467), Node(Leaf, BigInt(-1466), Leaf)), BigInt(-1465), Leaf), -1466) == Node(Node(Leaf, -1467, Leaf), -1466, Node(Leaf, -1465, Leaf)))
  def testsplay6 = tsplay6

  // def t100 = require(add(Node(Node(Leaf, 1, Leaf), 3, Leaf), 2) == Node(Node(Leaf, 1, Node(Leaf, 2, Leaf)), 3, Leaf))
  // def test100 = t100
}
