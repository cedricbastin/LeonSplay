import TreeSorting._
import TreeBasic._

object Test {
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
  def t100 = require(add(Node(Node(Leaf, 1, Leaf), 3, Leaf), 2) == Node(Node(Leaf, 1, Node(Leaf, 2, Leaf)), 3, Leaf))
  def test100 = t100
}
