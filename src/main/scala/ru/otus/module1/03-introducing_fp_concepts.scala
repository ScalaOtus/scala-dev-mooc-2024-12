package ru.otus.module1

import scala.annotation.tailrec
import scala.language.postfixOps



/**
 * referential transparency
 */




 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int = if(n <= 0) 1 else n * factRec(n - 1)


  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(x: Int, accum: Int): Int = {
      if( n <= 0) accum
      else loop(x - 1, x * accum)
    }
    loop(n, 1)
  }




  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   */


}



object hof{

  def dumb(string: String): Unit = {
    Thread.sleep(1000)
    println(string)
  }

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
    result
  }



  // изменение поведения ф-ции


  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)

  lazy val isEven: Int => Boolean = not(isOdd)



  // изменение самой функции

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def partial2[A, B, C](a: A, f: (A, B) => C): B => C = f.curried(a)


  def sum(x: Int, y: Int): Int = x + y


  val p: Int => Int = partial(3, sum)
  p(2) // 5
  p(3) // 5



















}






/**
 *  Реализуем тип Option
 */



 object opt {


  class Animal
  class Dog extends Animal
  class Cat extends Animal

  def treat(animal: Animal): Unit = ???
  def treat(animal: Option[Animal]): Unit = ???

  val d: Dog = ???
  val dOpt: Option[Dog] = ???
  treat(d)
  treat(dOpt)

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // Variance
  // 1. Invariance
  // 2. Covariance
  // 3. Contrvariance

  trait Option[+T]{
    def isEmpty: Boolean = if(this.isInstanceOf[None.type]) true else false

    def get: T = ???

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = ???

    def zip[B](obj: Option[B]): Option[(T, B)] = ???
  }

  object Option{
    def apply[T](v: T): Option[T] = Some(v)
  }

  val o1: Option[Int] = ???

  val o2: Option[Int] = o1.map(_ + 2)

  case class Some[T](v: T) extends Option[T]
  case object None extends Option[Nothing]

  var o: Option[Animal] = None
  var i: Option[Int] = None







  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */
  def printIfAny[T](option: Option[T]): Any = {
    option match {
      case Some(value) => println(value)
      case None =>
    }
  }


  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */

  def zip[A, B](optA: Option[A], optB: Option[B]): Option[(A, B)] = {
    (optA, optB) match {
      case (Some(a), Some(b)) => Some((a, b))
      case _ => None
    }
  }


  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */

  def filter[T](option: Option[T])(predicate: T => Boolean): Option[T] = {
    option match {
      case Some(value) if predicate(value) => Option(value)
      case _ => None
    }
  }

 }

 object list {
   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
    */


   trait List[+T]{
     def ::[TT >: T](elem: TT): List[TT] = ???
   }
   case class ::[T](head: T, tail: List[T]) extends List[T]
   case object Nil extends List[Nothing]

   object List{
     def apply[A](v: A*): List[A] = if(v.isEmpty) Nil
     else new ::(v.head, apply(v.tail:_*))
   }

   val l1: List[Nothing] = List()
   val l2 = List(1, 2, 3)



    /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      * 
      * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */

    implicit class ListOps[T](list: List[T]) {
      def mkString(separator: String = ""): String = {
        @tailrec
        def loop(lst: List[T], sum: String): String = lst match {
          case Nil => sum
          case ::(head, Nil) => sum + head
          case ::(head, tail) => loop(tail, sum + head + separator)
        }

        loop(list, "")
      }

      /**
       *
       * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
       */

      def ++(other: List[T]): List[T] = list match {
        case Nil => other
        case ::(head, tail) => ::(head, tail ++ other)
      }

      def reverse(): List[T] = {
        @tailrec
        def loop(plain: List[T], reversed: List[T]): List[T] = plain match {
          case Nil => reversed
          case ::(head, tail) => loop(tail, ::(head, reversed))
        }

        loop(list, List())
      }


      /**
       *
       * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
       */
      def map[B](f: T => B): List[B] = {
        list match {
          case Nil => Nil
          case ::(head, tail) => ::(f(head), tail.map(f))
        }
      }

      /**
       *
       * Реализовать метод filter для списка который будет фильтровать список по некому условию
       */
      def filter(f: T => Boolean): List[T] = {
        list match {
          case Nil => Nil
          case ::(head, tail) => if (f(head)) ::(head, tail.filter(f)) else tail.filter(f)
        }
      }
    }

   /**
    *
    * Написать функцию incList котрая будет принимать список Int и возвращать список,
    * где каждый элемент будет увеличен на 1
    */
   def incList(list: List[Int]): List[Int] = {
     list match {
       case Nil => Nil
       case ::(head, tail) => ::(head + 1, incList(tail))
     }
   }

   /**
    *
    * Написать функцию shoutString котрая будет принимать список String и возвращать список,
    * где к каждому элементу будет добавлен префикс в виде '!'
    */
   def shoutString(list: List[String]): List[String] = {
     list match {
       case Nil => Nil
       case ::(head, tail) => ::("!" + head, shoutString(tail))
     }
   }
 }