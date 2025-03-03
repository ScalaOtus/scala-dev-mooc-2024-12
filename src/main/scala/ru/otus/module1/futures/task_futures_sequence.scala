package ru.otus.module1.futures

import ru.otus.module1.futures.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    Future.foldLeft(
      futures.map { future => future.map(Left(_)).recover(Right(_)) }
    )(List[A]() -> List[Throwable]()){ (acc, futureRes) =>
      futureRes match {
        case Left(value) => (acc._1 ++ List(value)) -> acc._2
        case Right(exception) => acc._1 -> (List(exception) ++ acc._2)
      }
    }
  }

}
