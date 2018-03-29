/*
import cats.{ Applicative, Eval, Monad, Traverse, ~> }
import iota.TListK.:::

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }
import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import iota._
import iota.syntax.all._
import TList.::
import cats.implicits._


object Macros {

  def hello: Unit = macro helloImpl

  def helloImpl(c: blackbox.Context): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"""println("hello!")""")
  }

}

object Test extends App {

  implicit def futureMonad: Monad[Future] = new Monad[Future] {
    override def pure[A](x: A): Future[A] = Future.successful(x)
    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa flatMap f
    override def tailRecM[A, B](a: A)(f: A => Future[Either[A, B]]): Future[B] = ???
  }

  case object Mon
  case object Tue
  case object Wed
  case object Yesterday

  trait Writer[T] {
    def write(t: T): String
  }

  implicit val mondayW: Writer[Mon.type] = _ => "monday"
  implicit val tuesdayW: Writer[Tue.type] = _ => "TUESDAY"
  implicit val wednesdayW: Writer[Wed.type] = _ => "wed"
  implicit val yesterdayW: Writer[Yesterday.type] = _ => "yester"

  implicit val intW: Writer[Int] = _.toString

  implicit val optionW: Delay[Writer, Option] = new Delay[Writer, Option] {
    override def apply[A](fa: Writer[A]): Writer[Option[A]] = (o: Option[A]) => o match {
      case Some(a) => "SUMTING" + fa.write(a)
      case None => "Nuting"
    }
  }

  implicit val listW: Delay[Writer, List] = new Delay[Writer, List] {
    override def apply[A](fa: Writer[A]): Writer[List[A]] = (o: List[A]) =>
      "A Lizt: " + o.map(fa.write).mkString(" :-: ")
  }

  implicit class WriterOps[A](a: A)(implicit w: Writer[A]) {
    def writePls: String = w.write(a)
  }

  trait Eq[T] {
    def isEqual(t: T, other: T): Boolean
  }

  implicit class EqOps[A](a: A)(implicit eq: Eq[A]) {
    def isSame(b: A): Boolean = eq.isEqual(a, b)
  }


  implicit val mondayE: Eq[Mon.type] = (a, b) => a == b
  implicit val tuesdayE: Eq[Tue.type] = (a, b) => a == b
  implicit val wednesdayE: Eq[Wed.type] = (a, b) => a == b
  implicit val yesterdayE: Eq[Yesterday.type] = (a, b) => a == b

  Mon.writePls
  Yesterday.writePls

  type Weekday = Cop[Mon.type :: Tue.type :: Wed.type :: TNil]

  /*type WeekdayWriter = Cop[TList.Op.Map[Writer, Weekday#L]]

  def weekdayWriter[A](implicit w: Writer[A], in: Cop.Inject[Writer[A], WeekdayWriter]): WeekdayWriter = {
    w.inject[WeekdayWriter]
  }

  implicit class AnyWeekDayWriter[A](a: A)(implicit w: Writer[A], in: Cop.Inject[Writer[A], WeekdayWriter]) {
    def writePlx: String = weekdayWriter[A].value.asInstanceOf[Writer[A]].write(a)
  }

  Monday.writePlx
  // Yesterday.writePlx won't work*/

  final case class Coproduct[F[_], G[_], A](run: Either[F[A], G[A]]) {
    def traverse[X[_], B](g: A => X[B])(implicit F: Traverse[F], G: Traverse[G], A: Applicative[X]): X[Coproduct[F, G, B]] =
      run.fold(
        x => A.map(F.traverse(x)(g))(x => Coproduct(Left(x))),
        x => A.map(G.traverse(x)(g))(x => Coproduct(Right(x)))
      )
  }

  type Containers[A] = CopK[List ::: Option ::: TNilK, A]

  implicit def actualContainersTraverse: Traverse[Containers] = new Traverse[Containers] {
    override def traverse[G[_], A, B](fa: Containers[A])(f: A => G[B])(implicit A: Applicative[G]): G[Containers[B]] = {
      fa.index match {
        case 0 => A.map(implicitly[Traverse[List  ]].traverse(fa.value.asInstanceOf[List[A]  ])(f))(x => CopK.unsafeApply(0, x))
        case 1 => A.map(implicitly[Traverse[Option]].traverse(fa.value.asInstanceOf[Option[A]])(f))(x => CopK.unsafeApply(1, x))
      }
    }
    override def foldLeft[A, B](fa: Containers[A], b: B)(f: (B, A) => B): B = ???
    override def foldRight[A, B](fa: Containers[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
  }

  trait Delay[F[_], G[_]] {
    def apply[A](fa: F[A]): F[G[A]]
  }

  implicit def actualContainersWriter: Delay[Writer, Containers[?]] = {
    new Delay[Writer, Containers[?]] {
      override def apply[A](fa: Writer[A]): Writer[Containers[A]] = {
        (t: Containers[A]) => t.index match {
          case 0 => implicitly[Delay[Writer, List]].apply(fa).write(t.value.asInstanceOf[List[A]])
          case 1 => implicitly[Delay[Writer, Option]].apply(fa).write(t.value.asInstanceOf[Option[A]])
        }
      }
    }
  }

  implicit def actualContainersWriter2[A: Writer]: Writer[Containers[A]] = implicitly[Delay[Writer, Containers[?]]].apply(implicitly[Writer[A]])

  implicit def actualWeekdayWriter: Writer[Weekday] = (t: Weekday) => {
    t.index match {
      case 0 => implicitly[Writer[Mon.type]].write(t.value.asInstanceOf[Mon.type])
      case 1 => implicitly[Writer[Tue.type]].write(t.value.asInstanceOf[Tue.type])
      case 2 => implicitly[Writer[Wed.type]].write(t.value.asInstanceOf[Wed.type])
    }
  }

  implicit def actualWeekdayEq: Eq[Weekday] = (a: Weekday, b: Weekday) => {
    if (a.index == b.index) {
      a.index match {
        case 0 => implicitly[Eq[Mon.type]].isEqual(a.value.asInstanceOf[Mon.type], b.value.asInstanceOf[Mon.type])
        case 1 => implicitly[Eq[Tue.type]].isEqual(a.value.asInstanceOf[Tue.type], b.value.asInstanceOf[Tue.type])
        case 2 => implicitly[Eq[Wed.type]].isEqual(a.value.asInstanceOf[Wed.type], b.value.asInstanceOf[Wed.type])
      }
    } else {
      false
    }
  }

  println(Mon.inject[Weekday] isSame Mon.inject[Weekday])
  println(Tue.inject[Weekday] isSame Tue.inject[Weekday])
  println(Tue.inject[Weekday] isSame Wed.inject[Weekday])

  println(Mon.inject[Weekday].writePls)
  println(Tue.inject[Weekday].writePls)
  println(Wed.inject[Weekday].writePls)

  println(Await.result(Option(1).injectK[Containers].traverse(x => Future.successful(x + 1)), Duration.Inf))
  println(Await.result(List(1, 2, 3).injectK[Containers].traverse(x => Future.successful(x + 1)), Duration.Inf))

  println(List(1, 2, 3).injectK[Containers].writePls)
  println(Option(13).injectK[Containers].writePls)


}*/
