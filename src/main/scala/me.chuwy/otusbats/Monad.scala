package me.chuwy.otusbats
import scala.util.Try

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = 
    flatMap(fa)(a => a)
}

object Monad {

  implicit val optionMonad = new Monad[Option] {
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    def point[A](a: A): Option[A] = Some(a)
    override def flatten[A](fa: Option[Option[A]]): Option[A] = fa.flatten
  }

  implicit val listMonad = new Monad[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    def point[A](a: A): List[A] = a :: Nil
    override def flatten[A](fa: List[List[A]]): List[A] = fa.flatten
  }

  implicit val tryMonad = new Monad[Try]{
    def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)
    def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
    def point[A](a: A): Try[A] = Try(a)
  }
    
  def apply[F[_]](implicit mon: Monad[F]) : Monad[F] = new Monad[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B] = mon.map(fa)(f)
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = mon.flatMap(fa)(f)
    def point[A](a: A): F[A] = mon.point(a)
  }

  def flatMap[F[_],A, B] (fa : F[A]) (f: A => F[B]) (implicit mon : Monad[F]): F[B]  = mon.flatMap(fa)(f)
  def point[F[_],A] (a: A)(implicit mon: Monad[F]): F[A] = mon.point(a)
  def flatten[F[_],A] (a: F[F[A]])(implicit mon: Monad[F]) : F[A] = mon.flatten(a)

  implicit class MonadOps[F[_],A](a: A){
    def point(implicit mon: Monad[F]) : F[A] = mon.point(a)
    def flatMap[B](f: A => F[B]) (implicit mon: Monad[F]) = mon.flatMap(mon.point(a))(f)
  }

  implicit class MonadOpsF[F[_], A] (a : F[F[A]]){
    def flatten(implicit mon: Monad[F]) = mon.flatten(a)
  }

}
