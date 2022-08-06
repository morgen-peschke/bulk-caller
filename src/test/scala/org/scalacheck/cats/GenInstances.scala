package org.scalacheck.cats

// Copied from https://github.com/davenverse/cats-scalacheck/blob/main/core/shared/src/main/scala/org/scalacheck/cats/instances/CogenInstances.scala
// to avoid having to bother with version incompatibilities (minor tweaks only)

import cats._
import cats.syntax.all._
import org.scalacheck.Gen

object GenInstances extends GenInstances

trait GenInstances extends GenInstances1

trait GenInstances1 extends GenInstances0 {
  implicit val genInstances: Monad[Gen] with Alternative[Gen] with FunctorFilter[Gen] = new Monad[Gen] with Alternative[Gen] with FunctorFilter[Gen] {
    // Members declared in cats.Applicative
    override def pure[A](x: A): Gen[A] =
      Gen.const(x)

    // Members declared in cats.FlatMap
    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Gen[Either[A, B]]): Gen[B] =
      Gen.tailRecM(a)(f)

    override def combineK[A](x: Gen[A], y: Gen[A]): Gen[A] = Gen.gen { (params, seed) =>
      val xGen = x.doApply(params, seed)
      if (xGen.retrieve.isDefined) xGen
      else y.doApply(params, seed)
    }

    override def empty[A]: Gen[A] = Gen.fail

    override def map2Eval[A, B, Z](fa: Gen[A], fb: Eval[Gen[B]])(f: (A, B) => Z): Eval[Gen[Z]] =
      Eval.later(map2(fa, Gen.lzy(fb.value))(f))

    override def product[A, B](fa: Gen[A], fb: Gen[B]): Gen[(A, B)] = Gen.zip(fa, fb)

    override def functor: Functor[Gen] = this

    override def mapFilter[A, B](fa: Gen[A])(f: A => Option[B]): Gen[B] =
      fa.flatMap { a =>
        f(a) match {
          case Some(b) => pure(b)
          case _ => Gen.fail
        }
      }
  }

  implicit def genMonoid[A: Monoid]: Monoid[Gen[A]] =
    Monoid.instance[Gen[A]](
      Gen.const(Monoid[A].empty),
      (x, y) =>
        for {
          xa <- x
          ya <- y
        } yield xa |+| ya
    )
}

trait GenInstances0 {
  implicit def genSemigroup[A: Semigroup]: Semigroup[Gen[A]] =
    Semigroup.instance[Gen[A]] { (x, y) =>
      for {
      xa <- x
      ya <- y
    } yield xa |+| ya
  }
}
