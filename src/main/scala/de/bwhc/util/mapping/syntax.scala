package de.bwhc.util.mapping



object syntax
{

  implicit class MappingOps[A](val a: A) extends AnyVal
  {

    def mapTo[B](implicit m: A => B): B = m(a)

  }

}
