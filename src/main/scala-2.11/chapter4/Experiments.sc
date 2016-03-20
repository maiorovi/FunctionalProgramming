def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f



lift(math.abs)

//lift(String)

"112".toInt