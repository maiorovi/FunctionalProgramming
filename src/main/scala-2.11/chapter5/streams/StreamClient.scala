package chapter5.streams

/**
  * Created by Egor on 22.03.2016.
  */
object StreamClient {

  def main(args:Array[String]):Unit = {
    println("Hello")
    val myStream = Cons(() => 2, () => Cons(() => 3, () => Cons(() => 4, () => Empty)))
    println(myStream.toList)
    println(myStream.take(2).toList)
  }

}
