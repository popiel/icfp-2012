package com.wolfskeep

/**
 * @author T. Alexader Popiel
 */
object Lifter {

  @volatile var entry: String = "A"
  
  def foo(x : Array[String]) = x.foldLeft("")((a,b) => a + b)
  
  def main(args : Array[String]) {
    println( "Hello World!" )
    println("concat arguments = " + foo(args))

    Runtime.getRuntime.addShutdownHook(new Thread { override def run { println(entry) }})
    Thread.sleep(30000)
    println("awake")
  }

}
