package yoox.course

object ReaderSample extends App {
  
  {
    import Kleisli._
    
    val calculate: Reader[Int, Int] = for {
         a <- Reader { (_: Int) * 2 }
         b <- Reader { (_: Int) + 7 }
       } yield {
         a + b
       }
    
    println(calculate.run(4))
    
  }
  
  {
    
    val calculate: ReaderMonad[Int, Int] = for {
         a <- ReaderMonad { (_: Int) * 2 }
         b <- ReaderMonad { (_: Int) + 7 }
       } yield {
         a + b
       }
    println(calculate.run(4))
    
  }
  
  
}