
object Decorators {
    
  object profile { 
    
    private var cm: Map[String, Int] =  Map().withDefault(_ => 0)

    def count(name: String) = 
      cm(name)
   
    def reset(name: String) = 
      cm += name -> 0

    def apply[A, B](name: String)(f: A => B) = new Function1[A, B] {
      def apply(x: A) = {
        val n = cm(name)
        cm += name -> (1 + n)
        f(x)
      }
    }
  }

  /////////////////////////////////// 

 
 object trace {
    var count = 0
      // You may add more fields here
    def apply[A, B](name: String)(f: A => B) : Function1[A, B] = new Function1[A, B] {
      // You may add more fields here

      def apply (x: A): B = 
      {
        val depth = count
        for (i <- 1 to depth) 
          print("| ")
        
        print(",- " + name)
        println("(" + x.toString + ")")

        count += 1

        try 
	{
          val output = f(x)
          for (i <- 1 to depth) 
            print("| ")
          
          println("`- " + output)
	  count -= 1
	  output
        }
        catch 
	{
          case e : Exception => 
            count -= 1
            throw e;
        }
      }
    }
  } 
  
  
  object memo {
      // You may add more fields here
    def apply[A, B](f: A => B) : Function1[A, B] = new Function1[A, B] {
      // You may add more fields here
      var cache = Map[A, Either[B,Exception]]()

      def apply (x: A): B =
      {
        if (cache contains x) 
          cache(x) match 
	  {
            case Left(a) => a
            case Right(b) => throw b
          }
	else 
	{
          val output = 
	    try 
              Left(f(x))
	    catch 
	      {case e : Exception => Right(e)}
          
          cache = cache + (x -> output)
          output match 
	  {
            case Left(a) => a
            case Right(b) => throw b
          }
        }
      }
    }
  }

}
