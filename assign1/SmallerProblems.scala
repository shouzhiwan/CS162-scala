// These problems are extracted from "Programming Scala", by
// Dean Wampler.

object Problem1 {
    def main( args:Array[String] ) = {
      // Reverse each element of the args array and print out the
      // result, with each element printed on a new line.
      // Note that the String class has a 'reverse' method.
      //
      // For example:
      //
      // scala Problem1 foo bar baz
      // oof
      // rab
      // zab
    for(i <- 0 until args.length){
      println(args(i).reverse)
    }

    }
}

object Problem2 {
  // A binary tree node.  The field `ord` is declared with
  // `var`, so it is mutable.  For example, you can do:
  //
  // val n = Node(...)
  // n.ord = (1 -> 2)
  //
  // Because we introduced the `var`, you may modify _this_ `var`.
  // You may not introduce any other `var`s.
  case class Node(var ord:(Int,Int), 
                  left:Option[Node],
                  right:Option[Node])

  def main( args:Array[String] ) = {
    // example tree
    val tree = Node( (-1,-1), 
      None,
      Some(Node( (-1,-1),
                Some(Node( (-1,-1), None, None )),
                Some(Node( (-1,-1), Some(Node( (-1,-1), None, None )), None ))
      ))
    )
    val tree2 =
    Node( (-1,-1), // A
          Some(Node( (-1,-1), // B
                     Some(Node((-1,-1), // D
                               None,
                               Some(Node( (-1, -1), None, None)))), // G
                      None)),
          Some(Node( (-1, -1), // C
                     Some(Node( (-1, -1), // E
                                Some(Node( (-1, -1), None, None)), // H
                                None)),
                     Some(Node( (-1,-1), None, None))))) // F
    

    // set the tree nodes' labels and print the tree. note that case
    // classes are automatically given a toString method, so we don't
    // need to define our own.  Your solution must be general, in that
    // it can work with arbitrary trees.
    order( tree2 )
    println( tree2 )

    // For example:
    //
    // scala Problem2
    // Node((0,4),None,Some(Node((1,3),Some(Node((2,0),None,None)),Some(Node((3,2),Some(Node((4,1),None,None)),None)))))
  }

  def order( node:Node ) {
    postorder(Some(node),-1)
    preorder(Some(node),-1)
    //postorder(Some(node),-1)
    // use a nested method inside this method as a helper function to
    // traverse the given tree and set each Node's 'ord' field to the
    // tuple '(preorder, postorder)', where 'preorder' is the Node's
    // preorder label and 'postorder' is the Node's postorder
    // label. For consistent numbers, visit left children before right
    // children. Labels should start at 0 (i.e., the root node's
    // preorder label should be 0).


    def postorder(o: Option[Node], x :Int): Int = {
      o match {
        case None => x
        case Some(t) => {
          t.ord = (t.ord._1, postorder(t.right, postorder(t.left, x))+1) 
          t.ord._2
        }
      }
    }

    def preorder(o: Option[Node], x: Int): Int = {
      o match {
        case None => x
        case Some(t) => {
          t.ord = (x + 1, t.ord._2)
          preorder(t.right, preorder(t.left, t.ord._1))
        }  
      }
    }

    //def postorder(node :Option[Node]){

    //}

    /*
    def preorderHelper[Node](o: Option[Node], otherwise: Node): Node = {
      o match {
        case Some(t) => t
        case None => otherwise
      }

    }

    def postorderHelper[Node](o: Option[Node], otherwise: => Node): Node = {
      o match {
        case Some(t) => t
        case Node =>otherwise
      }
    }*/
    // As a hint, you'll need to use recursion here.  The nested
    // method should have an auxilliary parameter, representing the
    // currently available ID.  The nested method should return the
    // next available ID.  This is equivalent to an approach of
    // having a mutable variable elsewhere and incrementing it
    // each time we need a new ID, which is likely a more obvious
    // solution if you're coming from an imperative background.  This
    // is equivalent, because the mutable variable sets up an implicit
    // data dependency between recursive calls, whereas with functional
    // purity we must make this data dependency explicit.
  }
}

object Problem3 {
  def main( args:Array[String] ) = {
    val list = args.toList

    // Use the foldLeft method of list to print the following:
    //
    // 1. the largest element in args (using string comparison)
    // 2. args with no duplicate elements
    // 3. a run-length-encoded version of args

    
  
    println(list.foldLeft(""){(r,c) => if(r == "") c else if(r > c) r else c})

    println(list.foldLeft(List[String]()){(r,c) => if (r.contains(c)) r else c :: r }.reverse)

    println(list.foldLeft(List[(String, Int)]()){ (r,c) => if(r.isEmpty) (c, 1) :: r else if(r.head._1 == c)(c, r.head._2 + 1) :: r.drop(1) else (c, 1) :: r }.reverse)

    /*var s = largestItem(list)
    println(s)

    var l1 = UniqueItem(list)
    print("List(")
    for(i <- 0 until l1.length-1){
      print(l1(i) + ", ")
    }
    println(l1(l1.length-1)+ ")")

    var l2 = CountItem(list)
    print("List(")
    for(i <- 0 until l2.length-1){
      print("(" + l2(i)._1 + "," + l2(i)._2 + "), ")
    }*/
    //println("(" + l2(l2.length-1)._1 + "," + l2(l2.length-1)._2 + "))")
    /*
    var l2 =  CountItem(list)
    for(i <- 0 until l2.length){
      println(l2(i))
    }*/
    // NOTES
    //
    // If the initial value given to foldLeft is an empty List you
    // need to explicitly give the type of the List, e.g., List[Int]()
    // or List[String](), otherwise the compiler won't be able to
    // figure out the types.
    //
    // To determine if a string `s1` is greater than another string `s2`,
    // you can use `>` like so: `s1 > s2`.  The `compareTo` method on
    // `String` can also be used.
    // 
    // You may use reverse as part of your solution.
    //
    // For run-length-encoding specifics, see
    // http://en.wikipedia.org/wiki/Run_length_encoding.

    // For example:
    //
    // scala Problem3 foo bar bar baz moo moo moo cow
    // moo
    // List(foo, bar, baz, moo, cow)
    // List((foo,1), (bar,2), (baz,1), (moo,3), (cow,1))
  }
}
