package cz.cvut.fit.prl.scala.implicits

import org.scalatest.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.collection.mutable
import scala.collection.breakOut
import scala.language.implicitConversions
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.semanticdb.SemanticdbSuite
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.Classpath
import scala.meta.{inputs, _}
import scala.meta.internal.semanticdb.{IdTree, Synthetic}

class CallSiteExtractorSuite extends SemanticdbSuite with Matchers {

  implicit class XtensionOption[T](x: Option[T]) {
    def check(tester: T => Unit): Unit = x match {
      case Some(y) => tester(y)
      case None => throw new Exception("No such element")
    }
  }

  implicit class XtensionAny[T](x: T) {
    def check(tester: T => Unit): Unit = tester(x)
  }

  // implicit conversion - method call
  //  database(
  //    """
  //      | object X {
  //      |   implicit class A(x: Int) {
  //      |     def f(): String = "A"
  //      |   }
  //      |
  //      |   1.f()
  //      | }
  //    """.stripMargin) { db =>
  //    print(db.synthetics)
  //  }


  // TC
  //  database(
  //    """
  //      |object X {
  //      |  trait Jsonable[-T] {
  //      |    def toJson(x: T): String
  //      |  }
  //      |
  //      |  implicit val int2jsonable: Jsonable[Int] =  { x: Int => x.toString }
  //      |
  //      |  implicit def traversable2jsonable[T: Jsonable] = new Jsonable[Traversable[T]] {
  //      |    override def toJson(x: Traversable[T]): String = {
  //      |      val tc = implicitly[Jsonable[T]]
  //      |      x.map(tc.toJson).mkString("[", ",", "]")
  //      |    }
  //      |  }
  //      |
  //      |  implicit class XtensionJson[T: Jsonable](x: T) {
  //      |    def toJson: String = implicitly[Jsonable[T]].toJson(x)
  //      |  }
  //      |
  //      |  //def json[T](x: T)(implicit e: Jsonable[T]): String = {
  //      |  //  e.toJson(x)
  //      |  //}
  //      |
  //      |  Seq(1,2,3).toJson
  //      |  //json(Seq(1,2,3))
  //      |}
  //    """.stripMargin
  //  ) { db =>
  //  }
  //

  //  database(
  //    """
  //      |object X {
  //      |  Seq(1) ++ Seq(2)
  //      |}
  //    """.stripMargin) { db =>
  //    val cs = CallSite.extractCallSites(db.text.parse[Source].get)
  //    println(cs.mkString("\n\n"))
  //    println(db.synthetics.map(x => x -> new LegacySyntheticPrinter().toLegacy(x)).mkString("\n\n"))
  //    println()
  //  }

  //    println(cs.mkString("\n\n"))
  //    println(synthetics.map(x => x -> new LegacySyntheticPrinter().toLegacy(x)).mkString("\n\n"))
  //    println()

  def extraction(code: String)(fn: CallSiteExtractor => Unit): Unit = {
    synthetics(code)((db, tree) => fn(new CallSiteExtractor(db, symtab)))
  }

  extraction(
    """
      |object X {
      |  def f[T](x: T) = x
      |  f(1)
      |}
    """.stripMargin) { extractor =>
    extractor.failures shouldBe empty

    val css = extractor.callSites
    css should have size 1

    css.find(_.fun.name == "f") check { x =>
      x shouldBe a[NormalCall]
      x.typeArgs should have size 1
    }
  }


  extraction(
    """
      |object X {
      |  Seq(1)
      |}
    """.stripMargin) { extractor =>
    extractor.failures shouldBe empty

    val css = extractor.callSites
    css should have size 1

    css.find(_.fun.name == "apply") check { x =>
      x shouldBe a[NormalCall]
      x.typeArgs should have size 1
    }
  }

//  extraction(
//    """
//      |object X {
//      |  object A {
//      |    def apply(x: Int) = x
//      |  }
//      |  A(1)
//      |}
//    """.stripMargin) { extractor =>
//
//    val css = extractor.callSites
//    css should have size 1
//
//    css.find(_.fun.name == "apply") check { x =>
//      x shouldBe a[NormalCall]
//    }
//  }

  extraction(
    """
      |object X {
      |  object A {
      |    def apply(x: String)(implicit y: Int) = x
      |  }
      |  implicit val iy = 1
      |  A("A")
      |}
    """.stripMargin) { extractor =>
    extractor.failures shouldBe empty

    val css = extractor.callSites
    css should have size 1

    css.find(_.fun.name == "apply") check { x =>
      x shouldBe a[NormalCall]
      x.implicitArgs.check { list =>
        list.isImplicit shouldBe true
        list.syntactic shouldBe false
        list.args should have size 1
      }
    }
  }

  extraction(
    """
      |object X {
      |  object A {
      |    def apply[T](x: T)(implicit y: T) = x
      |  }
      |  implicit val iy = 1
      |  A(1)
      |}
    """.stripMargin) { extractor =>
    extractor.failures shouldBe empty

    val css = extractor.callSites
    css should have size 1

    css.find(_.fun.name == "apply") check { x =>
      x shouldBe a[NormalCall]
      x.implicitArgs.check { list =>
        list.isImplicit shouldBe true
        list.syntactic shouldBe false
        list.args should have size 1
      }
      x.typeArgs should have size 1
    }
  }

  extraction(
    """
      |object X1 {
      |  object A {
      |    def apply[T](x: T)(implicit y: T) = x
      |  }
      |  implicit val iy = 1
      |  A.apply[Int](1)
      |}
    """.stripMargin) { extractor =>
    extractor.failures shouldBe empty

    val css = extractor.callSites
    css should have size 1

    css.find(_.fun.name == "apply") check { x =>
      x shouldBe a[NormalCall]
      x.implicitArgs.check { list =>
        list.isImplicit shouldBe true
        list.syntactic shouldBe false
        list.args should have size 1
      }
      x.typeArgs should have size 1
    }
  }

  extraction(
    """
      |object X2 {
      |  object A {
      |    def apply[T](x: T)(implicit y: T) = x
      |  }
      |  implicit val iy = 1
      |  A.apply[Int](1)(2)
      |}
    """.stripMargin) { extractor =>
    extractor.failures shouldBe empty

    val css = extractor.callSites
    css should have size 1

    css.find(_.fun.name == "apply") check { x =>
      x shouldBe a[NormalCall]
      x.implicitArgs.check { list =>
        list.isImplicit shouldBe true
        list.syntactic shouldBe true
        list.args should have size 1
      }
      x.typeArgs should have size 1
    }
  }

  extraction(
    """
      |object X2 {
      |  class A(x: Int)(y: String)(implicit z: Boolean)
      |  implicit val iz = true
      |  new A(1)("A")
      |}
    """.stripMargin) { extractor =>
    extractor.failures shouldBe empty

    val css = extractor.callSites
    css should have size 1

    css.find(_.fun.name == "<init>") check { x =>
      x shouldBe a[NormalCall]
      x.implicitArgs.check { list =>
        list.isImplicit shouldBe true
        list.syntactic shouldBe false
        list.args should have size 1
      }
      x.typeArgs should have size 0
    }
  }

  extraction(
    """
      |object ClassConstructorImplicitTypeArgs {
      |  class A[T](x: T)(y: String)(implicit z: Boolean)
      |  implicit val iz = true
      |  new A(1)("A")
      |}
    """.stripMargin) { extractor =>
    extractor.failures shouldBe empty

    val css = extractor.callSites
    css should have size 1

    css.find(_.fun.name == "<init>") check { x =>
      x shouldBe a[NormalCall]
      x.implicitArgs.check { list =>
        list.isImplicit shouldBe true
        list.syntactic shouldBe false
        list.args should have size 1
      }
      // there are no type arguments, only parameters
      x.typeArgs should have size 0
    }
  }

  //  class X
  //  class X2 extends X

  extraction(
    """
      |object ClassConstructorExplicitImplicitTypeArgs {
      |  class A[T](x: T)(y: String)(implicit z: Boolean)
      |  implicit val iz = true
      |  new A[Any](1)("A")(false)
      |}
    """.stripMargin) { extractor =>
    extractor.failures shouldBe empty

    val css = extractor.callSites
    css should have size 1

    css.find(_.fun.name == "<init>") check { x =>
      x shouldBe a[NormalCall]
      x.implicitArgs.check { list =>
        list.isImplicit shouldBe true
        list.syntactic shouldBe true
        list.args should have size 1
      }
      x.typeArgs should have size 1
    }
  }

  // TODO: new A[]
  // TODO: new A[](impl)
  // TODO: new A[] {}
  // TODO: new A[](impl) {}
  // TODO: a.+[A](impl)
  // TODO: -[A]x(impl)
  // TODO: multiple parameter lists

//
//  extraction(
//    """
//      |object X {
//      |  val List(a,b) = Seq(1,2)
//      |}
//    """.stripMargin) { extractor =>
//
//    val css = extractor.callSites
//    css should have size 3
//
//    // TOD0: there should be two apply
//    css.find(_.fun.name == "apply") check { x =>
//      x shouldBe a[NormalCall]
//    }
//    css.find(_.fun.name == "unapplySeq") check { x =>
//      x shouldBe a[SyntheticCall]
//    }
//  }
//
//  extraction(
//    """
//      |object X {
//      |  1 -> 2
//      |}
//    """.stripMargin) { extractor =>
//    extractor.failues shouldBe empty
//
//    val css = extractor.callSites
//    css should have size 2
//
//    css.find(_.fun.name == "ArrowAssoc") check { x =>
//      x shouldBe a[ConversionCall]
//    }
//    css.find(_.fun.name == "->") check { x =>
//      x shouldBe a[NormalCall]
//    }
//  }
//
//  extraction(
//    """
//      |object X {
//      |  "hi".stripMargin
//      |}
//    """.stripMargin) { extractor =>
//    extractor.failues shouldBe empty
//
//    val css = extractor.callSites
//    css should have size 2
//
//    css.find(_.fun.name == "stripMargin") check { x =>
//      x shouldBe a[NormalCall]
//    }
//    css.find(_.fun.name == "augmentString") check { x =>
//      x shouldBe a[ConversionCall]
//    }
//  }

//  extraction(
//    """
//      |object X {
//      |  List(1).map(_ + 2)
//      |}
//    """.stripMargin) { extractor =>
//    extractor.failues shouldBe empty
//
//    val css = extractor.callSites
//    println(css)
//
//    css should have size 3
//
//  }

  extraction(
    """
      |object X {
      |  for {
      |    i <- Seq(1,2)
      |  } yield i + 1
      |}
    """.stripMargin) { extractor =>
    extractor.failures shouldBe empty

    val css = extractor.callSites
    println(css)

    css should have size 3

//    Seq(1,2) map (i=> i + 1)

//    css.find(_.fun.endsWith("map().")) check { x =>
//      x shouldBe a[ImplicitParameterCall]
//    }
//    css.find(_.fun == "Seq") check { x =>
//      x shouldBe a[NormalCall]
//    }
  }

//  extraction(
//    """
//      |object X {
//      |  for {
//      |    i <- 1 to 10
//      |    j <- 0 until 10
//      |    if i % 2 == 0
//      |  } yield (i, j)
//      |}
//    """.stripMargin) { extractor =>
//    extractor.failues shouldBe empty
//
//    val css = extractor.callSites
//    println(css)
//
//    css should have size 7
//
//    css.find(_.fun.endsWith("map().")).check { x =>
//      x shouldBe a[ImplicitParameterCall]
//    }
//    css.find(_.fun.endsWith("flatMap().")).check { x =>
//      x shouldBe a[ImplicitParameterCall]
//    }
//    css.find(_.fun.endsWith("withFilter().")).check { x =>
//      x shouldBe a[NormalCall]
//    }
//  }
//
//  extraction(
//    """
//      |object X {
//      |  List(1)
//      |}
//    """.stripMargin) { extractor =>
//    extractor.failues shouldBe empty
//
//    val css = extractor.callSites
//    css should have size (1)
//
//    val cs = css.head
//    cs shouldBe a[NormalCall]
//    cs.fun shouldBe "List"
//  }
//
//  extraction(
//    """
//      |object X {
//      |  val List(a,b) = Seq(1,2)
//      |}
//    """.stripMargin) { extractor =>
//    extractor.failues shouldBe empty
//
//    val css = extractor.callSites
//    css should have size (1)
//
//    val cs = css.head
//    cs shouldBe a[NormalCall]
//    cs.fun shouldBe "Seq"
//  }

  // "fooo".stripPrefix("o")
  // 1 #:: 2 #:: Stream.empty
  // Array.empty[Int]
  // for {
  //  i <- 1 to 10
  //  j <- 0 until 10
  //  if i % 2 == 0
  //} yield (i, j)
  // for {
  //   a <- Future.successful(1)
  //   b <- Future.successful(2)
  // } println(a)

  //  database(
  //    """
  //      | object X {
  //      |   val a = 1
  //      |   s"$a + 1"
  //      |   s"${a + 1}"
  //      |   s"${a.hashCode}"
  //      | }
  //    """.stripMargin) { db =>
  //
  //    val tree = db.text.parse[Source].get
  //    println(CallSite.extractCallSites(tree).mkString("\n\n"))
  //  }

  //  database(
  //    """
  //      | object X {
  //      |   val x = new StringBuffer
  //      |   val y = new StringBuffer()
  //      |   val z = new Runnable() {
  //      |   }
  //      | }
  //    """.stripMargin) { db =>
  //
  //    val tree = db.text.parse[Source].get
  //    println(CallSite.extractCallSites(tree).mkString("\n\n"))
  //  }

  //  database(
  //    """
  //      |object X {
  //      |  //Seq(1).map(_+1).mkString(",")
  //      |  ("A"+"B").getBytes().map(_+1).mkString(",").length.hashCode
  //      |  Seq(1).size.toString()
  //      |}
  //    """.stripMargin
  //  ) { db =>
  //
  //    val tree = db.text.parse[Source].get
  //    println(CallSite.extractCallSites(tree).mkString("\n\n"))
  //    //println(db.synthetics.map(x => x -> new LegacySyntheticPrinter().toLegacy(x)).mkString("\n\n"))
  //  }

  //  database(
  //    """
  //      |object X {
  //      |  Seq(1,2,3)
  //      |}
  //    """.stripMargin
  //  ) { db =>
  //    print(db.synthetics)
  //  }

}
