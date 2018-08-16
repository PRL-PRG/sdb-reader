package cz.cvut.fit.prl.scala.implicits

import org.scalatest.Matchers

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

  implicit class XtensionCheck[T](x: Option[T]) {
    def check(tester: T => Unit): Unit = x match {
      case Some(y) => tester(y)
      case None => throw new Exception("No such element")
    }
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
      |  1 -> 2
      |}
    """.stripMargin) { extractor =>
    extractor.failues shouldBe empty

    println(extractor.db.synthetics.map(new LegacySyntheticPrinter().toLegacy(_)).mkString("\n\n"))
    val css = extractor.callSites
    css should have size 1

    val cs = css.head
    cs shouldBe a[ConversionCall]
    cs.fun shouldBe "->"
  }

//  extraction(
//    """
//      |object X {
//      |  "hi".stripMargin
//      |}
//    """.stripMargin) { extractor =>
//    extractor.failues shouldBe empty
//
//    val css = extractor.callSites
//    css should have size (1)
//
//    val cs = css.head
//    cs shouldBe a[ConversionCall]
//    cs.fun shouldBe "stripMargin"
//  }
//
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
//    css should have size (3)
//
//    val cs = css.head
//    cs shouldBe a[ImplicitParameterCall]
//    cs.fun shouldBe "map"
//  }
//
//  extraction(
//    """
//      |object X {
//      |  for {
//      |    i <- Seq(1,2)
//      |  } yield i
//      |}
//    """.stripMargin) { extractor =>
//    extractor.failues shouldBe empty
//
//    val css = extractor.callSites
//    println(css)
//
//    css should have size 2
//
//    css.find(_.fun.endsWith("map().")) check { x =>
//      x shouldBe a[ImplicitParameterCall]
//    }
//    css.find(_.fun == "Seq") check { x =>
//      x shouldBe a[NormalCall]
//    }
//  }
//
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
