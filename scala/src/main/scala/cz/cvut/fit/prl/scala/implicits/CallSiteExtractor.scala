package cz.cvut.fit.prl.scala.implicits


import scala.collection.mutable
import scala.collection.breakOut
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.meta.internal.semanticdb.TreeMessage.SealedValue
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.Classpath
import scala.meta.{inputs, _}
import scala.meta.internal.semanticdb.{IdTree, Synthetic}
import scala.util.{Failure, Success, Try}
import scala.meta.io.Classpath
import scala.meta.internal.symtab._

object CallSiteExtractor {
  implicit def position2Range(that: Position): s.Range =
    s.Range(that.startLine, that.startColumn, that.endLine, that.endColumn)
}

sealed trait ImplicitSynthetic {
  def symbol: String

  def range: s.Range

  def synthetic: s.Synthetic
}

object ImplicitSynthetic {
  def unapply(arg: s.Synthetic): Option[ImplicitSynthetic] = arg match {
    // conversions
    case s@s.Synthetic(_, s.ApplyTree(s.IdTree(symbol), s.OriginalTree(Some(range)) :: _)) =>
      Some(ImplicitConversion(symbol, range, s))
    case s@s.Synthetic(_, s.ApplyTree(s.TypeApplyTree(s.IdTree(symbol), targs), s.OriginalTree(Some(range)) :: _)) =>
      Some(ImplicitConversion(symbol, range, s))

    // implicit parameters
    case s@s.Synthetic(_, s.ApplyTree(s.OriginalTree(Some(range)), s.TypeApplyTree(s.IdTree(symbol), targs) :: _)) =>
      Some(ImplicitParameter(symbol, range, s))
    case s@s.Synthetic(_, s.ApplyTree(s.OriginalTree(Some(range)), s.IdTree(symbol) :: _)) =>
      Some(ImplicitParameter(symbol, range, s))
    case _ => None
  }
}

case class ImplicitConversion(symbol: String, range: s.Range, synthetic: s.Synthetic) extends ImplicitSynthetic {
}

case class ImplicitParameter(symbol: String, range: s.Range, synthetic: s.Synthetic) extends ImplicitSynthetic {
}

sealed trait CallSite {

  import CallSiteExtractor.position2Range

  def fun: s.SymbolInformation

  def tree: Option[Tree]

  val syntactic: Boolean = tree.isDefined
  val range: Option[s.Range] = tree.map(_.pos)
  val code: String = tree.map(_.toString()).getOrElse("<synthetic call>")

  override def toString: String = s"Call(${fun.name}, ${tree.map(_.productPrefix)}($code)"
}

case class NormalCall(fun: s.SymbolInformation, tree: Option[Tree]) extends CallSite {
  override def toString: String = "Normal" + super.toString
}

case class ConversionCall(fun: s.SymbolInformation, tree: Option[Tree], conversion: ImplicitConversion) extends CallSite {
  override def toString: String = "Conversion" + super.toString
}

case class ImplicitParameterCall(fun: s.SymbolInformation, tree: Option[Tree], implicitParameter: ImplicitParameter) extends CallSite {
  override def toString: String = "ImplicitParameter" + super.toString
}

class CallSiteExtractor(val db: s.TextDocument, val symtab: SymbolTable) {

  import CallSiteExtractor._

  case class ResolvedSymbol(occurence: s.SymbolOccurrence) {
    lazy val info = symtab.info(occurence.symbol)
  }

  val tree: Tree = db.text.parse[Source].get
  val synthetics: Seq[Synthetic] = db.synthetics
  val implicits: Map[s.Range, ImplicitSynthetic] = synthetics.collect {
    case ImplicitSynthetic(s) => s.range -> s
  }.toMap
  val symbols: Map[s.Range, ResolvedSymbol] = db.occurrences.collect {
    case s@s.SymbolOccurrence(Some(range), _, _) => range -> ResolvedSymbol(s)
  }.toMap

  private val extraction: Seq[Try[CallSite]] = {
    val seenSelects = mutable.HashSet[Term.Select]()

    def matchImplicit(tree: Tree): Try[Option[ImplicitSynthetic]] = {
//      implicits.get(tree.children.head.pos) match {
//        case Some(x: ImplicitConversion) => Success(Some(x))
//        case Some(x) => Failure(new Exception(s"Matched first children range, but got $x instead of ImplicitConversion for $tree"))
//        case _ => implicits.get(tree.pos) match {
//          case Some(x: ImplicitParameter) => Success(Some(x))
//          case Some(x) => Failure(new Exception(s"Matched callsite range, but got $x instead of ImplicitParameter for $tree"))
//          case _ => Success(None)
//        }
//      }
      Success(None)
    }

    def resolveSymbol(name: Term.Name): Try[s.SymbolInformation] = Try {
      symbols.get(name.pos).map(_.info) match {
        case Some(Some(x)) => x
        case Some(None) => throw new Exception(s"Term $name is not in symtab")
        case None => throw new Exception(s"Term $name is not in symbol occurencies")
      }
    }

    def createCallSite(name: Term.Name, tree: Tree): Try[CallSite] = {
      resolveSymbol(name).flatMap { fun =>
        matchImplicit(tree) collect {
          case Some(x: ImplicitConversion) => ConversionCall(fun, Some(tree), x)
          case Some(x: ImplicitParameter) => ImplicitParameterCall(fun, Some(tree), x)
          case None => NormalCall(fun, Some(tree))
        }
      }
    }

    val explicitCallSites = tree collect {
      case t@Term.Apply(n: Term.Name, _) =>
        createCallSite(n, t)
      case t@Term.Apply(s@Term.Select(qual, n: Term.Name), _) => {
        seenSelects += s
        createCallSite(n, t)
      }
      case t@Term.Select(qual, n: Term.Name) if !seenSelects.contains(t) =>
        createCallSite(n, t)
      case t@Term.ApplyInfix(lhs, n: Term.Name, _, args) =>
        createCallSite(n, t)
      case t@Term.ApplyUnary(n: Term.Name, args) =>
        createCallSite(n, t)
      case t@Term.New(Init(n: Term.Name, _, _)) =>
        createCallSite(n, t)
      case t@Term.NewAnonymous(Template(_, Init(n: Term.Name, _, _) :: _, _, _)) =>
        createCallSite(n, t)
    }

    val processedSynthetics: Seq[s.Synthetic] = explicitCallSites collect {
      case Success(ConversionCall(_, _, x)) => x.synthetic
      case Success(ImplicitParameterCall(_, _, x)) => x.synthetic
    }
    val missedSynthetics = synthetics.toSet -- processedSynthetics
    val syntheticCallSites = missedSynthetics.flatMap(createSyntheticCallSites)

    explicitCallSites ++ syntheticCallSites
  }

  val callSites: Seq[CallSite] = extraction collect {
    case Success(x) => x
  }

  val failues: Seq[Throwable] = extraction collect {
    case Failure(e) => e
  }

  def createSyntheticCallSites(synthetic: s.Synthetic): Seq[Try[CallSite]] = {
    def symbolName(t: s.Tree): Try[String] = t match {
      case s.IdTree(symbol) => Success(symbol)
      case s.SelectTree(_, Some(fn)) => symbolName(fn)
      case s.TypeApplyTree(fn, _) => symbolName(fn)
      case x => Failure(new Exception(s"Unable to resolve $x"))
    }

    def resolveName(t: s.Tree): Try[s.SymbolInformation] = {
      symbolName(t).flatMap { name =>
        symtab.info(name) match {
          case Some(x) => Success(x)
          case None => Failure(new Exception(s"Unable to find $name in symtab"))
        }
      }
    }

    def process(t: s.Tree): Seq[Try[CallSite]] = {
      t match {
        case s.ApplyTree(s.ApplyTree(fn, args), s.TypeApplyTree(s.IdTree(symbol), targs) :: _) =>
          val call = resolveName(fn).map { fun =>
            ImplicitParameterCall(fun, None, ImplicitParameter(symbol, synthetic.range.get, synthetic))
          }
          call +: (process(fn) ++ args.flatMap(process))
        case s.ApplyTree(fn, args) =>
          val call = resolveName(fn).map { fun =>
            NormalCall(fun, None)
          }
          call +: args.flatMap(process)
        case s.TypeApplyTree(fn, _) => process(fn)
        case s.SelectTree(fn, _) => process(fn)
        case s.FunctionTree(_, term) => process(term)
        case s.MacroExpansionTree(expandee, _) => process(expandee) // TODO: check
        case s.IdTree(_) => Seq()
        case s.LiteralTree(_) => Seq()
        case s.OriginalTree(_) => Seq()
        case _ => Seq()
      }
    }

    process(synthetic.tree)
  }

}
