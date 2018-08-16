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


sealed trait CallSite {
  def fun: s.SymbolInformation

  def implicitArgs: Seq[s.SymbolInformation]

  // TODO: arguments
  // TODO: type arguments
  // TODO: location

  // FIXME: copy syntax does not work
  def update(
              fun: s.SymbolInformation = this.fun,
              implicitArgs: Seq[s.SymbolInformation] = this.implicitArgs
            ): CallSite =
    this match {
      case x: NormalCall => x.copy(fun = fun, implicitArgs = implicitArgs)
      case x: SyntheticCall => x.copy(fun = fun, implicitArgs = implicitArgs)
      case x: ConversionCall => x.copy(fun = fun, implicitArgs = implicitArgs)
    }
}

case class NormalCall(fun: s.SymbolInformation, tree: Tree, implicitArgs: Seq[s.SymbolInformation] = Seq()) extends CallSite {
  val code: String = tree.toString()

  override def toString: String = s"NormalCall(${fun.name}, ${tree.productPrefix}($code)"
}

case class SyntheticCall(fun: s.SymbolInformation, range: Option[s.Range], implicitArgs: Seq[s.SymbolInformation] = Seq()) extends CallSite {
  override def toString: String = s"SyntheticCall(${fun.name}, ${range.map(_.toString).getOrElse("<no range>")})"
}

case class ConversionCall(fun: s.SymbolInformation, implicitArgs: Seq[s.SymbolInformation] = Seq()) extends CallSite {
  override def toString: String = s"ConversionCall(${fun.name})"
}

class CallSiteExtractor(val db: s.TextDocument, val symtab: SymbolTable) {

  import CallSiteExtractor._

  case class ResolvedSymbol(occurence: s.SymbolOccurrence) {
    lazy val info = symtab.info(occurence.symbol)
  }

  val tree: Tree = db.text.parse[Source].get
  val synthetics: Seq[Synthetic] = db.synthetics
  val symbols: Map[s.Range, ResolvedSymbol] = db.occurrences.collect {
    case s@s.SymbolOccurrence(Some(range), _, _) => range -> ResolvedSymbol(s)
  }.toMap

  private val extraction: Seq[Try[CallSite]] = {
    val explicitCallSitesExtraction = extractExplicitCallSites(tree)
    updateWithSynthetics(explicitCallSitesExtraction)
  }

  val callSites: Seq[CallSite] = extraction collect {
    case Success(x) => x
  }

  val failues: Seq[Throwable] = extraction collect {
    case Failure(e) => e
  }

  def resolveSymbol(name: Term.Name): Try[s.SymbolInformation] = Try {
    symbols.get(name.pos).map(_.info) match {
      case Some(Some(x)) => x
      case Some(None) =>
        db.symbols.find(_.name == name.value) match {
          case Some(x) => x
          case None => throw new Exception(s"Term $name is not in symtab nor in local symbols")
        }
      case None => throw new Exception(s"Term $name is not in symbol occurencies")
    }
  }

  def createCallSite(name: Term.Name, tree: Tree): Try[CallSite] = {
    resolveSymbol(name).map(NormalCall(_, tree))
  }

  def extractExplicitCallSites(tree: Tree): Seq[Try[CallSite]] = {
    val seenSelects = mutable.HashSet[Term.Select]()

    // TODO: use recursion instead of collect
    tree collect {
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
  }

  def updateWithSynthetics(triedSites: Seq[Try[CallSite]]): Seq[Try[CallSite]] = {
    val extraction = triedSites.to[mutable.ArrayBuffer]

    def symbolName(t: s.Tree): Try[String] = t match {
      case s.IdTree(symbol) => Success(symbol)
      case s.SelectTree(_, Some(fn)) => symbolName(fn)
      case s.TypeApplyTree(fn, _) => symbolName(fn)
      case x => Failure(new Exception(s"Unable to resolve $x"))
    }

    def resolveSymbol(name: String): Try[s.SymbolInformation] = {
      symtab.info(name).orElse(db.symbols.find(_.symbol == name)) match {
        case Some(x) => Success(x)
        case None => Failure(new Exception(s"Unable to find $name in symtab or in local symbols"))
      }
    }

    def resolveName(t: s.Tree): Try[s.SymbolInformation] = {
      symbolName(t) flatMap resolveSymbol
    }

    def findCallSite(range: s.Range, full: Boolean): Option[(Int, CallSite)] = {
      extraction.indexWhere {
        case Success(NormalCall(_, t, _)) if !full => position2Range(t.children.head.pos) == range
        case Success(NormalCall(_, t, _)) if full => position2Range(t.pos) == range
        case Success(SyntheticCall(_, Some(r), _)) => r == range
        case _ => false
      } match {
        case -1 => None
        case idx => Some((idx, extraction(idx).get))
      }
    }

    def process(t: s.Tree): Unit = {
      t match {
        case s.ApplyTree(s.ApplyTree(fn, args), s.TypeApplyTree(s.IdTree(symbol), targs) :: _) =>
          val call = resolveName(fn).map { fun =>
            // TODO: implicit parameters
            extraction :+ SyntheticCall(fun, None)
          }

          process(fn)
          args.foreach(process)
        case s.ApplyTree(s.OriginalTree(Some(range)), args) =>
          // implicit arguments
          val symbols = args collect { case s.IdTree(symbol) => symbol }
          val resolving = symbols map resolveSymbol
          resolving.find(_.isFailure) match {
            case Some(x) =>
              extraction :+ Failure(new Exception(s"Unable to resolve implicit argument $x"))
            case None => findCallSite(range, true) match {
              case Some((idx, cs)) =>
                extraction.update(idx, Success(cs.update(implicitArgs = resolving map (_.get))))
              case None =>
                extraction :+ Failure(new Exception(s"Unable find callsite for $range to add implicit parameters"))
            }
          }
        case s.ApplyTree(fn, args) =>
          val call = resolveName(fn).map { fun =>
            // TODO: implicit parameters
            extraction :+ SyntheticCall(fun, None)
          }

          args.foreach(process)
        case s.SelectTree(s.OriginalTree(Some(range)), Some(fn)) =>
          // inferred method call (e.g. `.apply` or `.unapplySeq`)
          resolveName(fn).map { fun =>
            findCallSite(range, false) match {
              case Some((idx, cs)) =>
                extraction.update(idx, Success(cs.update(fun = fun)))
              case None =>
                extraction :+ SyntheticCall(fun, Some(range))
            }
          }
          // TODO: add error if resolveName fails
          process(fn)
        case s.TypeApplyTree(fn, targs) =>
          // TODO: type parameters
          process(fn)
        case s.SelectTree(fn, _) => process(fn)
        case s.FunctionTree(_, term) => process(term)
        case s.MacroExpansionTree(expandee, _) => process(expandee) // TODO: check
        case s.IdTree(_) => Seq()
        case s.LiteralTree(_) => Seq()
        case s.OriginalTree(_) => Seq()
        case _ => Seq()
      }
    }

    synthetics.foreach {
      // implicit conversion
      case s@s.Synthetic(_, s.ApplyTree(fun@s.IdTree(_), s.OriginalTree(Some(range)) :: _)) =>
        extraction :+ resolveName(fun).map(ConversionCall(_))
      case s@s.Synthetic(_, s.ApplyTree(fun@s.TypeApplyTree(s.IdTree(_), targs), s.OriginalTree(Some(range)) :: _)) =>
        extraction :+ resolveName(fun).map(ConversionCall(_))

      // other
      case s => process(s.tree)
    }

    extraction
  }
}
