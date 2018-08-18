package cz.cvut.fit.prl.scala.implicits

import scala.meta.internal.semanticdb.SymbolInformation.Property.IMPLICIT
import scala.collection.mutable
import scala.collection.breakOut
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.meta.internal.semanticdb.TreeMessage.SealedValue
import scala.meta.internal.semanticdb.TreeMessage.SealedValue.SelectTree
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.Classpath
import scala.meta.{inputs, _}
import scala.meta.internal.semanticdb.{IdTree, SymbolInformation, Synthetic}
import scala.util.{Failure, Success, Try}
import scala.meta.io.Classpath
import scala.meta.internal.symtab._
import scala.reflect.internal.Types


object CallSiteExtractor {

  implicit class XtensionPosition(that: Position) {
    def toRange: s.Range =
      s.Range(that.startLine, that.startColumn, that.endLine, that.endColumn)
  }

  implicit def position2Range(that: Position): s.Range = that.toRange
}

sealed trait Type

case object Empty extends Type

case class Literal(value: Lit) extends Type

case class TypeRef(prefix: Type, symbol: s.SymbolInformation, args: Seq[Type]) extends Type

case class FunctionLiteral(params: Seq[Type]) extends Type

case class ArgumentList(args: Seq[Type], isImplicit: Boolean, syntactic: Boolean)

sealed trait CallSite {
  def fun: s.SymbolInformation

  def argss: Seq[ArgumentList]

  def typeArgs: Seq[Type]

  // TODO: location

  def implicitArgs: Option[ArgumentList] = argss.last match {
    case x@ArgumentList(_, true, _) => Some(x)
    case _ => None
  }

  // FIXME: copy syntax does not work
  def update(
              fun: s.SymbolInformation = this.fun,
              argss: Seq[ArgumentList] = this.argss,
              typeArgs: Seq[Type] = this.typeArgs
            ): CallSite =
    this match {
      case x: NormalCall => x.copy(fun = fun, argss = argss, typeArgs = typeArgs)
      case x: SyntheticCall => x.copy(fun = fun, argss = argss, typeArgs = typeArgs)
      case x: ConversionCall => x.copy(fun = fun, argss = argss, typeArgs = typeArgs)
    }
}


case class NormalCall(
                       fun: s.SymbolInformation,
                       tree: Tree,
                       argss: Seq[ArgumentList] = Seq(),
                       typeArgs: Seq[Type] = Seq()
                     ) extends CallSite {
  val code: String = tree.toString()

  override def toString: String = s"NormalCall(${fun.name}, ${tree.productPrefix}($code)"
}

case class SyntheticCall(
                          fun: s.SymbolInformation,
                          range: Option[s.Range],
                          argss: Seq[ArgumentList] = Seq(),
                          typeArgs: Seq[Type] = Seq()
                        ) extends CallSite {
  override def toString: String = s"SyntheticCall(${fun.name}, ${range.map(_.toString).getOrElse("<no range>")})"
}

case class ConversionCall(
                           fun: s.SymbolInformation,
                           argss: Seq[ArgumentList] = Seq(),
                           typeArgs: Seq[Type] = Seq()
                         ) extends CallSite {
  override def toString: String = s"ConversionCall(${fun.name})"
}

class CallSiteExtractor(val db: s.TextDocument, val symtab: SymbolTable) {

  implicit class XtensionOptions[+A](that: Option[A]) {
    def getOrThrow(e: Throwable): A = that match {
      case Some(x) => x
      case None => throw e
    }
  }

  import CallSiteExtractor._

  case class ResolvedSymbol(occurence: s.SymbolOccurrence) {
    lazy val info: Option[SymbolInformation] =
      symtab.info(occurence.symbol).orElse(db.symbols.find(_.symbol == occurence.symbol))
  }

  val tree: Tree = db.text.parse[Source].get
  val synthetics: Seq[Synthetic] = db.synthetics
  val symbols: Map[s.Range, ResolvedSymbol] = db.occurrences.collect {
    case s@s.SymbolOccurrence(Some(range), _, _) => range -> ResolvedSymbol(s)
  }.toMap

  private val extraction: (Seq[CallSite], Seq[Throwable]) = {
    def split[A](xs: Seq[Try[A]]) =
      xs.foldLeft((List[A](), List[Throwable]())) { case ((succ, fail), t) =>
        t match {
          case Success(x) => (x :: succ, fail)
          case Failure(x) => (succ, x :: fail)
        }
      }

    val (callSites, errors) = split(extractExplicitCallSites(tree))
    val (updatedCallSites, syntheticErrors) = split(updateWithSynthetics(callSites))

    (updatedCallSites, errors ++ syntheticErrors)
  }

  val callSites: Seq[CallSite] = extraction._1

  val failures: Seq[Throwable] = extraction._2

  def resolveSymbol(range: s.Range): s.SymbolInformation =
    symbols(range).info.getOrThrow(new Exception(s"Symbol at range $range is not in semanticdb occurencies"))

  def resolveSymbol(name: String): s.SymbolInformation =
    db.symbols.find(_.symbol == name)
      .orElse(symtab.info(name))
      .getOrThrow(new Exception(s"Unable to find $name in symtab or in local symbols"))

  def findNameTerm(t: Term): Term.Name = t match {
    case Term.Select(_, name) => name
    case Term.ApplyType(fun, _) => findNameTerm(fun)
    case Term.Apply(fun, _) => findNameTerm(fun)
    case x: Term.Name => x
    case _ => throw new Exception(s"${t.structure} is not supported name term")
  }

  def findTypeArgs(term: Tree): Seq[Type] = term match {
    case Term.ApplyType(_, targs) =>
      targs map resolveType
    case Term.Apply(fun, _) =>
      findTypeArgs(fun)
    case Term.New(Init(value, _, _)) =>
      findTypeArgs(value)
    case Type.Apply(_, targs) =>
      targs map resolveType
    case _ =>
      Seq()
  }

  def resolveType(t: Tree): Type = t match {
    case Term.New(Init(tpe, _, _)) =>
      resolveType(tpe)
    case t@Lit(_) =>
      Literal(t)
    case t@scala.meta.Type.Name(_) =>
      TypeRef(Empty, resolveSymbol(t.pos), Seq())
    case x =>
      throw new Exception(s"${x.structure} is not supported scala.meta.Type for type resolution")
  }

  def hasImplicitParameterList(paramsLists: Seq[s.Scope]): Boolean =
    (resolveSymbol(paramsLists.last.symlinks.head).properties & IMPLICIT.value) > 0

  def extractExplicitCallSites(tree: Tree): Seq[Try[CallSite]] = {
    val seenTerms = mutable.HashSet[Tree]()

    def createCallSite(fun: Tree, tree: Tree, argss: Seq[Seq[Term]]): CallSite = {
      val symbol = fun match {
        case x: Term => {
          val term = findNameTerm(x)
          seenTerms += term.parent.get
          resolveSymbol(term.pos)
        }
        case Name(_) =>
          resolveSymbol(fun.pos)
        case Type.Name(_) =>
          resolveSymbol(fun.pos)
        case _ => throw new Exception(s"${fun.structure} is not supported to extract callsite symbol")
      }

      val typeArgs = findTypeArgs(tree)

      val nestedApplies = fun collect {
        case t@Term.Apply(_, _) => t
      }
      seenTerms ++= nestedApplies

      val allArgss = nestedApplies match {
        case Nil => argss
        case _ => argss ++ (nestedApplies collect { case Term.Apply(_, xs) => xs })
      }

      // we need to check if by any chance the last argument list is implicit
      val takesImplicitParams =
        allArgss.nonEmpty && {
          symbol.signature match {
            case s.MethodSignature(_, paramsLists, _) =>
              hasImplicitParameterList(paramsLists)
            case _: s.ClassSignature =>
              // this must be synthetic call to apply/unapply/unapplySeq which will be resolved in the next phase
              // at this point we do not know which overloaded apply/unapply/unapplySeq was it
              false
            case _ =>
              throw new Exception(s"${symbol.signature} not supported to extract parameter lists")
          }
        }

      val argumentsLists = {
        val tmp = allArgss.map(xs => ArgumentList(xs.map(resolveType), isImplicit = false, syntactic = true))
        if (takesImplicitParams) {
          tmp.take(tmp.length - 1) :+ tmp.last.copy(isImplicit = takesImplicitParams)
        } else {
          tmp
        }
      }

      NormalCall(symbol, tree, typeArgs = typeArgs, argss = argumentsLists)
    }

    tree collect {
      case t@Term.Apply(fun, args) if !seenTerms.contains(t) =>
        Try(createCallSite(fun, t, Seq(args)))
      case t@Term.Select(_, n: Term.Name) if !seenTerms.contains(t) =>
        Try(createCallSite(n, t, Seq()))
      case t@Term.ApplyInfix(_, n: Term.Name, _, args) =>
        Try(createCallSite(n, t, Seq(args)))
      case t@Term.ApplyUnary(n: Term.Name, arg) =>
        Try(createCallSite(n, t, Seq(Seq(arg))))
      case t@Term.New(Init(_, n, argss)) =>
        Try(createCallSite(n, t, argss))
      case t@Term.NewAnonymous(Template(_, Init(n, _, argss) :: _, _, _)) =>
        Try(createCallSite(n, t, argss))
    }
  }

  @scala.annotation.tailrec
  final def findOriginalTreeRange(t: s.Tree): s.Range = t match {
    case s.SelectTree(qual, _) => findOriginalTreeRange(qual)
    case s.OriginalTree(Some(range)) => range
    case s.ApplyTree(fn, _) => findOriginalTreeRange(fn)
    case s.TypeApplyTree(fn, _) => findOriginalTreeRange(fn)
    case s.FunctionTree(_, fn) => findOriginalTreeRange(fn)
    case s.MacroExpansionTree(fn, _) => findOriginalTreeRange(fn)
    case _ => throw new Exception(s"No range associated with $t")
  }

  final def resolveTypeFromTree(x: s.Tree): Type = x match {
    case s.IdTree(symbol) =>
      TypeRef(Empty, resolveSymbol(symbol), Seq())
    case s.FunctionTree(params, _) =>
      FunctionLiteral(params map resolveTypeFromTree)
    case s.TypeApplyTree(fn, targs) =>
      resolveTypeFromTree(fn) match {
        case t@TypeRef(_, _, _) =>
          t.copy(args=targs map resolveType)
        case t =>
          throw new Exception(s"$t semanticdb Tree does not take type parameters while it should $x")
      }
    case _ =>
      throw new Exception(s"$x semanticdb Tree type is not yet supported")
  }

  final def resolveType(x: s.Type): Type = x match {
    case s.TypeRef(prefix, symbol, targs) =>
      TypeRef(resolveType(prefix), resolveSymbol(symbol), targs map resolveType)
    case s.Type.Empty =>
      Empty
    case _ =>
      throw new Exception(s"$x semanticdb Type is not yet supported")
  }

  @scala.annotation.tailrec
  final def symbolName(t: s.Tree): String = t match {
    case s.IdTree(symbol) => symbol
    case s.SelectTree(_, Some(fn)) => symbolName(fn)
    case s.TypeApplyTree(fn, _) => symbolName(fn)
    case x => throw new Exception(s"Unable to resolve $x")
  }

  def resolveName(t: s.Tree): s.SymbolInformation = {
    resolveSymbol(symbolName(t))
  }

  def updateWithSynthetics(explicitCallsites: Seq[CallSite]): Seq[Try[CallSite]] = {
    val callSites = explicitCallsites.to[mutable.ArrayBuffer]
    val errors = mutable.ListBuffer[Exception]()

    def findCallSite(range: s.Range, full: Boolean): Option[(Int, CallSite)] = {
      callSites.lastIndexWhere {
        case SyntheticCall(_, Some(r), _, _) =>
          r == range
        case NormalCall(_, t, _, _) if !full =>
          position2Range(t.children.head.pos) == range
        case NormalCall(_, t, _, _) if full =>
          position2Range(t.pos) == range
        case _ =>
          false
      } match {
        case -1 => None
        case idx => Some(idx -> callSites(idx))
      }
    }

    def process(t: s.Tree): Unit = {
      try {
        t match {
          // implicit conversion
          case s.ApplyTree(fn, Seq(s.OriginalTree(Some(range)))) => {
            val resolvedFun = resolveName(fn)
            callSites += ConversionCall(resolvedFun)
            process(fn)
          }

          // for look desuggaring (e.g. `for(i <- Seq(1,2)) if (i & 1) == 0`)
          //        case s.ApplyTree(nested@s.ApplyTree(_, _), implicitArgs) =>
          //          // first process the nested call which might create synthetic calls
          //          process(nested)
          //          // add implicit arguments
          //          // continue
          //          implicitArgs.foreach(process)

          // implicit arguments (e.g. `(Seq(1) ++ Seq(2))(canBuildFrom)`)
          case s.ApplyTree(fn, implicitArgs) =>
            // first process the nested call which might create synthetic calls
            process(fn)

            val range = findOriginalTreeRange(fn)
            val resolvedArgs = implicitArgs map resolveTypeFromTree
            val (idx, cs) =
              findCallSite(range, true)
                .getOrThrow(new Exception(s"Unable to find callsite for $range to add implicit parameters"))

            callSites.update(
              idx,
              cs.update(argss = cs.argss :+ ArgumentList(resolvedArgs, isImplicit = true, syntactic = false))
            )

            //implicitArgs.foreach(process)

          // inferred type arguments (e.g. `Seq(1)` -> `Seq[Int](1)`)
          case s.TypeApplyTree(fn, targs) =>
            process(fn)

            val range = findOriginalTreeRange(fn)
            val typeArgs = targs map resolveType
            val (idx, cs) =
              findCallSite(range, false)
                .getOrThrow(new Exception(s"Unable to find callsite for $range to add implicit parameters"))

            callSites.update(idx, cs.update(typeArgs = typeArgs))


          // inferred method calls
          case s.SelectTree(qual, Some(fn)) =>
            // original: case s.SelectTree(s.OriginalTree(Some(range)), Some(fn)) =>
            val range = findOriginalTreeRange(qual)
            val resolvedFun = resolveName(fn)

            findCallSite(range, false) match {
              // (e.g. `.apply` or `.unapplySeq`)
              case Some((idx, cs)) => {

                val argss = resolvedFun.signature match {
                  case s.MethodSignature(_, paramsLists@Seq(x, xs@_*), _) if hasImplicitParameterList(paramsLists) =>
                    cs.argss.take(cs.argss.length - 1) :+ cs.argss.last.copy(isImplicit = true)
                  case s.MethodSignature(_, _, _) =>
                    cs.argss
                  case x =>
                    throw new Exception(s"Unexpected symbol $x (expecting MethodSignature)")
                }

                callSites.update(idx, cs.update(fun = resolvedFun, argss = argss))
              }

              // a new synthetic call (e.g. `map`, `flatMap`, `withFilter`)
              case None => {
                callSites += SyntheticCall(resolvedFun, Some(range))
              }
            }

            // TODO: should no tbe here
            //process(fn)

          case s.FunctionTree(_, term) =>
            process(term)
          case s.MacroExpansionTree(expandee, _) =>
            process(expandee)
          case s.IdTree(_) =>
          case s.LiteralTree(_) =>
          case s.OriginalTree(_) =>
          case x =>
            throw new Exception(s"Unexpected synthetic tree $x")
        }
      } catch {
        case e: Exception => errors += e
      }
    }

    synthetics map (_.tree) foreach process

    val good = callSites.map(Success(_)).toSeq
    val bad = errors.map(Failure(_)).toSeq

    good ++ bad
  }
}
