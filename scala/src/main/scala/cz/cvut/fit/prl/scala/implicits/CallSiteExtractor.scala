package cz.cvut.fit.prl.scala.implicits

import scala.meta.internal.semanticdb.SymbolInformation.Property.IMPLICIT
import scala.meta.internal.semanticdb.SymbolInformation.Kind
import scala.collection.mutable
import scala.language.implicitConversions
import scala.meta.internal.{semanticdb => s}
import scala.meta._
import scala.meta.internal.semanticdb.{SymbolInformation, TextDocument}
import scala.util.{Failure, Success, Try}
import scala.meta.internal.symtab._
import cz.cvut.fit.prl.scala.implicits.utils._

trait SymbolResolver {
  def resolveSymbol(range: s.Range): s.SymbolInformation

  def resolveSymbol(name: String): s.SymbolInformation
}

case class SemanticdbSymbolResolver(db: TextDocument, symtab: SymbolTable) extends SymbolResolver {

  case class ResolvedSymbol(occurence: s.SymbolOccurrence) {
    lazy val info: Option[s.SymbolInformation] =
      symtab.info(occurence.symbol).orElse(db.symbols.find(_.symbol == occurence.symbol))
  }

  private val symbols: Map[s.Range, ResolvedSymbol] = db.occurrences.collect {
    case s@s.SymbolOccurrence(Some(range), _, _) => range -> ResolvedSymbol(s)
  }.toMap

  def resolveSymbol(range: s.Range): s.SymbolInformation =
    symbols(range).info.getOrThrow(new Exception(s"Symbol at range $range is not in semanticdb occurencies"))

  def resolveSymbol(name: String): s.SymbolInformation =
    db.symbols.find(_.symbol == name)
      .orElse(symtab.info(name))
      .getOrThrow(new Exception(s"Unable to find $name in symtab or in local symbols"))
}

object CallSiteExtractor {


}

case class Declaration(
                        kind: Kind,
                        fqn: String,
                        name: String,
                        isImplicit: Boolean,
                        parameterLists: Seq[ParameterList]
                      ) {

}

object Declaration {
  implicit def apply(x: s.SymbolInformation)(implicit context: SymbolResolver): Declaration = {
    val parameterLists: Seq[ParameterList] = x.signature match {
      case x: s.MethodSignature =>
        x.parameterLists.map(ParameterList(_))
      case _ => Seq()
    }

    Declaration(x.kind, x.symbol, x.name, (x.properties & IMPLICIT.value) > 0, parameterLists)
  }
}

case class ParameterList(isImplicit: Boolean)

object ParameterList {
  implicit def apply(x: s.Scope)(implicit context: SymbolResolver): ParameterList = {
    val isImplicit =
      x.symlinks
        .headOption
        .exists(x => (context.resolveSymbol(x).properties & IMPLICIT.value) > 0)

    ParameterList(isImplicit)
  }
}

sealed trait Type

case object Empty extends Type

case class Literal(value: Lit) extends Type

case class TypeRef(prefix: Type, symbol: s.SymbolInformation, args: Seq[Type]) extends Type

case class FunctionLiteral(params: Seq[Type]) extends Type

case class ArgumentsList(args: Seq[Type], syntactic: Boolean)

sealed trait CallSite {
  def declaration: Declaration

  def argss: Seq[ArgumentsList]

  def typeArgs: Seq[Type]

  def implicitArgs: Option[ArgumentsList] =
    declaration
      .parameterLists
      .find(_.isImplicit)
      .map(_ => argss.last)

  // FIXME: copy syntax does not work
  def update(
              declaration: Declaration = this.declaration,
              argss: Seq[ArgumentsList] = this.argss,
              typeArgs: Seq[Type] = this.typeArgs
            ): CallSite =
    this match {
      case x: NormalCall => x.copy(declaration = declaration, argss = argss, typeArgs = typeArgs)
      case x: SyntheticCall => x.copy(declaration = declaration, argss = argss, typeArgs = typeArgs)
      case x: ConversionCall => x.copy(declaration = declaration, argss = argss, typeArgs = typeArgs)
    }
}


case class NormalCall(
                       declaration: Declaration,
                       tree: Tree,
                       argss: Seq[ArgumentsList] = Seq(),
                       typeArgs: Seq[Type] = Seq()
                     ) extends CallSite {
  val code: String = tree.toString()

  override def toString: String = s"NormalCall(${declaration.name}, ${tree.productPrefix}($code)"
}

case class SyntheticCall(
                          declaration: Declaration,
                          range: Option[s.Range],
                          argss: Seq[ArgumentsList] = Seq(),
                          typeArgs: Seq[Type] = Seq()
                        ) extends CallSite {
  override def toString: String = s"SyntheticCall(${declaration.name}, ${range.map(_.toString).getOrElse("<no range>")})"
}

case class ConversionCall(
                           declaration: Declaration,
                           argss: Seq[ArgumentsList] = Seq(),
                           typeArgs: Seq[Type] = Seq()
                         ) extends CallSite {
  override def toString: String = s"ConversionCall(${declaration.name})"
}

class CallSiteExtractor(val db: s.TextDocument, val symtab: SymbolTable) {

  import CallSiteExtractor._

  implicit val resolver = SemanticdbSymbolResolver(db, symtab)

  val tree: Tree = db.text.parse[Source].get
  val synthetics: Seq[s.Synthetic] = db.synthetics

  // TODO: move a function and make immutable
  val declarations = mutable.Map[String, Declaration]()

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

  def resolveDeclaration(symbol: SymbolInformation): Declaration = {
    declarations.getOrElseUpdate(symbol.symbol, Declaration(symbol))
  }


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
    case t@Term.Name(_) =>
      TypeRef(Empty, resolver.resolveSymbol(t.pos), Seq())
    case t@Lit(_) =>
      Literal(t)
    case t@scala.meta.Type.Name(_) =>
      TypeRef(Empty, resolver.resolveSymbol(t.pos), Seq())
    case x =>
      throw new Exception(s"${x.structure} is not supported scala.meta.Type for type resolution")
  }

  def extractExplicitCallSites(tree: Tree): Seq[Try[CallSite]] = {
    val seenTerms = mutable.HashSet[Tree]()

    def createCallSite(fun: Tree, tree: Tree, argss: Seq[Seq[Term]]): CallSite = {
      val symbol = fun match {
        case x: Term => {
          val term = findNameTerm(x)
          seenTerms += term.parent.get
          resolver.resolveSymbol(term.pos)
        }
        case Name(_) =>
          resolver.resolveSymbol(fun.pos)
        case Type.Name(_) =>
          resolver.resolveSymbol(fun.pos)
        case _ =>
          throw new Exception(s"${fun.structure} is not supported to extract callsite symbol")
      }

      val declaration = resolveDeclaration(symbol)

      val typeArgs = findTypeArgs(tree)

      val nestedApplies = fun collect {
        case t@Term.Apply(_, _) => t
      }
      seenTerms ++= nestedApplies

      val allArgss = nestedApplies match {
        case Nil => argss
        case _ => argss ++ (nestedApplies collect { case Term.Apply(_, xs) => xs })
      }

      val argumentsLists =
        allArgss.map(xs => xs.map(resolveType)).map(xs => ArgumentsList(xs, true))

      NormalCall(declaration, tree, typeArgs = typeArgs, argss = argumentsLists)
    }

    def inImport(t: Tree): Boolean = {
      if (t.isInstanceOf[Import]) true
      else t.parent.exists(inImport)
    }

    tree collect {
      case t@Term.Apply(fun, args) if !seenTerms.contains(t) =>
        Try(createCallSite(fun, t, Seq(args)))
      case t@Term.Select(_, n: Term.Name) if !seenTerms.contains(t) && !inImport(t) =>
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
  final def findOriginalTreeRange(t: s.Tree): Option[s.Range] = t match {
    case s.SelectTree(qual, _) => findOriginalTreeRange(qual)
    case s.OriginalTree(Some(range)) => Some(range)
    case s.ApplyTree(fn, _) => findOriginalTreeRange(fn)
    case s.TypeApplyTree(fn, _) => findOriginalTreeRange(fn)
    case s.FunctionTree(_, fn) => findOriginalTreeRange(fn)
    case s.MacroExpansionTree(fn, _) => findOriginalTreeRange(fn)
    case _ => None
  }

  final def resolveTypeFromTree(x: s.Tree): Type = x match {
    case s.IdTree(symbol) =>
      TypeRef(Empty, resolver.resolveSymbol(symbol), Seq())
    case s.FunctionTree(params, _) =>
      FunctionLiteral(params map resolveTypeFromTree)
    case s.TypeApplyTree(fn, targs) =>
      resolveTypeFromTree(fn) match {
        case t@TypeRef(_, _, _) =>
          t.copy(args = targs map resolveType)
        case t =>
          throw new Exception(s"$t semanticdb Tree does not take type parameters while it should $x")
      }
    case _ =>
      throw new Exception(s"$x semanticdb Tree type is not yet supported")
  }

  final def resolveType(x: s.Type): Type = x match {
    case s.TypeRef(prefix, symbol, targs) =>
      TypeRef(resolveType(prefix), resolver.resolveSymbol(symbol), targs map resolveType)
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
    resolver.resolveSymbol(symbolName(t))
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

          // implicit arguments (e.g. `(Seq(1) ++ Seq(2))(canBuildFrom)`)
          case s.ApplyTree(fn, implicitArgs) =>
            // first process the nested call which might create synthetic calls
            implicitArgs.foreach(process)
            process(fn)

            val range = findOriginalTreeRange(fn).getOrThrow(new Exception(s"Unable to find OriginalTree in $fn"))
            val resolvedArgs = implicitArgs map resolveTypeFromTree
            val (idx, callSite) =
              findCallSite(range, true)
                .getOrThrow(new Exception(s"Unable to find callsite for $range to add implicit parameters"))

            callSites.update(
              idx,
              callSite.update(argss = callSite.argss :+ ArgumentsList(resolvedArgs, false))
            )

          // inferred type arguments (e.g. `Seq(1)` -> `Seq[Int](1)`)
          case s.TypeApplyTree(fn, targs) =>
            process(fn)

            findOriginalTreeRange(fn).foreach { range =>
              val typeArgs = targs map resolveType
              val (idx, cs) =
                findCallSite(range, false)
                  .getOrThrow(new Exception(s"Unable to find callsite for $range to add implicit parameters"))

              callSites.update(idx, cs.update(typeArgs = typeArgs))
            }

          // inferred method calls
          case s.SelectTree(qual, Some(fn)) =>
            process(qual)

            val range = findOriginalTreeRange(qual).getOrThrow(new Exception(s"Unable to find OriginalTree in $qual"))
            val resolvedFun = resolveName(fn)
            val declaration = resolveDeclaration(resolvedFun)

            findCallSite(range, false) match {
              // (e.g. `.apply` or `.unapplySeq`)
              case Some((idx, callSite: NormalCall)) => {
                callSites.update(idx, callSite.update(declaration = declaration))
              }

              // for-loop desuggaring (e.g. `for(i <- Seq(1,2) if (i & 1) == 0`)
              case _ => {
                callSites += SyntheticCall(declaration, Some(range))
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

    val good = callSites.map(Success(_))
    val bad = errors.map(Failure(_))

    good ++ bad
  }
}
