package org.scala_lang.virtualized

import scala.reflect.macros._
import whitebox.Context
import scala.language.experimental.macros
import scala.language.dynamics

trait Rep[+T] {
  val fields: List[(String, Any)]
}
trait Struct
trait Convert[From, To] {
  def convert(v: From): To
}

object RecordMacros {

  def apply_impl(c: Context)(method: c.Expr[String])(v: c.Expr[(String, Any)]*): c.Expr[Any] = {
    import c.universe._
    method.tree match {
      case Literal(Constant(str: String)) if str == "apply" =>
        recordApply(c)(v)
      case Literal(Constant(str: String)) =>
        val targetName = c.prefix.actualType.typeSymbol.fullName
        c.abort(NoPosition,
          s"value $str is not a member of $targetName")
      case _ =>
        val methodName = c.macroApplication.symbol.name
        c.abort(NoPosition,
          s"You may not invoke Rec.$methodName with a non-literal method name.")
    }
  }

  /**
   * Macro that implements [[Record.applyDynamicNamed]].
   */
  def recordApply(c: Context)(v: Seq[c.Expr[(String, Any)]]): c.Expr[Any] = {
    import c.universe._
    val constantLiteralsMsg =
      "Records can only be constructed with constant keys (string literals)."
    val noEmptyStrMsg =
      "Records may not have a field with an empty name"

    object Tuple2 {
      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        case q"($a, $b)" => Some((a, b))
        case q"scala.this.Tuple2.apply[..${ _ }]($a, $b)" => Some((a, b))
        case _ => None
      }
    }
    val tuples = v.map(_.tree).map {
      case Tuple2(Literal(Constant(s: String)), v) =>
        if (s == "") c.abort(NoPosition, noEmptyStrMsg)
        else (s, v)
      case Tuple2(_, _) =>
        c.abort(NoPosition, constantLiteralsMsg)
      case x =>
        c.abort(NoPosition, "Records can only be constructed with named parameters on apply (a = b).")
    }

    val schema = tuples.map {
      case (s, v) =>
        val widened = v.tpe.widen
        val tpe = if (widened <:< c.typeOf[Rep[_]]) widened.dealias match {
          case TypeRef(_, _, arg :: Nil) =>
            arg
        }
        else widened
        (s, tpe)
    }

    def checkDuplicate(schema: Seq[(String, c.Type)]): Unit = {
      val duplicateFields = schema.groupBy(_._1).filter(_._2.size > 1)
      if (duplicateFields.nonEmpty) {
        val fields = duplicateFields.keys.toList.sorted
        if (fields.size == 1)
          c.abort(NoPosition, s"Field ${fields.head} is defined more than once.")
        else
          c.abort(NoPosition, s"Fields ${fields.mkString(", ")} are defined more than once.")
      }
    }

    checkDuplicate(schema)

    val vals = schema.map {
      case (f, t) =>
        q"val ${TermName(f)}: $t"
    }
    val tpTree = tq"Struct { ..$vals }"
    c.Expr(q"newStruct[$tpTree](..${tuples.map(x => q"(${x._1}, ${x._2})")})")
  }

  def materializeImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context): c.Expr[Convert[Rep[A], B]] = {
    import c.universe._
    import compat._

    val srcMembers = c.weakTypeTag[A].tpe.members.collect { case x: MethodSymbol if x.isStable => x }
    val dstTpeMembers = srcMembers.map(x => q"""def ${x.name}: Rep[${x.returnType}]""")
    val dstMembers =
      srcMembers.map(x => q"""def ${x.name}: Rep[${x.returnType}] = field[${x.returnType}](rec, ${x.name.toString})""")

    c.Expr(q"""new Convert[Rep[${weakTypeOf[A]}],{..$dstTpeMembers}]{
      def convert(rec: Rep[${weakTypeOf[A]}]): {..$dstTpeMembers} = new {
        ..$dstMembers
      }
    }""")
  }

}

object Record extends Dynamic {

  /**
   * Create a "literal record" with field value pairs `v` using named
   * parameters:
   * {{{
   * Rec(name = "Hans", age = 7)
   * }}}
   */
  def applyDynamicNamed(method: String)(v: (String, Any)*): Any = macro RecordMacros.apply_impl

  implicit def materialize[A <: Struct, B]: Convert[Rep[A], B] = macro RecordMacros.materializeImpl[A, B]

  implicit def convert[A <: Struct, B](rec: Rep[A])(implicit ev: Convert[Rep[A], B]): B =
    ev.convert(rec)

}

