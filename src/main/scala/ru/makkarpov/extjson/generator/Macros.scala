package ru.makkarpov.extjson.xgenerator

import play.api.libs.json.Format
import ru.makkarpov.extjson.StrFormat
import ru.makkarpov.extjson.annotations.{asString, requireImplicit}
import ru.makkarpov.extjson.generator.Macros.generateImpl

import scala.collection.mutable
import scala.reflect.macros.{blackbox, whitebox}

object Macros {
  // Calling macro bundle directly produced some weird errors like
  // > IllegalArgumentException: wrong number of arguments
  // before the first line of macro bundle was executed.
  def generateImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    val tag = implicitly[c.WeakTypeTag[T]]
    val mm = new Macros(c)
    mm.generate(tag.asInstanceOf[mm.c.WeakTypeTag[T]]).asInstanceOf[c.Tree]
  }

  def generateDebugImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    val tag = implicitly[c.WeakTypeTag[T]]
    val mm = new Macros(c)
    mm.generateDebug(tag.asInstanceOf[mm.c.WeakTypeTag[T]]).asInstanceOf[c.Tree]
  }

  def materializeFormat[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    val underlying = generateImpl(c)
    q"_root_.ru.makkarpov.extjson.GeneratedFormat($underlying)"
  }
}

class Macros(val c: blackbox.Context) extends Utils with CaseClasses with SealedClasses with Collections {
  import c.universe._

  // Serializers for same type with same settings are considered compatible with each other and aren't regenerated.
  case class GenerationSettings(fromString: Boolean)

  case class GenerationDependency(tpe: Type, requireImplicit: Boolean, settings: GenerationSettings) {
    def withSettings(f: GenerationSettings => GenerationSettings): GenerationDependency =
      copy(settings = f(settings))
  }

  trait GenerationContext {
    def abort(msg: String): Nothing
    def serializer(dep: GenerationDependency): Tree
  }

  trait GenerationPlugin {
    def appliesTo(tpe: Type): Boolean
    def getDependencies(tpe: Type, ctx: GenerationContext): Seq[GenerationDependency]
    def generate(tpe: Type, ctx: GenerationContext): Tree
  }

  private case class GenerationTask(dep: GenerationDependency, isInitial: Boolean, path: List[Type])
  private case class GenerationKey(tpe: Type, settings: GenerationSettings)

  private class GenerationStruct(val key: GenerationKey) {
    var term: TermName = TermName(c.freshName("serializer"))
    var dependencies = Set.empty[GenerationDependency]
    var requireImplicit: Boolean = false

    var isResolved: Boolean = false
    var resolvedImplicit: Tree = EmptyTree
    var resolvedPlugin: Option[GenerationPlugin] = None

    override def toString =
      s"GenerationStruct(term=$term, dependencies=$dependencies, isResolved=$isResolved, " +
        s"requireImplicit=$requireImplicit, resolvedImplicit=$resolvedImplicit)"
  }

  val KnownPlugins: Seq[GenerationPlugin] = Seq(
    new TraversablePlugin,
    new MapPlugin,
    new EnumerationPlugin,
    new OptionPlugin,

    // in that order!
    new TuplePlugin,
    new CaseClassPlugin,
    new SealedClassPlugin
  )

  protected def dependency(tpe: Type): GenerationDependency =
    GenerationDependency(tpe, annotationPresent[requireImplicit](tpe.typeSymbol),
      GenerationSettings(annotationPresent[asString](tpe.typeSymbol)))

  protected def dependency(tpe: Symbol): GenerationDependency =
    GenerationDependency(toType(tpe), annotationPresent[requireImplicit](tpe),
      GenerationSettings(annotationPresent[asString](tpe)))

  def generate(baseType: Type): Tree = {
    val tpe = baseType.dealias

    val generationQueue = mutable.Queue.empty[GenerationTask]
    val baseKey = GenerationKey(tpe, GenerationSettings(false))
    generationQueue.enqueue(GenerationTask(GenerationDependency(baseKey.tpe, false, baseKey.settings), true, Nil))

    val resolvedTypes = mutable.Map.empty[GenerationKey, GenerationStruct]

    while (generationQueue.nonEmpty) {
      val task = generationQueue.dequeue()

      def xabort(s: String): Nothing = {
        val invokedStr =
          if (task.path.nonEmpty) s"\ninvoked from: ${task.path.mkString(", ")}"
          else ""

        val msg =
          s"""In generation of Format[${task.dep.tpe}]:$invokedStr
             |
             |$s
           """.stripMargin

        c.abort(c.enclosingPosition, msg.trim)
      }

      val key = GenerationKey(task.dep.tpe, task.dep.settings)
      val struct = resolvedTypes.getOrElseUpdate(key, new GenerationStruct(key))

      struct.dependencies += task.dep

      if (task.dep.requireImplicit && !task.isInitial && !struct.requireImplicit) {
        struct.requireImplicit = true
        struct.isResolved = false
      }

      if (!struct.isResolved) {
        if (task.dep.settings.fromString) {
          val ret = implicitApplied(task.dep.tpe)(typeOf[StrFormat[_]], Seq(q"import $ownPkg.StrFormat._"))
          if (ret.isEmpty)
            xabort(s"@asString found and no implicit StrFormat[${task.dep.tpe}] is present")

          struct.resolvedImplicit = q"$ret.format"
        } else {
          val implicitTree =
            if (task.isInitial) EmptyTree
            else implicitApplied(task.dep.tpe)(typeOf[Format[_]], Seq(q"import $ownPkg.JsonFormats._"))

          if (implicitTree.isEmpty && struct.requireImplicit) {
            xabort(s"@requireImplicit is present and no implicit format was found. " +
              s"Consider providing implicit Format[${task.dep.tpe}]")
          }

          if (implicitTree.isEmpty) {
            KnownPlugins.find(_.appliesTo(task.dep.tpe)) match {
              case None => xabort(s"No plugin can handle that type. Consider providing implicit Format[${task.dep.tpe}].")
              case Some(x) =>
                val newPath = task.dep.tpe :: task.path

                val gctx = new GenerationContext {
                  override def abort(msg: String): Nothing = xabort(msg)

                  override def serializer(dep: GenerationDependency) =
                    throw new IllegalStateException("Serializers are not yet generated")
                }

                for (d <- x.getDependencies(task.dep.tpe, gctx))
                  generationQueue.enqueue(GenerationTask(d, isInitial = false, newPath))

                struct.resolvedPlugin = Some(x)
            }
          } else {
            struct.resolvedImplicit = implicitTree
          }
        }

        struct.isResolved = true
      }
    }

    val varDefs: Seq[Tree] =
      resolvedTypes.values.map { r =>
        if (r.resolvedImplicit.isEmpty) {
          q"val ${r.term}: $ownPkg.JsonUtils.RecursiveHelper[${r.key.tpe}] = new $ownPkg.JsonUtils.RecursiveHelper[${r.key.tpe}]()"
        } else {
          q"val ${r.term}: $jsonPkg.Format[${r.key.tpe}] = ${r.resolvedImplicit}"
        }
      }.toSeq

    val serializerDefs: Seq[Tree] =
      resolvedTypes.values.collect {
        case r if r.resolvedPlugin.nonEmpty =>
          val gctx = new GenerationContext {
            override def serializer(dep: GenerationDependency): c.universe.Tree =
              resolvedTypes.get(GenerationKey(dep.tpe, dep.settings)) match {
                case Some(x) => q"${x.term}"
                case None =>
                  abort(
                    s"""Internal macros error: no serializer was pre-generated for dependency:
                       |     type: ${dep.tpe}
                       | settings: ${dep.settings}
                       |
                       |Please define it in getDependencies().
                     """.stripMargin
                  )
              }

            override def abort(msg: String): Nothing =
              c.abort(c.enclosingPosition,
                s"""In generation of code for Format[${r.key.tpe}]:
                   |
                   |$msg
                 """.stripMargin.trim)
          }

          q"""${r.term}() = ${r.resolvedPlugin.get.generate(r.key.tpe, gctx)}"""
      }.toSeq

    val ret =
      q"""
         ..$varDefs
         ..$serializerDefs
         ${resolvedTypes(baseKey).term}
       """

//    c.abort(c.enclosingPosition, showCode(ret))

    ret
  }

  def generate[T](tag: WeakTypeTag[T]): Tree = generate(tag.tpe)

  def generateDebug[T](tag: WeakTypeTag[T]): Tree = {
    val r = generate(tag.tpe)

    c.abort(c.enclosingPosition,
      s"""Generated code:
         |==============
         |${showCode(r)}
         |==============
       """.stripMargin)
  }
}
