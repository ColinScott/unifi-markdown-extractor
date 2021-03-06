name := "unifi-markdown-extractor"

version := "0.1"

scalaVersion := "2.13.2"

val catsVersion = "2.1.1"
val catsEffectVersion = "2.1.3"
val circeVersion = "0.13.0"
val http4sVersion = "0.21.3"
val logbackVersion = "1.2.3"

val scalaCheckVersion = "1.14.3"
val spec2Version = "4.9.4"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % logbackVersion,

  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,

  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-client" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,

  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,

  "org.scalacheck" % "scalacheck_2.13" % scalaCheckVersion % Test,
  "org.specs2" %% "specs2-core" % spec2Version % Test,
  "org.specs2" %% "specs2-scalacheck" % spec2Version % Test,
  "org.specs2" %% "specs2-matcher-extra" % spec2Version % Test,
)

scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding", "utf-8", // Specify character encoding used by source files.
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.

  "-Werror", // Fail the compilation if there are any warnings.
  "-Wdead-code", //  Warn when dead code is identified.
  "-Wextra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Wmacros:after", // Only inspect expanded trees when generating unused symbol warnings.
  "-Wnumeric-widen", // Warn when numerics are widened.
  "-Woctal-literal", // Warn on obsolete octal syntax.
  "-Wunused:imports", //Warn if an import selector is not referenced.
  "-Wunused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Wunused:privates", // Warn if a private member is unused.
  "-Wunused:locals", // Warn if a local definition is unused.
  "-Wunused:explicits", // Warn if an explicit parameter is unused.
  "-Wunused:implicits", // Warn if an implicit parameter is unused.
  "-Wunused:params", // Enable -Wunused:explicits,implicits.
  "-Wvalue-discard", // Warn when non-Unit expression results are unused.

  "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  "-Xlint:nullary-override", // Warn when non-nullary def f() overrides nullary def f.
  "-Xlint:infer-any", // Warn when a type argument is inferred to be Any.
  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:option-implicit", // Option.apply used implicit view.
  "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
  "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:nonlocal-return", // A return statement used an exception for flow control.
  "-Xlint:implicit-not-found", // Check @implicitNotFound and @implicitAmbiguous messages.
  "-Xlint:serial", // @SerialVersionUID on traits and non-serializable classes.
  "-Xlint:valpattern", // Enable pattern checks in val definitions.
  "-Xlint:eta-zero", // Warn on eta-expansion (rather than auto-application) of zero-ary method.
  "-Xlint:eta-sam", // Warn on eta-expansion to meet a Java-defined functional interface that is not explicitly annotated with @FunctionalInterface.
  "-Xlint:deprecation", // Enable linted deprecations.

  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-numeric-widen", // Warn when numerics are widened.
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
  "-Ywarn-unused:params", // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates", // Warn if a private member is unused.
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused.
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)