lms-kappa
=========

LMS-based Kappa simulator

Installation
------------

First of all, since LMS-Kappa is written in Scala and Scala runs on the JVM, you will need a [Java Development Kit](http://openjdk.java.net/).

1. [Install sbt](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html#installing-sbt)
3. [Install virtualization-lms-core](https://github.com/TiarkRompf/virtualization-lms-core)

        $ git clone https://github.com/TiarkRompf/virtualization-lms-core
        $ sbt compile publish-local

2. Clone the repo: `git clone https://github.com/sstucki/lms-kappa.git`
4. Go into the root directory and compile the source code using sbt: `cd lms-kappa && sbt compile`

Usage
-----

If you just want to play a little bit around, then probably the easiest way is to go to `src/test/scala/kappa/models/` and create your own file there. It should begin with `package kappa.models`. You can use any of the available models that are in there as a template.

If instead you want to start your own project, then follow these steps:

1. Compile and publish LMS-Kappa in your local Ivy2 repository (~./ivy2) by running `sbt compile publish-local` in the root directory.
2. Create a [sbt project](http://www.scala-sbt.org/release/docs/Getting-Started/Hello.html).
3. Make sure you include `"EPFL" %% "lms" % "0.3-SNAPSHOT"` and `"EPFL" %% "lms-kappa" % "0.1-SNAPSHOT"` as `libraryDependencies` in your project's `build.sbt`.

Tip: To program in Scala, you can use any Java IDE ([Eclipse](http://scala-ide.org/), [IntelliJ](http://confluence.jetbrains.com/display/SCA/Getting+Started+with+IntelliJ+IDEA+Scala+Plugin), [NetBeans](https://github.com/dcaoyuan/nbscala), etc) or text editor. We use **[Emacs](http://www.gnu.org/software/emacs/)**. If you use Emacs 24, install [scala-mode2](https://github.com/hvesalai/scala-mode2). It will make your life much easier.


