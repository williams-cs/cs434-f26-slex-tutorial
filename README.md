# The slex Tutorial Companion

Every example in the
[slex tutorial](https://www.cs.williams.edu/~freund/cs434/slex.html)
is a real file in this repository: a spec file at the root, plus a
small program that loads and runs it.  Run each one with sbt --
without arguments it lexes the tutorial's sample input, and anything
you pass is lexed instead:

    sbt "runMain toy.demo"                  # toy.slex     the toy while-language
    sbt "runMain toy.demo if (2 <= 3)"
    sbt "runMain laws.rel"                  # rel.slex     Law 1: maximal munch
    sbt "runMain laws.kw"                   # kw.slex      Law 2: first rule wins ties
    sbt "runMain mini.demo"                 # mini.slex    a small cut of the IC lexer
    sbt "runMain strings.demo"              # strings.slex action blocks at work
    sbt "runMain hw.lex"                    # hw2.slex     HW 2's three-rule lexer

The laws, mini, and hw demos take a `-trace` flag that narrates every
munch -- the tutorial's Watching It Scan section (`sbt "runMain
laws.kw -trace whilewhilst"`), and `slex.check -dump` prints a spec's
rules, its combined NFA, and the DFA built from it, state by state:

    sbt "runMain slex.check -dump kw.slex"

The tutorial's claims about these files are also an MUnit suite
(`src/test/scala/tutorial/TutorialTests.scala`):

    sbt test

Modify everything freely as you read -- nothing in this repository is
handed in.

The library source is included under `src/main/scala/slex` (plus the
scup parsing library it builds on, under `src/main/scala/scup`) --
read it, it is short.
