# ODGSim
Grid Simulator for the Open DC Grid.

Under development!

Currently running from the command line doesn't do anything because the default grid is empty
so it's best to install the Scala development tools and run the test suite.

To run:  
You need Java 8+ JDK - either Oracle or OpenJDK  (java --version to check)  
Then go to the Scala main site for documentation, libraries etc is [scala-lang.org](https://www.scala-lang.org/).
Follow the instructions ([download](https://www.scala-lang.org/download/)) on that site to get a working scala installation.

This will install:  
SBT - Scala build tool

Alternatively - the project uses IntelliJ community edition with the Scala plugin (which installs SBT as a side effect):  
[Jetbrains Intellij IDEA Community Edition](https://www.jetbrains.com/idea/) then [Scala Plugin](https://plugins.jetbrains.com/plugin/1347-scala?_ga=2.87603032.1953515542.1593715163-1632901351.1593715163)

 With this installed you can go into the project directory and run the app using sbt from the command line:
 
     sbt run
 
 Not very interesting, after all the compilation blather it just silently exits. More interesting:
 
     sbt test

This runs a bunch of tests from src/test. The most interesting one is [GridTest.scala](https://github.com/jlgula/ODGSim/blob/master/src/test/scala/org/opendcgrid/app/sim/GridTest.scala)
using grid configurations defined in [Samples.scala](https://github.com/jlgula/ODGSim/blob/master/src/test/scala/org/opendcgrid/app/sim/GridTest.scala).
You can get a richer trace of what the grid is doing by enabling trace flags in the run configuration.
Examples are commented out in GridTest.scala.


To Do SOON!
Read grid configurations and trace flags from a JSON file so everything can run from the command line.

