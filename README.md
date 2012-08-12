GrowCode
========

A non-parametric framework for learning how networks grow

This tool accompanies the paper:

R. Patro+ G. Duggal+, E. Sefer, H. Wang, D. Filippova, C. Kingsford. The
missing models: a data driven approach for learning how networks grow. To
Appear: Proc. 18th Intl. Conference on Knowledge Discovery and Data Mining
(KDD) 2012. 

\+ Equal contributions

Requirements
------------

For building and editing source:
* Java 1.6 or greater
* Scala 2.9.1 or greater. (http://scala-lang.org)
* Simple Build Tool (SBT) 

Running
-------

The 'runexample.sh' script shows how to run a basic learning and program
selection procedure using the SBT framework.  The learning procedure is
associated with a variety of genetic algorithm parameters that can be
customized in each example's '.params' file.  Please see the ECJ documentation
(ECJ link) for more information on what these parameters mean.

To Do
-----

* Make the network properties tested in BestProgramProblem.scala selectable via
  a config file or command line interface
* Add a runnable jar to the repo
