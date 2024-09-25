# Toy Computer
Scala implementation of Toy-16 and Toy-8 from `Computer Science: Algorithms, Theory, and Machines` course.

Toy-16, or Toy, is a simulation of computer with 16 8-bit word registers, 256 8-bit word memory, and minimal
instruction set. Toy is powerful enough to perform any computation a "real" computer can do. Look at `@main`
method for how to run. 

Toy-8, is single 8-bit register with 16 8-bit registers with a tiny instruction set. It is also powerful enough to
do arbitrary computation. Uncomment it's `@main` method (comment out the one in Toy) to see it work. For Toy-8, all
the circuits down to the wires and On/Off power signals are simulated. This would be much harder for Toy, with it's
larger instruction set.

Official [booksite](https://introcs.cs.princeton.edu/java/62toy/) for course.