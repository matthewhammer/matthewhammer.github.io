CSCI 7000 -- Course Project Proposal
=====================================

This is a template.  Please do the following by March 20; we will finalize by March 26:

1. To propose a course project, fill in the fields marked with XXX below.
2. Make a gist on github; paste your project proposal into this gist.
3. Share a link to this gist on the class discussion page.

Student name: XXX (your name)

Project Description:
---------------------
- **Project kind**:    XXX (choose an option below)
- **Project details**: XXX (write a sentence or two to define the scope of the project; this refines the project kind with more specific details and choices)

Expected Milestones:  
--------------------
- Week 1 (March 20--26): Finalize project scope and details.
- Week 2 (March 27--April 2): XXX
- Week 3 (April 3--April 9) : XXX
- Week 4 (April 10--April 16): XXX
- Week 5 (April 17--April 23): XXX
- Week 6 (April 24--May 6): XXX


The remainder of this document provides some project ideas that you can use to fill in the blanks above.  
_(You do not need to include the remainder of this document when you post your project description)._

Project Kinds
=================
 
- IC algorithm implementation, with PL techniques
- IC algorithm implementation, without PL techniques
- IC library implementation
- IC survey paper
- Students' choice


IC algorithm implementation: Project Ideas
===========================================

...with PL techniques (Adapton.Rust)
--------------------------------------

For each algorithm listed below, a project could consist of these parts:

1. Implement the (non-incremental) version of the algorithm in Rust
2. Characterize random inputs by writing an input generator in Rust
3. Characterize random input edits by writing an edit generator in Rust
4. Run 1, with 2 and 3 in Adapton Lab. Look at the visualized output.
5. Gradually, add annotations to the Rust code to make it incremental; repeat step 4 as you go.

Step 5 is the hardest and most time-consuming.  Further, it is
unlikely to be completed by the end of the semester unless the
algorithm is simple.

However, I want to encourage some students to choose _interesting_
algorithms, if they want to do so.  So, if you choose a more complex
algorithm (with an interesting non-incremental implementation as a
functional Rust program), you can receive full credit for your project
if you do steps 1--4 and only make _some_ progress on number 5.

Roughly speaking, the divide-and-conquer and dynamic algorithms listed
below are "simple", meaning that we should hope to make an incremental
version in Rust for a course project.  The others are more involved,
and may be substantial enough to warrent doing steps 1--4 completely
and only formulating an educated plan for step 5.

...without PL techniques
--------------------------------------

As an alternative to using Rust and Adapton, one can also do steps
1--4 in another language.  For step 5, one can implement an "ad hoc"
incremental algorithm in place of using Adapton.

For example, this chapter about graphs that we read in class provides
some examples:

  http://dl.acm.org/citation.cfm?id=1882766  
  Camil Demetrescu, David Eppstein, Zvi Galil, and Giuseppe F. Italiano.   
  2010. Dynamic graph algorithms.   
  Algorithms and theory of comp. handbook  

Note however that this chapter, and others like it, rarely give code
listings!  This fact makes projects of this kind quite challenging,
potentially.

Algorithm Ideas
===============

Here's a list of algorithms that may be interesting to implement in
Rust as functional programs, and to (eventually) make incremental with
Adapton.

(Also, please feel free to suggest additional algorithms that are not
yet listed).

Divide-and-Conquer
------------------
- Quicksort
- Mergesort
- Quickhull
- Integer multiplication (for huge integers)

Dynamic Programming
--------------------
- Weighted interval scheduling
- Knapsack problem
- Sequence alignment (e.g., for Bio applications)

Graph algorithms
------------------
- Breadth-first-search, 
- Test bipartiteness
- Test for cycles (return count of cycles, or just a bit saying if there is at least one)
- Minimum spanning tree
- Djikstra's shortest path,
- All-pairs shortest path,
- A-star search

Games (0-player, 1-player, etc)
--------------------------------
- Conway's game of life
- A* search (e.g., in a 2D grid with obstacles/mazes)
- Min/max search, with alpha-beta pruning (e.g., to encode chess, checkers, go, etc.) 

Logic programming
-----------------
- Datalog evaluation
- Prolog evaluation

Generalized A-star
------------------
- See also: https://arxiv.org/abs/1110.2216
- This framework can solve vision problems, and other problems sometimes tackled by machine learning.
- It uses a variation of A* search, over _parse trees_ of a formal grammar (think, "chart parsing, but with weights")


IC library implementation: Project Ideas
===========================================

This is a category of projects that build alternatives to the
Rust+Adapton solution we've discussed in class.

 - Implement Adapton in another programming language (e.g., Haskell,
   Scala, Typescript, etc.).  The current implementations are in OCaml
   and Rust.  Older implementations that are not full featured exist
   in Python, but the use of "names" is missing there, for instance.

 - Implement another IC approach, e.g., one that we read about in class.

Survey Paper
================

This is a survey class, so a survey paper can complement what we've
done in class nicely.  One kind of possible survey paper dives deeper
into something that we only discussed for one day.  Another kind would
give a survey a larger collection of papers, though it should be more
than a recap of what we discussed over the semester.  Again, it should
draw out connections that we did not see, or that we did not have time
to explore in detail.

Student's Choice
================

Propose something that I have not considered here!