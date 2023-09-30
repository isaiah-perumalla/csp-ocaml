# Instructions


You are asked to write a program which outputs a flower bed design for a garden, subject to some constraints.

Your program should take two inputs, passed as command line arguments:
1. A filename containing the layout of the garden
2. A filename containing the details of the flowers to be planted

The program should print the solution to standard output.
You can assume that the input files will be correctly formed.

Use Java unless otherwise agreed.
We would expect your solution to solve at least the examples provided, but not necessarily all possible gardens.
We would expect you to understand the strengths and weaknesses of your approach.
We would expect you to spend a couple of hours on the problem.

As well as how well your program solves the problem, we will review your code against the same quality standards that we
would apply internally for a shared codebase.

Please submit your entire solution in a form which can be compiled and executed independently.


## Garden File

The garden input file will be a text file which visually represents the empty garden layout and possible planting locations.
Walls are represented as 'X' and possible planting locations as ' '. All gardens will be rectangular.
You may plant a flower only on a ' ', and in a position which meets that flowers' constraints.

An example garden input file is shown below:
```
XXXXXXXXXX
X      XXX
X X   XXXX
X     XXXX
X     XX X
XXXXXXXXXX
```

In this garden there are 21 possible positions in which a flower can be planted.


## Flower File

The flower input file will be a text file with each line containing a tuple of flower type, the number of flowers to plant
of that type, and a constraint on how close flowers of that type can be planted. There will only by a single line for each
flower type.

An example flower input file is show below:
- a,3,1
- b,2,2
- c,2,2
- d,2,5

This should be interpreted as:
3 flowers of type 'a' should be planted - the minimum distance is 1 (so these can be placed next to each other)
2 flowers of type 'b' should be planted - the minimum distance is 2 (so all 'b' flowers must have at least distance 2 to all other 'b' flowers)
2 flowers of type 'c' should be planted - the minimum distance is 2
2 flowers of type 'd' should be planted - the minimum distance is 5

When considering the distance between two plants, use the manhattan distance (vertical distance + horizontal distance).


Example Output
==============

One possible solution for the garden and flowers given above is shown below:
```
XXXXXXXXXX
Xaaa  dXXX
XbXb  XXXX
X c   XXXX
Xc    XXdX
XXXXXXXXXX
```


In this garden, the distances are as follows, all of which meet the minimum requirements:
- a: 1 + 0 -> 1
- b: 2 + 0 -> 2
- c: 1 + 1 -> 2
- d: 2 + 3 -> 5 