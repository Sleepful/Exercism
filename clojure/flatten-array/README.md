# Flatten Array

Welcome to Flatten Array on Exercism's Clojure Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Take a nested list and return a single flattened list with all values except nil/null.

The challenge is to take an arbitrarily-deep nested list-like structure and produce a flattened structure without any nil/null values.

For example:

input: [1,[2,3,null,4],[null],5]

output: [1,2,3,4,5]

## Appendix

~~~~exercism/note
The instructions above are synchronized with a shared repository to ensure consistency across all language tracks.
This appendix provides additional clarification or modifies the instructions as needed to better align with the goals of the Clojure track.
~~~~

For this exercise in the Clojure track, you may **assume the input is any nested combination of vectors, and the output is a vector**, as indicated by the tests.
However, this is not a strict requirement; you are free to assume that the input is any nested combination of sequential things (lists, vectors, etc.), and the output is a sequence.

## Source

### Created by

- @haus

### Contributed to by

- @AndreaCrotti
- @mwfogleman
- @object88
- @sjwarner-bp
- @yurrriq
- @tasxatzial

### Based on

Interview Question - https://reference.wolfram.com/language/ref/Flatten.html