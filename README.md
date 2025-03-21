[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/K-qtPRYu)
# haskell-streaming

The task is to recreate a portion of the functionality of the unix utility
[`wc`](https://linux.die.net/man/1/wc).

## Functionality

The program should run in constant memory &mdash; otherwise, your solution might
not be graded at all. The following command-line options have to be supported:

0. `--help` (Nice, informative message) (1 point)
1. `-c, --bytes` (1 point)
2. `-m, --chars` (1 point)
3. `-l, --lines` (4 points)

```
stack build

stack run wc example_texts/example.txt -- -m -c -l
stack run wc example_texts/utf8_example.txt -- -m -c -l
```

Please see the [`wc` docs](https://linux.die.net/man/1/wc) for what every option
should do. The binary should accept one additional **positional** CLI argument
&mdash; a path to the file to read. You mustn't read from stdin.

The rest of the points are given for:

* Making sure all the various I/O edge cases are handled and performing an
  analysis of exception handling in your application will give you 1 more point.
* Properly parsing the CLI arguments into a separate data structure will give
  you 2 additional points for a total of 10.

## Implementation

You should use the
[`nightly-2024-10-11`](https://www.stackage.org/nightly-2024-10-11) stack
snapshot in your project.

You should use the [streamly](https://hackage.haskell.org/package/streamly)
family of packages for streaming functionality. Using a streaming package will
help ensure constant memory.

(Alternatively, you can use [foldl](https://hackage.haskell.org/package/foldl).)

You should use the
[optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
package to parse CLI arguments.

You can use any additional libraries if you wish to do so. However, basic `wc`
functionality has to be written from scratch.
