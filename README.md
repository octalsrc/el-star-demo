# An Interactive Implementation of the `L*` Algorithm #

This README is the primary report and documentation for this project.

## Building ##

This might be a lot of work, depending on your system.

Follow
[these instructions](https://github.com/reflex-frp/reflex-platform#setup)
to get a shell with the "reflex platform" (the `ghcjs` compiler with
the `reflex` libraries in scope).  Before running `./try-reflex` you
will need to add the following packages to
`reflex-platform/packages.nix`:

- `fgl`
- `containers`
- `parsec`
- `mtl`

Once in the shell, go to the directory with this README and perform:

    cabal configure --ghcjs
    cabal build

## Usage ##

To run the demo, open
`./dist/build/el-star-demo/el-star-demo.jsexe/index.html` in a web
browser.

To observe the algorithm, make up a regular language (draw a
deterministic finite-state automaton to go with it) in the alphabet
"ab" (just the letters 'a' and 'b').

For example, "The language of all strings containing zero or more
'a's followed by zero or more 'b's".

    "aaaaab"
    ""
    "abb"
    "bbb"
    "a"
    
Answer the questions about your language as the app poses them to you.
The table at the bottom will grow at certain intervals as the
algorithm chooses new prefixes and suffixes to ask about and then will
fill in as you answer.

Eventually, when the table is "closed", the algorithm will propose a
DFA that might model your language.  If you reject the DFA with a
counterexample, the counterexample will be added to the suffixes in
the table and more questions will be asked to fill it in again.

Reload the page to start a new run.

## Motivation ##

This project has two purposes.  First, I want to make my understanding
of the `L*` algorithm by writing a well-structured implementation of
it.  Second, I want to make a tool that could be used by someone first
trying to learn the algorithm that could make the learning process
quicker and less frustrating.

Algorithms such as this one that aren't commonly used outside of
computer science can only be learned in a few ways:

1. From inert text, in a paper or textbook
2. From a professor in a lecture
3. From a professor or knowledgeable student one-on-one.

I tend to find the first two similarly non-interactive, and the third
is often infeasible because suitable teachers tend to also be very
busy.

Method #1 is usually what I use.  I usually sit down with the text and
try to perform a simple example run of the algorithm.  This can be
frustrating because the steps are often tedious and tend to blow up on
non-trivial inputs, and it's hard to tell if I've made a mistake until
I've wasted a lot of time going down an incorrect branch of
operations.

Ideally, I'd like a machine to execute the algorithm, but in such a
way that I can see exactly what it's doing and I can pause it at each
step to see if I can correctly reason out the next action.

## Implementation ##

I chose Haskell because I thought the "transparent algorithm" design
goal would fit well with some of the higher-order construction
elements commonly used in Haskell, particularly Monads.

In this case, I was able to design the `L*` algorithm (defined in
`./src/Automata/LStar.hs`) with respect to an anonymous Monadic
"teacher".  I used a teacher implementation that would ask questions
and print results on the commandline (the `AskTeacher`) while I was
writing the algorithm, and was able to switch to the `FunTeacher`
implementation found in `./src/Main.hs` when I found I needed the
whole algorithm run to be a piece of data passed around by the GUI
elements in the app, all with no changes to the algorithm
implementation.

## Conclusion ##

To really reach its goal, this app still needs a lot of work.  This is
really just a proof of concept.  I would like to include explanations
of why the table expands in certain ways as it occurs, and of course
let the user define their own alphabet.

