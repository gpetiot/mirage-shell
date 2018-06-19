
# mirage-starter

This is the minimal template for a Mirage unikernel.


## Prerequisites

- Install latest OPAM (at least 1.2.2), following instructions at
<https://opam.ocaml.org/>

- Install the `mirage` package with OPAM, updating your package first if
necessary:

```
    $ opam update -u
    $ opam install mirage
    $ eval `opam config env`
```

- Please ensure that your Mirage command-line version is at least 3.0.0 before
proceeding:

```
    $ mirage --version
    3.0.5
```

## Configure, Build, Run

There is a top-level `Makefile` at the root of this repository with
convenience functions for configuring, building, and running all of the examples
in one step.

```
    $ make all                   ## equivalent to ...
    $ make configure build
    $ make clean
```
