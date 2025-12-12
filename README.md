# Rizzo OCaml Library

This is an OCaml library implementation of the Rizzo FRP model.

## Import the Library

To use the Rizzo library in your OCaml project, add `rizzo` as a dependency in your `dune` file:

```lisp
(library
 (name your_project_name)
 (libraries rizzo))
```

[https://ocaml.org/p/rizzo/latest](https://ocaml.org/p/rizzo/latest)


## Installation

To install the library, use OPAM:

```bash
opam install rizzo
```

## Development

To set up the development environment, clone the repository and install dependencies:

```bash
git clone https://github.com/itu-msc/Research-Project.git

cd Research-Project
opam install . --deps-only

# opam env Linux/MacOS
eval $(opam env)

# opam env Windows
opam env | Invoke-Expression
# or
./opam-env.ps1

# build the project
dune build
# or
bun run build

# run Rizzo
cd src
dune exec Rizzo
# or
bun run dev
```

## Examples

You can find minor examples in the [main.ml](src/bin/main.ml) file. 
To run the examples execute:

```bash
dune exec Rizzo
```

or

```bash
bun dev
```
