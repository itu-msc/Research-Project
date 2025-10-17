# Rizzo Ocaml Library

This is an OCaml library implementation of the Rizzo FRP model.

## Installation

To install the library, use OPAM:

```bash
opam install rizzo
```

## Development

To set up the development environment, clone the repository and install dependencies:

```bash
git clone ...

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
bun run exec
```

