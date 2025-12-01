(lang dune 3.20)

(name rizzo)

(generate_opam_files true)

(source
 (github itu-msc/Research-Project))

(authors "Authors Patrick Matthiesen and tjobi")

(maintainers "Maintainers Patrick Matthiesen and tjobi")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name rizzo)
 (version 0.1.0)
 (synopsis "FRP library implementation of Rizzo")
 (description "Rizzo is a Functional Reactive Programming (FRP) library implemented in OCaml.
  It provides abstractions and tools for building reactive applications with ease and efficiency.
  Rizzo was made as one solution to the challenges with time leaks in FRP systems.")
 (depends
  (ocaml (>= 5.3))
  (dune (= 3.20)))
 (tags
  ("FRP" "space leaks" "time leaks" "signals" "channels" "functional reactive programming")))

(map_workspace_root false)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
