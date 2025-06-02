(lang dune 3.18)

(name zin)

(generate_opam_files true)

(source
 (github nikhilcodes777/zin))

(authors "Nikhil <nikhil7x7@gmail.com>")

(maintainers "Nikhil <nikhil7x7@gmail.com>")

(license LICENSE)

; Update it to Readme file and write a better readme

(documentation https://github.com/nikhilcodes777/zin)

(package
 (name zin)
 (synopsis "A cool language")
 (description
  "A cool programming language made in coolest programming langauge")
 (depends ocaml dune cmdliner ppx_deriving ounit2 base odoc)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
