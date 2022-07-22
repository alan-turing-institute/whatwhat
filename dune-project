(lang dune 3.3)

(name whatwhat)

(generate_opam_files true)

(source
 (github alan-turing-institute/whatwhat))

(authors "James Geddes")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name whatwhat)
 (synopsis "A reimagining of nowwhat")
 (description "Project scheduling ")
 (depends ocaml dune)
 (tags
  (topics "REG")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
