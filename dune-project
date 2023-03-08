(lang dune 3.3)

(name whatwhat)

(generate_opam_files true)

(source
 (github alan-turing-institute/whatwhat))

(authors "Hut 23")

(maintainers "Hut 23")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name whatwhat)
 (synopsis "A reimagining of nowwhat")
 (description "Project scheduling")
 (depends
  xdg-basedir
  yojson
  lwt
  cohttp-lwt-unix
  calendar
  re
  tls
  batteries
  yaml
  csv
  ppx_yojson_conv
  ppx_deriving
  ANSITerminal
  )
 (tags
  (topics "REG")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
