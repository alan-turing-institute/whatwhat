(lang dune 3.3)

(name whatwhat)

(generate_opam_files true)

(source
 (github alan-turing-institute/whatwhat))

(authors "Hut 23")

(maintainers "Hut 23")

(license MIT)

(documentation https://alan-turing-institute.github.io/whatwhat/whatwhat/index.html)

(package
 (name whatwhat)
 (version 0.0.29)
 (synopsis "A reimagining of nowwhat")
 (description "Project scheduling")
 (depends
  xdg-basedir
  yojson
  lwt
  lwt_ssl
  cohttp-lwt-unix
  calendar
  tyre
  yaml
  csv
  ppx_yojson_conv
  ppx_deriving
  ANSITerminal
  ounit2
  websocket-lwt-unix
  angstrom
  dune-build-info
  wcwidth
  )
 (tags
  (topics "REG")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
