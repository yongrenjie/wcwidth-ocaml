(lang dune 3.3)

(name wcwidth)
(version 1.0.0)
(authors "Jonathan Yong")
(maintainers "Jonathan Yong")
(license MIT)

(source
 (github yongrenjie/wcwidth-ocaml))
; (documentation https://yongrenjie.github.io/wcwidth-ocaml)

(generate_opam_files true)

(package
  (name wcwidth)
  (synopsis "Get terminal display width of a string")
  (description "Get terminal display width of a string")
  )

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
