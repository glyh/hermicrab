(lang dune 3.15)

(name hermicrab)

(using menhir 2.0)

(generate_opam_files true)

(source
 (github glyh/hermicrab))

(authors "Yihang Liu")

(maintainers "Yihang Liu")

(license GPL-3.0-or-later)

(documentation https://github.com/glyh/hermicrab)

(package
 (name hermicrab)
 (synopsis "HermiCrab takes place of the shell!")
 (description "HermiCrab takes place of the shell!")
 (depends
   ocaml
   dune
   (menhir (= 20231231))
   (core :with-test)
   (ppx_jane :with-test)
   ; (minttea (= 0.0.2))
   (linenoise (>= 1.5.1))
   (nice_parser (= 1.0.0))
   (alcotest :with-test))
 (tags
  ("hmc" "shell" "unix" "tui")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
