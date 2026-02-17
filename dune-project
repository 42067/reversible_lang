(lang dune 3.21)

(name reversible_lang)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name reversible_lang)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml ocamllex menhir)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

(using menhir 3.0)
