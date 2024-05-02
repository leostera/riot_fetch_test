(lang dune 3.16)

(name riot_fetch_test)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name riot_fetch_test)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune riot blink)
 (tags
  (topics "to describe" your project)))


(pin
  (url "git+https://github.com/suri-framework/blink")
  (package (name blink)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
