(lang dune 1.10)

(generate_opam_files true)

(license MIT)
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://gitlab.inria.fr/tmartine/ppx_show"))
(homepage "https://gitlab.inria.fr/tmartine/ppx_show")
(bug_reports "https://gitlab.inria.fr/tmartine/ppx_show")
(documentation "https://gitlab.inria.fr/tmartine/ppx_show")

(package
 (name ppx_show)
 (synopsis "OCaml PPX deriver for deriving show based on ppxlib")
 (depends
   (ppxlib (>= 0.8.0))
   (stdcompat (>= 9))))

