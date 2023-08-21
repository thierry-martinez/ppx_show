# OCaml PPX deriver for deriving `show` based on `ppxlib`.

This library reimplements the `show` plugin from [`ppx_deriving`]
directly as a `ppxlib` deriver.

[`ppx_deriving`]: https://github.com/ocaml-ppx/ppx_deriving

## Usage

`ppx_show` can be used with [`dune`] by using the [`preprocess`] field.
`ppx_show.runtime` has to be added to runtime [`libraries`].

[`dune`]: https://github.com/ocaml/dune
[`preprocess`]: https://dune.readthedocs.io/en/latest/concepts.html#preprocessing-with-ppx-rewriters
[`libraries`]: https://dune.readthedocs.io/en/latest/concepts.html#library-deps

```lisp
(executable
  ...
  (preprocess (pps ppx_show))
  (libraries ppx_show.runtime)
  ...)
```

Using `[@@deriving show]` on a type `t` creates the following two functions:

```ocaml
val pp : Format.formatter -> t -> unit
val show : t -> string
```

By default, constructor and field identifiers are prefixed with the current module path. This behaviour can be turned off by writing `[@@deriving show { with_path = false }]` instead.
