forms
=====
[![Build Status](https://travis-ci.org/efcasado/forms.svg?branch=master)](https://travis-ci.org/efcasado/forms)

A simple library that simplifies working with the Erlang abstract format.

### Fetching a module's forms

The `forms` module features a `read/1` function that can be used to fetch a module's abstract syntax tree (AST).
`read/1` works with both Erlang source files (i.e., files with the `.erl` suffix) and Erlang binary files
(i.e., files with the `.beam` suffix).

> Note that in order to be able to fetch a beam file's AST the Erlang binary file must have been compiled using the
> `debug_info` option (e.g., `erlc -o ebin +debug_info src/hello_world.erl`). Obviously, this is not a requirement
> when reading the forms from a source file.


### High-order functions

Traversing an Erlang module's AST is now as simple as traversing a list.
The `forms` library ships with the `map/{2,3}`, `reduce\{3,4}`, `mr/3`, `filter/2`, `any/2` and `all/2` functions,
which are anologous to those available in the `lists` module.

Most of the above mentioned functions support two different traversal modes: 1) (valid) `forms only` and 2) `full`.
The latter (i.e., `full`) is the default traversal mode. If one wants to change to `forms_only`, one can do so by using the
optional `options` argument (e.g., `forms:map(Fun, Forms, _Opts = [forms_only]).`.
