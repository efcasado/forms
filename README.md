forms
=====
[![Build Status](https://travis-ci.org/efcasado/forms.svg?branch=master)](https://travis-ci.org/efcasado/forms)

A simple library that simplifies working with the Erlang abstract format.

### High-order functions

Traversing an Erlang module's abstract syntax tree (AST) is now as simple as traversing a list.
The `forms` library ships with the `map/{2,3}`, `reduce\{3,4}`, `mr/3`, `filter/2`, `any/2` and `all/2` functions,
which are anologous to those available in the `lists` module.

Most of the above mentioned functions support two different traversal modes: 1) (valid) `forms only` and 2) `full`.
The latter (i.e., `full`) is the default traversal mode. If one wants to change to `forms_only`, one can do so by using the
optional `options` argument (e.g., `forms:map(Fun, Forms, _Opts = [forms_only]).`.
