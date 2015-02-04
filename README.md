meta
====
[![Build Status](https://travis-ci.org/efcasado/meta.svg?branch=master)](https://travis-ci.org/efcasado/meta)


A metaprogramming library for the Erlang programming language. `meta` can be used to
transform Erlang modules not only at compile time, but also at run time.

> [Meta](http://en.wikipedia.org/wiki/Meta)
> (from the Greek preposition and prefix meta- `μετά-` meaning "after", or "beyond")
> is a prefix used in English to indicate a concept which is an abstraction from another
> concept, used to complete or add to the latter.
>
> [Metaprogramming](http://en.wikipedia.org/wiki/Metaprogramming)
> is the writing of computer programs with the ability to treat
> programs as their data. It means that a program could be designed to read, generate,
> analyse and/or transform other programs, and even modify itself while running.


### How to use this library

Most of the functions available in the `meta` module operate on other Erlang modules.
Erlang modules can be fed in to these functions in two flavors: 1) as a list of forms
(e.g., result of invoking `forms:read(<module-name>)`, etc.) and 2) as an atom (i.e,
module name). For instance, the function `meta:is_exported_function/3` can be used
as

```erl
meta:is_exported_function(new, 0, sets).
% => true
```

or as

```erl
SetsForms = forms:read(sets),
meta:is_exported_function(new, o, SetsForms).
% => true
```

indistinctively.


Moreover, when operating on modules specified as atoms, most of the functions
accept a list of options as the last argument. These options can be used to
specify how the transformations will be applied on the target module. At the
time of this writing, only two options are supported (i.e., `permanent` and
`force`). By default, all changes are transient (that is, they do not survive
to a node restart) and cannot be applied to protected modules. If one wants
to alter this behaviour, one has to set the `permanent` and/or `force` options
respectively.

Note that when operating on a module represented as a list of forms the
transformations will not be effective until they are applied by means of a
call to the `meta:apply_changes/{1, 2, 3}` function. This function accepts
the same `permanent` and `force` options described above.
