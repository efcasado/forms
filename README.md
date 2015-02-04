forms
=====
[![Build Status](https://travis-ci.org/efcasado/forms.svg?branch=master)](https://travis-ci.org/efcasado/forms)

A simple library that simplifies working with the Erlang abstract format.

> If you are curious about what `forms` is capable of doing, I suggest you to check out
> [meta](https://github.com/efcasado/meta).

### Fetching a module's forms

The `forms` module features a `read/1` function that can be used to fetch a module's abstract syntax tree (AST).
`read/1` works with both Erlang source files (i.e., files with the `.erl` suffix) and Erlang binary files
(i.e., files with the `.beam` suffix).

The line below would read the forms from Erlang's internal `lists` module.

```erl
forms:read(lists).
```

Similarly, the following line would read the forms from a developer-provided `hello_world` source file.

```erl
forms:read("src/hello_world.erl").
```


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

### Debug functions

Working with Erlang's abstract format is often tedious because Erlang developers are not used to see Erlang abstract code.
Unlike `Lisp`, Erlang is not `homonoic`, which means that Erlang's code does not have the same structure as its AST. Homoicinity makes metaprogramming easier than in a programming language without this property because code can be treated as data.

To fill this gap, `forms` features the `eval/1`, `from_abstract/1` and `to_abstract/1` debug functions that may come in handy
when working with Erlang's abstract code.

##### eval/1

Evaluate an Erlang expression (in string form) or abstract form.

```erl
forms:eval("1 + 1.").
%% => 2
```

```erl
forms:eval({op,1,'+',{integer,1,1},{integer,1,1}}).
%% => 2
```

##### to_abstract/1

Convert the provided Erlang attribute or expression (in string form) to its abstract format representation.

```erl
forms:to_abstract("hello(Name) -> io:format(\"Hello, ~s!~n\", [Name]).").
%% => {function,1,hello,1,
%%     [{clause,1,
%%      [{var,1,'Name'}],
%%       [],
%%       [{call,1,
%%        {remote,1,{atom,1,io},{atom,1,format}},
%%        [{string,1,"Hello, ~s!~n"},
%%         {cons,1,{var,1,'Name'},{nil,1}}]}]}]}
```

```erl
forms:to_abstract("-export([hello/1]).").
%% => {attribute,1,export,[{hello,1}]}
```

##### from_abstract/1

Convert the provided form into its Erlang attribute or expression counterpart.

```erl
forms:from_abstract({attribute,1,export,[{hello,1}]}).
%% => "-export([hello/1])."
```

```erl
forms:from_abstract({function,1,hello,1,
                     [{clause,1,
                      [{var,1,'Name'}],
                      [],
                      [{call,1,
                        {remote,1,{atom,1,io},{atom,1,format}},
                         [{string,1,"Hello, ~s!~n"},
                          {cons,1,{var,1,'Name'},{nil,1}}]}]}]}).
%% => "hello(Name) -> io:format(\"Hello, ~s!~n\", [Name])."
```

### Examples

Count how many times the anonymous variable (i.e., `'_'`) is used in the lists module.

```erl
Forms = forms:read(lists),
forms:reduce(fun({var, _, '_'}, Count) -> Count + 1; (_, Count) -> Count end, 0, Forms).
%% => 57
```

Easy. Isn't it? :-)
