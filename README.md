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

### Error handling

Most of the functions will throw an exception in case they face an error
during their execution.

| Function           | Exception                    | When                                  |
| ------------------ | ---------------------------- | ------------------------------------- |
| module_name/1      | invalid_module               | The provided forms do not contain a `-module(<module-name>).` attribute  |
| function/3         | {function_not_found, {F, A}} | The specified function is not implemented by the provided module         |
| spec/3             | {spec_not_found, {F, A}}     | The specified function specification is not found in the provided module |
| type/3             | {type_not_found, {T, A}}     | The specified type is not defined in the provided module                 |
| record/3           | {record_not_found, R}        | The provided module does not have a definition for the specified record  |
| *                  | {cannot_load_forms, Module}  | The AST of the provided module cannot be loaded. Most likely because it has not been compiled using the `+debug_info` option |
| *, apply_changes/3 | {protected, Module}          | Attempting to apply changes on a protected module without setting the `force` option                                       |
| *, apply_changes/3 | {compile_error, Module}      | There is an error in the AST one is attempting to compile                |
| *, apply_changes/3 | Error                        | When setting the `permanent` option, if an error occurs when attempting to create the `.beam` file                              |

### Examples

##### Function injection

The code below illustrates how to add a `hello/1` function to an arbitrary
module.

```erl
HelloFunction = forms:function(hello,
                               fun(Name) ->
                                   io:format("Hello, ~s!~n", [Name])
                               end,
                               []),
meta:add_function(HelloFunction, _Export = true, my_awsome_module).
```

The example above assumes you have compiled the code using the `forms_pt` parse
transform. If you cannot use the `forms_pt` parse transform, you can obtain the
form of the `hello/1` function using `forms:to_abstract/1`.

```erl
HelloFunction = forms:to_abstract("hello(Name) -> io:format(\"Hello, ~s!~n\", [Name]).").
```

##### (Temporary) latency measurement in a live system

This example illustrates how to use `meta` to measure how long does it take to
start a virtual node in a live Riak deployment.

```erl
%% Wrapper function
WrapperFunction =
    forms:to_abstract(
        "init(X) ->"
        "    {Time, Value} = timer:tc(fun() -> '_init'(X) end),"
        "    io:format(user, \"[meta] init/1 latency = ~p~n\", [Time]),"
        "    Value.").

Forms0 = forms:read(riak_core_vnode).
Forms1 = meta:rename_function(init, 1, '_init', false, Forms0).
Forms2 = meta:add_function(WrapperFunction, false, Forms1).
meta:apply_changes(Forms2).

%% ...
%% [meta] init/1 latency = 51
%% [meta] init/1 latency = 6
%% ...

%% ... after some time we decide to disable the measurements ...

%% Rollback
meta:apply_changes(Forms0).
```

Note that the code above has been executed on the shell process running on
a Riak node and no restart was required.
