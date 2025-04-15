# Erlang Decorators

An early implementation of Python-style decorators for Erlang.

## Quick Start

Add the following to your `rebar.config` file:
```erlang
{deps,
 [
    {decorators, "", {git, "git://github.com/chrisavl/erlang_decorators.git", {branch, "master"}}}
 ]
}.
```

Include `-compile([{parse_transform, decorators}]).` in any source file that uses decorators, or add `{erl_opts, [{parse_transform, decorators}]}.` to your `rebar.config`.

## Usage

### Module-Level Decorators
There are four ways to apply a decorator to a function:
```erlang
-mod_decorate(mod_decorator). %% Local function, no decorator data
-mod_decorate({mod_decorator, [{option, value}]}). %% Local function, with data
-mod_decorate({my_module, mod_decorator}). %% External function, no decorator data
-mod_decorate({my_module, mod_decorator, [{option, value}]}). %% External function, with data

%% NOTE: If no data/options are specified, it defaults to an empty list.
```

### Function Group Decorators
```erlang
-group_decorator({my_module, group_decorator, [{option, value}]}).
-export([
    a/0,
    b/0
]).
-export([
    c/0
]).

%% The `my_module:group_decorator` will be applied to all functions exported in the immediately following `-export` block.
%% In the example above, it will be applied to functions `a/0` and `b/0`.
```

### Function-Level Decorators
Function-level decorators are similar to module-level decorators, but use the `decorate` attribute instead of `mod_decorate`.

## Example

```erlang
-module(mymod).
-compile([{parse_transform, decorators}]).
%% Module-level decorator, applied to all functions in the module
-mod_decorate({my_module, my_decorator, [{option, value}]}).

-group_decorator({my_module, group_decorator, [{option, value}]}).
-export([
    example_1/0,
    example_2/0
]).
-export([
    example_3/0,
    example_4/0
]).

my_decorator(OriginalFun, OriginalArgs, Data) ->
        Result = apply(OriginalFun, OriginalArgs),
        case proplists:get_value(print_input, Data) of
                true -> io:format("Input Args = ~p~n", [OriginalArgs]);
                _ -> ok
        end,
        Result.

%% 1. Local decorator function, no decorator data
-decorate(my_decorator).
example_1(A, B) ->
        timer:sleep(100),
        A + B.

%% 2. Local decorator function, with decorator data
-decorate(my_decorator, [{print_input, true}]).
example_2(A, B) ->
        A * B.

%% 3. Decorator function from another module, no decorator data
-decorate(diff_module, decorate_fun).
example_3(A, B) ->
        A * B.

%% 4. Decorator function from another module, with decorator data
-decorate(diff_module, decorate_fun, [{print_input, false}]).
example_4(A, B) ->
        A * B.
```
