-module(decorator_test).
-include_lib("eunit/include/eunit.hrl").
-export([replace_return_value_decorator/3, options_decorator/3]).

-compile([{parse_transform, decorators}]).


% example decorator that replaces the return value with the atom 'replaced'
% note that we always pass the arguments as a single list to the next fun
% DecData now includes {orig_mfa, {Module, Function, Arity}} from the transform
replace_return_value_decorator(F, Args, _DecData)->
    _R = apply(F, Args),
    replaced.

-decorate({?MODULE, replace_return_value_decorator}).
replace_ret_val_decorated() -> ok.


replace_args_decorator(F, _Args, _DecData)->
    apply(F, [replaced1, replaced2]).

-decorate(replace_args_decorator).
replace_args_decorated(replaced1, replaced2) -> ok.

-decorate(replace_return_value_decorator).
-decorate(replace_args_decorator).
multiple_decorators(replaced1, replaced2) ->
    ok.

options_decorator(Fun, Args, Options) ->
    % Options now includes {orig_mfa, ...} prepended to the original options
    % Extract just the option value to verify the decorator options were passed correctly
    [{orig_mfa, _} | UserOptions] = Options,
    ?assertEqual([{option, value}], UserOptions),
    apply(Fun, Args).

-decorate({options_decorator, [{option, value}]}).
-decorate({?MODULE, options_decorator, [{option, value}]}).
options_decorated() ->
    ok.

replace_ret_value_test()->
    ?assertEqual(replaced, replace_ret_val_decorated()),
    ?assertEqual(ok, replace_args_decorated(arg1, arg2)),
    ?assertEqual(replaced, multiple_decorators(arg1, arg2)),
    ?assertEqual(ok, options_decorated()).

