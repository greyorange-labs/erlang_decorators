-module(decorators).
-include_lib("eunit/include/eunit.hrl").

-export([parse_transform/2, pretty_print/1]).

%% TODO: add warnings for rogue decorators
parse_transform(Ast, _Options) ->
    % io:format("~nAst: ~p~n=======~n", [Ast]),
    % io:format("~nPretty Ast: ~s~n=======~n", [pretty_print(Ast)]),
    ModuleName = extract_module_name(Ast),
    {ExtendedAst2, RogueDecorators} = lists:mapfoldl(fun(Node, Acc) -> transform_node(Node, Acc, ModuleName) end, [], Ast),
    Ast2 =
        lists:flatten(lists:filter(fun(Node) -> Node =/= nil end, ExtendedAst2)) ++
            emit_errors_for_rogue_decorators(RogueDecorators),
    %%io:format("~p~n<<<<~n", [Ast2]),
    %%io:format("~s~n>>>>~n", [pretty_print(Ast2)]),
    Ast2.

pretty_print(Ast) -> lists:flatten([erl_pp:form(N) || N <- Ast]).

extract_module_name(Ast) ->
    case lists:keyfind(module, 3, Ast) of
        {attribute, _Line, module, ModuleName} -> ModuleName;
        false -> undefined
    end.

emit_errors_for_rogue_decorators(DecoratorList) ->
    [
        {error, {Line, erl_parse, ["rogue decorator ", io_lib:format("~p", [D])]}}
     || {attribute, Line, decorate, D} <- DecoratorList
    ].

%% transforms module level nodes
%% see http://www.erlang.org/doc/apps/erts/absform.html
%% outputs nil (to swallow the node), a single node, or a list of nodes.
%% nil nodes are removed in a subsequent pass and the lists flattened

transform_node(Node = {attribute, _Line, mod_decorate, _Decorator}, DecoratorList, _ModuleName) ->
    %% keep a list of module level decorators but dont emit them in the code.
    %% this is important as you arent meant to have attributes after functions in a module
    {nil, [Node | DecoratorList]};
transform_node(Node = {attribute, _Line, group_decorator, _Decorator}, DecoratorList, _ModuleName) ->
    %% keep a list of group_decorator level decorators but dont emit them in the code.
    {nil, [Node | DecoratorList]};
transform_node(Node = {attribute, _Line, export, ExportedFuns}, [
    {attribute, _Line0, group_decorator, _Decorator} = DecNode | DecoratorList
], _ModuleName) ->
    %% keep a list of group_decorator level decorators but dont emit them in the code.
    GroupDecorators = [{group_decorator, {Fun, Arity}, DecNode} || {Fun, Arity} <- ExportedFuns],
    {Node, GroupDecorators ++ DecoratorList};
transform_node(Node = {attribute, _Line, decorate, _Decorator}, DecoratorList, _ModuleName) ->
    %% keep a list of decorators but dont emit them in the code.
    %% this is important as you arent meant to have attributes after functions in a module
    {nil, [Node | DecoratorList]};
transform_node(Node = {function, _Line, _FuncName, _Arity, _Clauses}, [], _ModuleName) ->
    %% pass through decoratorless functions
    {Node, []};
transform_node(Node = {function, _Line, FuncName, Arity, _Clauses}, DecoratorList, ModuleName) ->
    %% Apply decorators to this function and Remove function level decorators
    AccDecoratorList =
        lists:filter(
            fun
                ({attribute, _Line0, decorate, _Decorator}) -> false;
                (_ModOrgroup) -> true
            end,
            DecoratorList
        ),
    DecoratorListForThisFunction =
        lists:filtermap(
            fun
                ({group_decorator, {FunName0, Arity0}, Decorator}) ->
                    case {FunName0, Arity0} of
                        {FuncName, Arity} -> {true, Decorator};
                        _ -> false
                    end;
                (Decorator) ->
                    {true, Decorator}
            end,
            DecoratorList
        ),
    case DecoratorListForThisFunction of
        [] ->
            %% no decorators for this function
            {Node, AccDecoratorList};
        _ ->
            %% apply decorators to this function
            {apply_decorators(Node, DecoratorListForThisFunction, ModuleName), AccDecoratorList}
    end;
transform_node(Node = {eof, _Line}, DecoratorList, _ModuleName) ->
    DecoratorList0 = [
        Node0
     || Node0 = {attribute, _Line0, decorate, _Decorator} <- DecoratorList
    ],
    {[Node | emit_errors_for_rogue_decorators(DecoratorList0)], []};
transform_node(Node, DecoratorList, _ModuleName) ->
    %% some other form (only other valid forms are other attributes)
    %% keep going
    {Node, DecoratorList}.

apply_decorators(Node = {function, Line, FuncName, Arity, _Clauses}, DecoratorList, ModuleName) when
    length(DecoratorList) > 0
->
    [
        %% output the original function renamed
        function_form_original(Node),
        %% output a trampoline into our decorator chain
        function_form_trampoline(Line, FuncName, Arity, DecoratorList)
        %% output our decorator chain
        | function_forms_decorator_chain(Line, FuncName, Arity, DecoratorList, ModuleName)
    ].

function_form_original({function, Line, FuncName, Arity, Clauses}) ->
    {function, Line, generated_func_name({original, FuncName}), Arity, Clauses}.

%% outputs a single clause function that gets the first decorator chain function and calls it
function_form_trampoline(Line, FuncName, Arity, DecoratorList) ->
    NumDecorators = length(DecoratorList),
    ArgNames = arg_names(Arity),
    {function, Line, FuncName, Arity, [
        {clause, Line, emit_arguments(Line, ArgNames), emit_guards(Line, []), [
            emit_local_call(
                Line,
                generated_func_name({decorator_wrapper, FuncName, NumDecorators}),
                emit_arguments(Line, ArgNames)
            )
        ]}
    ]}.

function_forms_decorator_chain(Line, FuncName, Arity, DecoratorList, ModuleName) ->
    NumDecorators = length(DecoratorList),
    DecoratorIndexes = lists:zip(DecoratorList, lists:seq(1, NumDecorators)),
    [
        function_form_decorator_chain(Line, FuncName, Arity, D, I, ModuleName)
     || {{attribute, _, Dec, D}, I} <- DecoratorIndexes,
        Dec =:= decorate orelse Dec =:= mod_decorate orelse Dec =:= group_decorator
    ].

function_form_decorator_chain(Line, FuncName, Arity, Decorator, DecoratorIndex, ModuleName) ->
    ArgNames = arg_names(Arity),
    NextFuncName =
        case DecoratorIndex - 1 of
            0 ->
                generated_func_name({original, FuncName});
            N ->
                generated_func_name({decorator_wrapper, FuncName, N})
        end,
    {function, Line, generated_func_name({decorator_wrapper, FuncName, DecoratorIndex}), Arity, [
        {clause, Line, emit_arguments(Line, ArgNames), emit_guards(Line, []), [
            %% DecMod:Decfun(fun NextFun/1, [Arg1, Arg2, ...]).
            emit_decorated_fun(Line, Decorator, NextFuncName, ArgNames, FuncName, Arity, ModuleName)
        ]}
    ]}.

emit_decorated_fun(Line, {DecMod, DecFun}, InnerFunName, ArgNames, OriginalFuncName, OriginalArity, ModuleName) when
    is_atom(DecMod), is_atom(DecFun)
->
    emit_decorated_fun(Line, {DecMod, DecFun, []}, InnerFunName, ArgNames, OriginalFuncName, OriginalArity, ModuleName);
emit_decorated_fun(Line, DecFun, InnerFunName, ArgNames, OriginalFuncName, OriginalArity, ModuleName) when
    is_atom(DecFun)
->
    emit_decorated_fun(Line, {DecFun, []}, InnerFunName, ArgNames, OriginalFuncName, OriginalArity, ModuleName);
emit_decorated_fun(Line, {DecMod, DecFun, DecData}, InnerFunName, ArgNames, OriginalFuncName, OriginalArity, ModuleName) when
    is_list(DecData)
->
    case validate_decorator_data(Line, DecData) of
        {ok, ValidatedData} ->
            Arity = length(ArgNames),
            EnhancedDecData = [{orig_mfa, {ModuleName, OriginalFuncName, OriginalArity}} | ValidatedData],
            {call, Line, {remote, Line, {atom, Line, DecMod}, {atom, Line, DecFun}}, [
                {'fun', Line, {function, InnerFunName, Arity}},
                emit_var_list(Line, ArgNames),
                erl_parse:abstract(EnhancedDecData)
            ]};
        {error, ErrorData} ->
            throw(ErrorData)
    end;
emit_decorated_fun(Line, {DecFun, DecData}, InnerFunName, ArgNames, OriginalFuncName, OriginalArity, ModuleName) when
    is_list(DecData)
->
    case validate_decorator_data(Line, DecData) of
        {ok, ValidatedData} ->
            Arity = length(ArgNames),
            EnhancedDecData = [{orig_mfa, {ModuleName, OriginalFuncName, OriginalArity}} | ValidatedData],
            ArgList = [
                {'fun', Line, {function, InnerFunName, Arity}},
                emit_var_list(Line, ArgNames),
                erl_parse:abstract(EnhancedDecData)
            ],
            emit_local_call(Line, DecFun, ArgList);
        {error, ErrorData} ->
            throw(ErrorData)
    end.

emit_local_call(Line, FuncName, ArgList) ->
    {call, Line, {atom, Line, FuncName}, ArgList}.

%% Validates that DecData is a strict proplist
%% A strict proplist is a list of {Key, Value} tuples (Key can be any term)
%% Empty list is acceptable
-spec is_proplist_strict(term()) -> boolean().
is_proplist_strict([]) ->
    true;
is_proplist_strict([{_Key, _Value} | Rest]) ->
    is_proplist_strict(Rest);
is_proplist_strict(_) ->
    false.

%% Validates DecData during AST transformation
%% Returns {ok, DecData} if valid, {error, Reason} if invalid
validate_decorator_data(Line, DecData) when is_list(DecData) ->
    case is_proplist_strict(DecData) of
        true -> {ok, DecData};
        false ->
            ErrorMsg = format_decorator_data_error(DecData),
            {error, {Line, invalid_decorator_options, ErrorMsg}}
    end;
validate_decorator_data(Line, DecData) ->
    ErrorMsg = format_decorator_data_error(DecData),
    {error, {Line, invalid_decorator_options, ErrorMsg}}.

%% Formats helpful error messages for invalid decorator data
format_decorator_data_error(DecData) ->
    FormattedData = io_lib:format("~p", [DecData]),
    lists:flatten([
        "Invalid decorator options. Decorator data must be a proplist [{key, value}, ...] or empty list []. ",
        "Got: ", FormattedData, ". "
    ]).

emit_arguments(Line, AtomList) ->
    [{var, Line, Arg} || Arg <- AtomList].

emit_guards(_Line, []) ->
    [];
emit_guards(_, _) ->
    throw(not_yet_implemented).

emit_var_list(Line, AtomList) ->
    %% build a list of args out of cons cells
    %% {cons, 43, {var, 43, 'Arg1'}, {cons, 43, {var, 43, 'Arg2'}, {nil, 43}}}
    lists:foldr(
        fun(Arg, Acc) ->
            {cons, Line, {var, Line, Arg}, Acc}
        end,
        {nil, Line},
        AtomList
    ).

generated_func_name({original, OrigName}) ->
    atom_name([OrigName, "_original___"]);
generated_func_name({trampoline, OrigName}) ->
    OrigName;
generated_func_name({decorator_wrapper, OrigName, N}) ->
    atom_name([OrigName, "_decorator", N, "___"]).

%% list() -> atom()
atom_name(Elements) ->
    list_to_atom(
        lists:flatten(
            lists:map(
                fun
                    (A) when is_atom(A) -> atom_to_list(A);
                    (A) when is_number(A) -> io_lib:format("~p", [A]);
                    (A) when is_binary(A) -> io_lib:format("~s", [A]);
                    (A) when is_list(A) -> io_lib:format("~s", [A])
                end,
                Elements
            )
        )
    ).

arg_names(Arity) ->
    [atom_name(["Arg", ArgNum]) || ArgNum <- lists:seq(1, Arity)].

% for example
%    -decorate({decmod, decfun2}).
%    -decorate({decmod, decfun1}).
%    baz(N1, N2) -> 0.
% is transformed into
%    baz_arity2_original(N1, N2) -> 0.
%    baz_arity2_0([N1, N2]) -> baz_arity2_original(N1, N2).
%    baz_arity2_1(Args) ->
%        F = decmod:decfun1(fun baz_arity2_0/1, Args),
%        F().
%    baz_arity2_2(Args) ->
%        F = decmod:decfun2(fun baz_arity2_1/1, Args),
%        F().
%    baz(N1, N2) -> baz_arity2_2([N1, N2]).
% which is output as
% {function, 35, baz_arity2_original, 0, [{clause, 35, [], [], [{integer, 35, 0}]}]},
% {function, 36, baz_arity2_0, 1,
%     [{clause, 36,
%          [{cons, 36, {var, 36, 'N1'}, {cons, 36, {var, 36, 'N2'}, {nil, 36}}}],
%          [],
%          [{call, 36,
%               {atom, 36, baz_arity2_original},
%               [{var, 36, 'N1'}, {var, 36, 'N2'}]}]}]},
% {function, 37, baz_arity2_1, 1,
%     [{clause, 37,
%          [{var, 37, 'Args'}],
%          [],
%          [{match, 38,
%               {var, 38, 'F'},
%               {call, 38,
%                   {remote, 38, {atom, 38, decmod}, {atom, 38, decfun1}},
%                   [{'fun', 38, {function, baz_arity2_0, 1}}, {var, 38, 'Args'}]}},
%           {call, 39, {var, 39, 'F'}, []}]}]},
% {function, 40, baz_arity2_2, 1,
%     [{clause, 40,
%          [{var, 40, 'Args'}],
%          [],
%          [{match, 41,
%               {var, 41, 'F'},
%               {call, 41,
%                   {remote, 41, {atom, 41, decmod}, {atom, 41, decfun2}},
%                   [{'fun', 41, {function, baz_arity2_1, 1}}, {var, 41, 'Args'}]}},
%           {call, 42, {var, 42, 'F'}, []}]}]},
% {function, 43, baz, 2,
%     [{clause, 43,
%          [{var, 43, 'N1'}, {var, 43, 'N2'}],
%          [],
%          [{call, 43,
%               {atom, 43, baz_arity2_2},
%               [{cons, 43, {var, 43, 'N1'}, {cons, 43, {var, 43, 'N2'}, {nil, 43}}}]}]}]},

atom_name_test_() ->
    [
        ?_assertEqual(foobar, atom_name([foo, bar])),
        ?_assertEqual(foobarbaz1, atom_name([foo, "bar", <<"baz">>, 1]))
    ].

args_to_list_form_of_args_test() ->
    Line = 1,
    ?assertEqual(
        {cons, Line, {var, Line, 'Arg1'}, {cons, Line, {var, Line, 'Arg2'}, {nil, Line}}},
        emit_var_list(Line, ['Arg1', 'Arg2'])
    ).

%% Tests for orig_mfa enhancement
extract_module_name_test_() ->
    [
        ?_assertEqual(mymod, extract_module_name([
            {attribute, 1, module, mymod}
        ])),
        ?_assertEqual(another_module, extract_module_name([
            {attribute, 1, module, another_module},
            {attribute, 2, export, [{foo, 1}]}
        ])),
        ?_assertEqual(undefined, extract_module_name([
            {attribute, 1, export, [{foo, 1}]}
        ]))
    ].

emit_decorated_fun_with_external_decorator_test() ->
    Line = 10,
    InnerFunName = my_func_original___,
    ArgNames = ['Arg1', 'Arg2'],
    OriginalFuncName = my_func,
    OriginalArity = 2,
    ModuleName = mymod,

    Result = emit_decorated_fun(Line, {mydecorator, my_decorator_fun}, InnerFunName, ArgNames, OriginalFuncName, OriginalArity, ModuleName),

    % Extract the abstract form to verify orig_mfa was added
    {call, Line, {remote, Line, {atom, Line, mydecorator}, {atom, Line, my_decorator_fun}},
        [{'fun', Line, {function, InnerFunName, 2}}, _VarList, AbstractData]} = Result,

    % Verify the abstract form contains orig_mfa tuple
    {cons, _, {tuple, _, [{atom, _, orig_mfa}, {tuple, _, [{atom, _, mymod}, {atom, _, my_func}, {integer, _, 2}]}]}, {nil, _}} = AbstractData.

emit_decorated_fun_with_local_decorator_test() ->
    Line = 15,
    InnerFunName = another_func_original___,
    ArgNames = ['Arg1'],
    OriginalFuncName = another_func,
    OriginalArity = 1,
    ModuleName = testmod,

    Result = emit_decorated_fun(Line, my_local_decorator, InnerFunName, ArgNames, OriginalFuncName, OriginalArity, ModuleName),

    % Extract the abstract form to verify orig_mfa was added
    {call, Line, {atom, Line, my_local_decorator},
        [{'fun', Line, {function, InnerFunName, 1}}, _VarList, AbstractData]} = Result,

    % Verify the abstract form contains orig_mfa tuple
    {cons, _, {tuple, _, [{atom, _, orig_mfa}, {tuple, _, [{atom, _, testmod}, {atom, _, another_func}, {integer, _, 1}]}]}, {nil, _}} = AbstractData.

emit_decorated_fun_with_existing_decdata_test() ->
    Line = 20,
    InnerFunName = func_original___,
    ArgNames = ['Arg1', 'Arg2', 'Arg3'],
    OriginalFuncName = func,
    OriginalArity = 3,
    ModuleName = testmod2,
    ExistingDecData = [{custom_key, custom_value}, {another_key, 42}],

    Result = emit_decorated_fun(Line, {mydecorator, my_decorator_fun, ExistingDecData}, InnerFunName, ArgNames, OriginalFuncName, OriginalArity, ModuleName),

    % Extract the abstract form to verify orig_mfa was prepended
    {call, Line, {remote, Line, {atom, Line, mydecorator}, {atom, Line, my_decorator_fun}},
        [{'fun', Line, {function, InnerFunName, 3}}, _VarList, AbstractData]} = Result,

    % Verify the abstract form starts with orig_mfa tuple, followed by other data
    {cons, _,
        {tuple, _, [{atom, _, orig_mfa}, {tuple, _, [{atom, _, testmod2}, {atom, _, func}, {integer, _, 3}]}]},
        {cons, _,
            {tuple, _, [{atom, _, custom_key}, {atom, _, custom_value}]},
            {cons, _,
                {tuple, _, [{atom, _, another_key}, {integer, _, 42}]},
                {nil, _}
            }
        }
    } = AbstractData.

emit_decorated_fun_preserves_order_with_decdata_test() ->
    Line = 25,
    InnerFunName = func_original___,
    ArgNames = ['X', 'Y'],
    OriginalFuncName = func,
    OriginalArity = 2,
    ModuleName = mymodule,
    ExistingDecData = [{first, 1}, {second, 2}],

    Result = emit_decorated_fun(Line, {some_decorator, ExistingDecData}, InnerFunName, ArgNames, OriginalFuncName, OriginalArity, ModuleName),

    % Extract the abstract form to verify order
    {call, Line, {atom, Line, some_decorator},
        [{'fun', Line, {function, InnerFunName, 2}}, _VarList, AbstractData]} = Result,

    % Verify structure: orig_mfa first, then other items
    {cons, _,
        {tuple, _, [{atom, _, orig_mfa}, {tuple, _, [{atom, _, mymodule}, {atom, _, func}, {integer, _, 2}]}]},
        {cons, _,
            {tuple, _, [{atom, _, first}, {integer, _, 1}]},
            {cons, _,
                {tuple, _, [{atom, _, second}, {integer, _, 2}]},
                {nil, _}
            }
        }
    } = AbstractData.

%% Tests for proplist validation
is_proplist_strict_test_() ->
    [
        ?_assert(is_proplist_strict([])),
        ?_assert(is_proplist_strict([{key, value}])),
        ?_assert(is_proplist_strict([{key1, value1}, {key2, value2}])),
        ?_assert(is_proplist_strict([{1, value}])),
        ?_assert(is_proplist_strict([{key, value}, {another, 123}, {"string_key", data}])),
        ?_assertNot(is_proplist_strict([{key, value, extra}])),
        ?_assertNot(is_proplist_strict([atom_key])),
        ?_assertNot(is_proplist_strict(["string", "list"])),
        ?_assertNot(is_proplist_strict(123)),
        ?_assertNot(is_proplist_strict(not_a_list))
    ].

validate_decorator_data_valid_proplist_test() ->
    Line = 10,
    ValidData = [{option, value}, {another_option, 42}],
    {ok, Result} = validate_decorator_data(Line, ValidData),
    ?assertEqual(ValidData, Result).

validate_decorator_data_empty_list_test() ->
    Line = 10,
    {ok, Result} = validate_decorator_data(Line, []),
    ?assertEqual([], Result).

validate_decorator_data_atom_list_test() ->
    Line = 10,
    ValidData = [{option1, value1}, {option2, value2}, {option3, value3}],
    {ok, Result} = validate_decorator_data(Line, ValidData),
    ?assertEqual(ValidData, Result).

validate_decorator_data_invalid_test() ->
    Line = 15,
    InvalidData = [{key, value, extra}],
    {error, {RetLine, RetError, _RetMsg}} = validate_decorator_data(Line, InvalidData),
    ?assertEqual(Line, RetLine),
    ?assertEqual(invalid_decorator_options, RetError).

validate_decorator_data_not_list_test() ->
    Line = 20,
    InvalidData = not_a_list,
    {error, {RetLine, RetError, _RetMsg}} = validate_decorator_data(Line, InvalidData),
    ?assertEqual(Line, RetLine),
    ?assertEqual(invalid_decorator_options, RetError).

emit_decorated_fun_validates_data_test() ->
    Line = 10,
    InnerFunName = func_original___,
    ArgNames = ['Arg1'],
    OriginalFuncName = func,
    OriginalArity = 1,
    ModuleName = testmod,

    % Valid data should succeed
    ValidData = [{option, value}],
    Result = emit_decorated_fun(Line, {my_dec, my_fun, ValidData}, InnerFunName, ArgNames, OriginalFuncName, OriginalArity, ModuleName),
    ?assertMatch({call, _, _, _}, Result),

    % Invalid data should throw
    InvalidData = [{key, value, extra}],
    ?assertThrow({_, invalid_decorator_options, _},
        emit_decorated_fun(Line, {my_dec, my_fun, InvalidData}, InnerFunName, ArgNames, OriginalFuncName, OriginalArity, ModuleName)
    ).
