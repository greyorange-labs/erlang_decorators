-module(decorators).
-include_lib("eunit/include/eunit.hrl").

-export([parse_transform/2, pretty_print/1]).

%% TODO: add warnings for rogue decorators
parse_transform(Ast, _Options) ->
    % io:format("~nAst: ~p~n=======~n", [Ast]),
    % io:format("~nPretty Ast: ~s~n=======~n", [pretty_print(Ast)]),
    {ExtendedAst2, RogueDecorators} = lists:mapfoldl(fun transform_node/2, [], Ast),
    Ast2 =
        lists:flatten(lists:filter(fun(Node) -> Node =/= nil end, ExtendedAst2)) ++
            emit_errors_for_rogue_decorators(RogueDecorators),
    %%io:format("~p~n<<<<~n", [Ast2]),
    %%io:format("~s~n>>>>~n", [pretty_print(Ast2)]),
    Ast2.

pretty_print(Ast) -> lists:flatten([erl_pp:form(N) || N <- Ast]).

emit_errors_for_rogue_decorators(DecoratorList) ->
    [
        {error, {Line, erl_parse, ["rogue decorator ", io_lib:format("~p", [D])]}}
     || {attribute, Line, decorate, D} <- DecoratorList
    ].

%% transforms module level nodes
%% see http://www.erlang.org/doc/apps/erts/absform.html
%% outputs nil (to swallow the node), a single node, or a list of nodes.
%% nil nodes are removed in a subsequent pass and the lists flattened

transform_node(Node = {attribute, _Line, mod_decorate, _Decorator}, DecoratorList) ->
    %% keep a list of module level decorators but dont emit them in the code.
    %% this is important as you arent meant to have attributes after functions in a module
    {nil, [Node | DecoratorList]};
transform_node(Node = {attribute, _Line, group_decorator, _Decorator}, DecoratorList) ->
    %% keep a list of group_decorator level decorators but dont emit them in the code.
    {nil, [Node | DecoratorList]};
transform_node(Node = {attribute, _Line, export, ExportedFuns}, [
    {attribute, _Line0, group_decorator, _Decorator} = DecNode | DecoratorList
]) ->
    %% keep a list of group_decorator level decorators but dont emit them in the code.
    GroupDecorators = [{group_decorator, {Fun, Arity}, DecNode} || {Fun, Arity} <- ExportedFuns],
    {Node, GroupDecorators ++ DecoratorList};
transform_node(Node = {attribute, _Line, decorate, _Decorator}, DecoratorList) ->
    %% keep a list of decorators but dont emit them in the code.
    %% this is important as you arent meant to have attributes after functions in a module
    {nil, [Node | DecoratorList]};
transform_node(Node = {function, _Line, _FuncName, _Arity, _Clauses}, []) ->
    %% pass through decoratorless functions
    {Node, []};
transform_node(Node = {function, _Line, FuncName, Arity, _Clauses}, DecoratorList) ->
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
            {apply_decorators(Node, DecoratorListForThisFunction), AccDecoratorList}
    end;
transform_node(Node = {eof, _Line}, DecoratorList) ->
    DecoratorList0 = [
        Node0
     || Node0 = {attribute, _Line0, decorate, _Decorator} <- DecoratorList
    ],
    {[Node | emit_errors_for_rogue_decorators(DecoratorList0)], []};
transform_node(Node, DecoratorList) ->
    %% some other form (only other valid forms are other attributes)
    %% keep going
    {Node, DecoratorList}.

apply_decorators(Node = {function, Line, FuncName, Arity, _Clauses}, DecoratorList) when
    length(DecoratorList) > 0
->
    [
        %% output the original function renamed
        function_form_original(Node),
        %% output a trampoline into our decorator chain
        function_form_trampoline(Line, FuncName, Arity, DecoratorList)
        %% output our decorator chain
        | function_forms_decorator_chain(Line, FuncName, Arity, DecoratorList)
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

function_forms_decorator_chain(Line, FuncName, Arity, DecoratorList) ->
    NumDecorators = length(DecoratorList),
    DecoratorIndexes = lists:zip(DecoratorList, lists:seq(1, NumDecorators)),
    [
        function_form_decorator_chain(Line, FuncName, Arity, D, I)
     || {{attribute, _, Dec, D}, I} <- DecoratorIndexes,
        Dec =:= decorate orelse Dec =:= mod_decorate orelse Dec =:= group_decorator
    ].

function_form_decorator_chain(Line, FuncName, Arity, Decorator, DecoratorIndex) ->
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
            emit_decorated_fun(Line, Decorator, NextFuncName, ArgNames)
        ]}
    ]}.

emit_decorated_fun(Line, {DecMod, DecFun}, InnerFunName, ArgNames) when
    is_atom(DecMod), is_atom(DecFun)
->
    emit_decorated_fun(Line, {DecMod, DecFun, []}, InnerFunName, ArgNames);
emit_decorated_fun(Line, DecFun, InnerFunName, ArgNames) when
    is_atom(DecFun)
->
    emit_decorated_fun(Line, {DecFun, []}, InnerFunName, ArgNames);
emit_decorated_fun(Line, {DecMod, DecFun, DecData}, InnerFunName, ArgNames) when
    is_list(DecData)
->
    Arity = length(ArgNames),
    {call, Line, {remote, Line, {atom, Line, DecMod}, {atom, Line, DecFun}}, [
        {'fun', Line, {function, InnerFunName, Arity}},
        emit_var_list(Line, ArgNames),
        erl_parse:abstract(DecData)
    ]};
emit_decorated_fun(Line, {DecFun, DecData}, InnerFunName, ArgNames) when
    is_list(DecData)
->
    Arity = length(ArgNames),
    ArgList = [
        {'fun', Line, {function, InnerFunName, Arity}},
        emit_var_list(Line, ArgNames),
        erl_parse:abstract(DecData)
    ],
    emit_local_call(Line, DecFun, ArgList).

emit_local_call(Line, FuncName, ArgList) ->
    {call, Line, {atom, Line, FuncName}, ArgList}.

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
