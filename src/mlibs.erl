-module(mlibs).

-compile(export_all).

-type id() :: {neg_integer(), pos_integer()}.

-export_type([
    id/0
    ]).

% @doc Get current system time in milli_seconds
-spec get_time() -> Result when
    Result :: pos_integer().

get_time() ->
    erlang:convert_time_unit(erlang:system_time(), native, milli_seconds).

% @doc Generate unique id
-spec gen_id() -> Result when
    Result :: id().

gen_id() ->
    {erlang:monotonic_time(), erlang:unique_integer([monotonic,positive])}.

% @doc Generate random atom
-spec random_atom() -> Result when
    Result :: atom().

random_atom() -> 
    list_to_atom(erlang:ref_to_list(make_ref())).

% @doc Generate password (by erlangcentral)
-spec generate_password(Number) -> Password when
    Number :: pos_integer(),
    Password :: list().

generate_password(Number) ->
    lists:map(fun (_) -> rand:uniform(90)+$\s+1 end, lists:seq(1,Number)).

% @doc generate binary/atom key from list
% todo(maybe): implement concuting binaries not via lists
% should look into this for binaries and compare performance
% something like erlang:iolist_to_binary([<<"foo">>, <<"bar">>])
%bjoin(List) ->
%    F = fun(A, B) -> <<A/binary, B/binary>> end,
%    lists:foldr(F, <<>>, List).

% atomic
build_atom_key(List) -> build_key_lists(List, [], "_", atomic).
build_atom_key(List, Delm) -> build_key_lists(List, [], Delm, atomic).
% binary
build_binary_key(List) -> build_key_lists(List, [], ".", binary).
build_binary_key(List, Delm) -> build_key_lists(List, [], Delm, binary).
% support function for build key with lists module
build_key_lists([H|T],[], Delm, Type) when is_atom(H) ->
    build_key_lists(T, atom_to_list(H), Delm, Type);
build_key_lists([H|T],[], Delm, Type) when is_list(H) ->
    build_key_lists(T, H, Delm, Type);
build_key_lists([H|T],[], Delm, Type) when is_binary(H) ->
    build_key_lists(T, binary_to_list(H), Delm, Type);
build_key_lists([H|T],Acc, Delm, Type) when is_atom(H) ->
    build_key_lists(T, lists:concat([Acc, Delm, atom_to_list(H)]), Delm, Type);
build_key_lists([H|T],Acc, Delm, Type) when is_list(H) ->
    build_key_lists(T, lists:concat([Acc, Delm, H]), Delm, Type);
build_key_lists([H|T],Acc, Delm, Type) when is_binary(H) ->
    build_key_lists(T, lists:concat([Acc, Delm, binary_to_list(H)]), Delm, Type);
% output
build_key_lists([], Acc, _Delm, atomic) ->
    list_to_atom(Acc);
build_key_lists([], Acc, _Delm, binary) ->
    list_to_binary(Acc).

% @doc Trim _sup subfix from atoms like somemodule_sup
-spec trim_sup(Name) -> Result when
    Name :: atom(),
    Result :: atom().

trim_sup(Name) ->
    list_to_atom(lists:takewhile(fun(X) -> X /=$_ end, atom_to_list(Name))).

% @doc Sync posthook for autotesting while development
autotest_on_compile() ->
    ok = sync:start(),
    RunTests = fun(Mods) ->
        _ = [Mod:test() || Mod <- Mods, 
            erlang:function_exported(Mod, test, 0)
        ],
        [eunit:test(Mod) || Mod <- Mods]
    end,
    sync:onsync(RunTests).

% @doc disable lager output to console
dclog() ->
    lager:set_loglevel(lager_console_backend, critical).

% @doc enable lager output to console
eclog() ->
    GetConfig = fun() -> 
        {ok, Data} = application:get_env(lager, handlers), 
        {lager_console_backend, Value} = lists:keyfind(lager_console_backend,1,Data),
        Value
    end,

    try GetConfig() of 
        Value -> eclog(Value)
    catch _:_ ->
        eclog(info)
    end.
eclog(LogLevel) -> lager:set_loglevel(lager_console_backend, LogLevel).
