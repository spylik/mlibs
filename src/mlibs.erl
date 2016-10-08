-module(mlibs).

-compile(export_all).

-type id() :: {neg_integer(), pos_integer()}.

-define(epoch, 62167219200).

-export_type([
    id/0
    ]).

% @doc Get current system time in milli_seconds (unix timestamp in milliseconds)
-spec get_time() -> Result when
    Result      :: integer() | 'milli_seconds'.

get_time() ->
    erlang:system_time(milli_seconds).

% @doc Convert http request_date to mlibs:get_time/0 format (unix timestamp in millisecond)
-spec http_time_to_unix_time_ms(DateTime) -> Result when
    DateTime    :: nonempty_list() | binary(),
    Result      :: integer() | 'milli_seconds'.

http_time_to_unix_time_ms(DateTime) when is_binary(DateTime) ->
    http_time_to_unix_time_ms(binary_to_list(DateTime));

http_time_to_unix_time_ms(DateTime) when is_list(DateTime) ->
    erlang:convert_time_unit(
        calendar:datetime_to_gregorian_seconds(httpd_util:convert_request_date(DateTime)) - ?epoch,
        seconds, milli_seconds).

% @doc Convert unixtimestamp to mlibs:get_time/0 format (unix timestamp in millisecond)
-spec unixtimestamp_to_ms(DateTime) -> Result when
    DateTime    :: integer() | binary() | 'seconds',
    Result      :: integer() | 'milli_seconds'.

unixtimestamp_to_ms(DateTime) when is_binary(DateTime) ->
    unixtimestamp_to_ms(binary_to_integer(DateTime));
unixtimestamp_to_ms(DateTime) when is_integer(DateTime) ->
    erlang:convert_time_unit(DateTime, seconds, milli_seconds).

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

etests() -> autotest_on_compile().
dtests() -> sync:onsync(undefined).

% @doc discover modules and tests
discover() ->
    [case filelib:ensure_dir(Dir) of
        ok ->
            lists:map(fun(Module) ->
                    code:ensure_loaded(list_to_atom(lists:takewhile(fun(X) -> X /= $. end, lists:subtract(Module,Dir))))
                end,
                filelib:wildcard(Dir++"*.erl")
            );
        _ -> ok
    end || Dir <- ["src/","test/"]].

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
        Value -> elog(Value)
    catch _:_ ->
        elog(info)
    end.
elog(LogLevel) -> lager:set_loglevel(lager_console_backend, LogLevel).
