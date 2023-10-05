-module(mlibs).

-compile(export_all).
-compile(nowarn_export_all).

-include("utils.hrl").

-define(epoch, 62167219200).
-define(biggest_bigint, 9223372036854775808).
-define(lowest_bigint, -?biggest_bigint).
-define(mtime_unit, nanosecond).

-type id_type()     :: strict | unstrict.
-type strict_id()   :: {integer(), integer(), node()}.
-type unstrict_id() :: {integer(), integer()}.

-type id()          :: unstrict_id().

-type mtime()   :: 1696387175273909454..?biggest_bigint.

-type time_pointer() :: {erlang:time_unit(), pos_integer()}.

-export_type([
    id/0,
    strict_id/0,
    unstrict_id/0,
    mtime/0,
    time_pointer/0
    ]).

-spec mtime_unit() -> Result when
    Result  :: erlang:time_unit().

mtime_unit() -> ?mtime_unit.

-spec convert_time_pointer(TimePointer, TargetUnit) -> Result when
    TimePointer :: time_pointer(),
    TargetUnit  :: erlang:time_unit(),
    Result      :: pos_integer().

convert_time_pointer({TimeUnit, Time}, TargetUnit) ->
    erlang:convert_time_unit(Time, TimeUnit, TargetUnit).

-spec biggest_bigint() -> Result when
      Result :: pos_integer().

biggest_bigint() -> ?biggest_bigint.

-spec lowest_bigint() -> Result when
      Result :: neg_integer().

lowest_bigint() -> ?lowest_bigint.

% @doc Get current system POSIX time in nanoseconds (unix timestamp in nanoseconds)
% the POSIX means it's since ?epoch and in GMT
-spec get_time() -> Result when
    Result      :: mtime().

get_time() -> erlang:system_time(?mtime_unit).

% @doc Convert http request_date to mlibs:get_time/0 format (unix timestamp in nanoseconds)
-spec http_time_to_mtime(DateTime) -> Result when
    DateTime    :: nonempty_list() | binary(),
    Result      :: mtime().

http_time_to_mtime(DateTime) when is_binary(DateTime) ->
    http_time_to_mtime(binary_to_list(DateTime));

http_time_to_mtime(DateTime) when is_list(DateTime) ->
    erlang:convert_time_unit(
        calendar:datetime_to_gregorian_seconds(httpd_util:convert_request_date(DateTime)) - ?epoch,
        second, ?mtime_unit
    ).

% @doc
% Time <<"2023-10-04 03:30:00">> to nanoseconds 1696390200000000000
% Time <<"2023-10-04 03:30:00.000000">> to nanoseconds 1696390200000000000
-spec datetime_to_mtime(DateTime) -> Result when
    DateTime    :: list() | binary(),
    Result      :: mtime().

datetime_to_mtime(<<
    Y:4/binary,
    $-,
    M:2/binary,
    $-,
    D:2/binary,
    " ",
    Hrs:2/binary,
    $:,
    Min:2/binary,
    $:,
    Sec:2/binary
    >>
) -> erlang:convert_time_unit(
        calendar:datetime_to_gregorian_seconds(
            {
                {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)},
                {binary_to_integer(Hrs), binary_to_integer(Min), binary_to_integer(Sec)}
            }
        ) - ?epoch,
        second, ?mtime_unit
    );
datetime_to_mtime(<<
    Y:4/binary,
    $-,
    M:2/binary,
    $-,
    D:2/binary,
    " ",
    Hrs:2/binary,
    $:,
    Min:2/binary,
    $:,
    Sec:2/binary,
    $.,
    MicroSeconds:6/binary
    >>
) -> erlang:convert_time_unit(
        calendar:datetime_to_gregorian_seconds(
            {
                {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)},
                {binary_to_integer(Hrs), binary_to_integer(Min), binary_to_integer(Sec)}
            }
        ) - ?epoch,
        second, ?mtime_unit
    ) + binary_to_integer(MicroSeconds) * 1000;

datetime_to_mtime(List) when is_list(List) -> datetime_to_mtime(erlang:iolist_to_binary(List)).


% @doc Convert unixtimestamp to mlibs:get_time/0 format (unix timestamp in nanoseconds)
-spec unixtimestamp_to_mtime(DateTime) -> Result when
    DateTime    :: integer() | binary(),
    Result      :: mtime().

unixtimestamp_to_mtime(DateTime) when is_binary(DateTime) ->
    unixtimestamp_to_mtime(binary_to_integer(DateTime));
unixtimestamp_to_mtime(DateTime) when is_integer(DateTime) ->
    erlang:convert_time_unit(DateTime, second, ?mtime_unit).

% @doc Convert unixtimestamp to mlibs:get_time/0 format (unix timestamp micro in nanoseconds)
-spec unixtimestamp_micro_to_mtime(DateTime) -> Result when
    DateTime    :: integer() | binary(),
    Result      :: mtime().

unixtimestamp_micro_to_mtime(DateTime) when is_binary(DateTime) ->
    unixtimestamp_micro_to_mtime(binary_to_integer(DateTime));
unixtimestamp_micro_to_mtime(DateTime) when is_integer(DateTime) ->
    erlang:convert_time_unit(DateTime, microsecond, ?mtime_unit).

-spec id_function(IdType) -> Result when
    IdType  :: id_type(),
    Result  :: atom().

id_function(strict) -> gen_strict_id;
id_function(unstrict) -> gen_id.

-spec gen_id() -> Result when
    Result      :: id().

gen_id() -> gen_unstrict_id().

% @doc Convert given time unit to mtime unit
-spec to_mtime({TimeUnit, Time}) -> Result when
    TimeUnit    :: erlang:time_unit(),
    Time        :: pos_integer(),
    Result      :: mtime().

to_mtime({OriginalUnit, Time}) ->
    erlang:convert_time_unit(Time, OriginalUnit, ?mtime_unit).

% @doc Generate unique id
-spec gen_unstrict_id() -> Result when
    Result      :: unstrict_id().

gen_unstrict_id() -> {erlang:monotonic_time(?mtime_unit), erlang:unique_integer([monotonic])}.

% @doc the interface for generate strict id (when we really need it. it's more expensive).
-spec gen_strict_id() -> Result when
      Result      :: strict_id().

gen_strict_id() ->
    {erlang:monotonic_time(?mtime_unit), erlang:unique_integer([monotonic]), node()}.

% @doc convert id to mtime

-spec id_to_mtime(IdOrStrictId) -> Result when
    IdOrStrictId    :: unstrict_id() | strict_id(),
    Result          :: mtime().

id_to_mtime({MonoTime, UniqueInteger, _Node}) -> id_to_mtime({MonoTime, UniqueInteger});
id_to_mtime({MonoTime, _UniqueInteger}) -> MonoTime + erlang:time_offset(?mtime_unit).

% @doc
% Because for different scenarious we may require just id() but for some strict_id() we need to generate
% proper function name which we will use for prev/next.
% By default we generating for "next" as it's most common scenario when we traversing events from
% the past to the future
% @end

-spec ms_pattern_function(SampleOfIdFromTableOrIdType) -> Result when
    SampleOfIdFromTableOrIdType :: unstrict_id() | strict_id() | id_type(),
    Result :: atom().

ms_pattern_function(SampleOfIdFromTableOrIdType) -> ms_pattern_function(SampleOfIdFromTableOrIdType, next).

-spec ms_pattern_function(SampleOfIdFromTableOrIdType, DirectionType) -> Result when
    SampleOfIdFromTableOrIdType :: unstrict_id() | strict_id() | id_type(),
    DirectionType :: next | prev,
    Result :: atom().

% @doc
% Sometimes during traversing tables we will want to include that timestamp into result and sometimes exclude (equal_to
% or greater_than / smaller_than).
% By default we including.
% @end

ms_pattern_function(SampleOfIdFromTableOrIdType, DirectionType) -> ms_pattern_function(SampleOfIdFromTableOrIdType, DirectionType, true).

-spec ms_pattern_function(SampleOfIdFromTableOrIdType, DirectionType, Included) -> Result when
    SampleOfIdFromTableOrIdType :: unstrict_id() | strict_id() | id_type(),
    DirectionType :: next | prev,
    Included      :: boolean(),
    Result :: atom().

ms_pattern_function(strict, next, true) -> mtime_to_strict_id_next_pattern;
ms_pattern_function(unstrict, next, true) -> mtime_to_unstrict_id_next_pattern;
ms_pattern_function({_MonoTime, _UniqueInteger, _Node}, next, true) -> mtime_to_strict_id_next_pattern;
ms_pattern_function({_MonoTime, _Node}, next, true) -> mtime_to_unstrict_id_next_pattern;

ms_pattern_function(strict, next, false) -> mtime_to_strict_id_prev_pattern;
ms_pattern_function(unstrict, next, false) -> mtime_to_unstrict_id_prev_pattern;
ms_pattern_function({_MonoTime, _UniqueInteger, _Node}, next, false) -> mtime_to_strict_id_prev_pattern;
ms_pattern_function({_MonoTime, _Node}, next, false) -> mtime_to_unstrict_id_prev_pattern;

ms_pattern_function(strict, prev, true) -> mtime_to_strict_id_prev_pattern;
ms_pattern_function(unstrict, prev, true) -> mtime_to_unstrict_id_prev_pattern;
ms_pattern_function({_MonoTime, _UniqueInteger, _Node}, prev, true) -> mtime_to_strict_id_prev_pattern;
ms_pattern_function({_MonoTime, _Node}, prev, true) -> mtime_to_unstrict_id_prev_pattern;

ms_pattern_function(strict, prev, false) -> mtime_to_strict_id_next_pattern;
ms_pattern_function(unstrict, prev, false) -> mtime_to_unstrict_id_next_pattern;
ms_pattern_function({_MonoTime, _UniqueInteger, _Node}, prev, false) -> mtime_to_strict_id_next_pattern;
ms_pattern_function({_MonoTime, _Node}, prev, false) -> mtime_to_unstrict_id_next_pattern.


% @doc Generate MatchSpec pattern for unstrict id (NEXT).
-spec mtime_to_unstrict_id_next_pattern(MTime) -> Result when
    MTime   :: mtime(),
    Result  :: unstrict_id().

mtime_to_unstrict_id_next_pattern(MTime) -> {mtime_to_monotonic(MTime), ?lowest_bigint}.

% @doc Generate MatchSpec pattern for strict id (NEXT).
-spec mtime_to_strict_id_next_pattern(MTime) -> Result when
    MTime   :: mtime(),
    Result  :: strict_id().

mtime_to_strict_id_next_pattern(MTime) ->
    {mtime_to_monotonic(MTime), ?lowest_bigint, node()}.

% ==== prev ===

% @doc Generate MatchSpec pattern for unstrict id (PREV).
-spec mtime_to_unstrict_id_prev_pattern(MTime) -> Result when
    MTime   :: mtime(),
    Result  :: unstrict_id().

mtime_to_unstrict_id_prev_pattern(MTime) -> {mtime_to_monotonic(MTime), ?biggest_bigint}.

% @doc Generate MatchSpec pattern for strict id (PREV).
-spec mtime_to_strict_id_prev_pattern(MTime) -> Result when
    MTime   :: mtime(),
    Result  :: strict_id().

mtime_to_strict_id_prev_pattern(MTime) -> {MTime, ?biggest_bigint, node()}.

mtime_to_monotonic(MTime) -> MTime - erlang:time_offset(?mtime_unit).

% @doc Generate random atom
-spec random_atom() -> Result when
    Result  :: atom().

random_atom() ->
    list_to_atom(erlang:ref_to_list(make_ref())).

-spec random_atom(Length) -> Result when
    Length  :: pos_integer(),
    Result  :: atom().

random_atom(Length) ->
    list_to_atom(random_string(Length, "abcdefghijklmnopqrstuvwxyz1234567890")).

-spec random_string(Length, AllowedChars) -> Result when
    Length          :: pos_integer(),
    AllowedChars    :: list(),
    Result          :: list().

random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(rand:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).

% @doc wait_for
wait_for(Msg) ->
    wait_for(Msg, 1000).

wait_for(Msg, Timeout) ->
    ?info(Msg),
    timer:sleep(Timeout).

wait_for(Function, Arguments, Expectation, Timeout, MaxAttempts, ReturnInCaseOfFail) when MaxAttempts > 0 ->
    case erlang:apply(Function, Arguments) of
        Expectation ->
            Expectation;
        _Other ->
            timer:sleep(Timeout),
            wait_for(Function, Arguments, Expectation, Timeout, MaxAttempts-1, ReturnInCaseOfFail)
    end;
wait_for(_Function, _Arguments, _Expectation, _Timeout, _MaxAttempts, ReturnInCaseOfFail) -> ReturnInCaseOfFail.

% @doc Generate password (by erlangcentral)
-spec generate_password(Number) -> Password when
    Number      :: pos_integer(),
    Password    :: list().

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
build_key_lists([H|T],[], Delm, Type) when is_number(H) ->
    build_key_lists(T, io_lib:write(H), Delm, Type);
build_key_lists([H|T],[], Delm, Type) when is_list(H) ->
    build_key_lists(T, H, Delm, Type);
build_key_lists([H|T],[], Delm, Type) when is_binary(H) ->
    build_key_lists(T, binary_to_list(H), Delm, Type);
build_key_lists([H|T],[], Delm, Type) when is_tuple(H) ->
    build_key_lists(T, io_lib:write(H), Delm, Type);
build_key_lists([H|T],Acc, Delm, Type) when is_atom(H) ->
    build_key_lists(T, lists:concat([Acc, Delm, atom_to_list(H)]), Delm, Type);
build_key_lists([H|T],Acc, Delm, Type) when is_number(H) ->
    build_key_lists(T, lists:concat([Acc, Delm, io_lib:write(H)]), Delm, Type);
build_key_lists([H|T],Acc, Delm, Type) when is_list(H) ->
    build_key_lists(T, lists:concat([Acc, Delm, H]), Delm, Type);
build_key_lists([H|T],Acc, Delm, Type) when is_binary(H) ->
    build_key_lists(T, lists:concat([Acc, Delm, binary_to_list(H)]), Delm, Type);
build_key_lists([H|T],Acc, Delm, Type) when is_tuple(H) ->
    build_key_lists(T, lists:concat([Acc, Delm, io_lib:write(H)]), Delm, Type);

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
        [eunit:test(Mod, [verbose, {report,{eunit_surefire,[{dir,"./log"}]}}]) || Mod <- Mods]
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

% @doc http://stackoverflow.com/questions/13480462/erlang-can-i-get-a-list-of-all-currently-registered-atoms
atom_by_number(N) ->
    binary_to_term(<<131,75,N:24>>).

all_atoms() ->
    atoms_starting_at(0).

atoms_starting_at(N) ->
    try atom_by_number(N) of
        Atom ->
            [Atom] ++ atoms_starting_at(N + 1)
    catch
        error:badarg ->
            []
    end.

hexlify(Bin) when is_binary(Bin) ->
    << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.

