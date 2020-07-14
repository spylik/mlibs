-module(something).

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
-endif.


-define(PRECISION_VALUE, 8).
-define(PRECISION,  [{decimals, ?PRECISION_VALUE}, compact]).
-define(PRECISION(Value), [{decimals, Value}, compact]).

-export([
    to_float/1,
    to_float/2,
    to_positive_float/1,

    to_integer/1,
    to_positive_integer/1,

    to_binary/1,

    to_boolean/1
    ]).

-include("utils.hrl").

%======== convert any numbers to float with precision  =========
% @doc convert convertable integers, binaries, lists to floats.
% it works with precision 8.
% For support precision we added some overhead : we converting float to lists
% with ?PRECISION option and then back to floats.
-spec to_float(Number) -> Result
    when
        Number :: float() | integer() | nonempty_list() | binary(),
        Result :: float().


to_float(Number) -> to_float(Number, ?PRECISION_VALUE).

to_float(Number, Precision) when is_float(Number) -> to_float(float_to_list(Number, ?PRECISION(Precision)));
to_float(Number, _Precision) when is_integer(Number) -> float(Number);
to_float(Number, Precision) when is_list(Number) ->
    case lists:member($.,Number) of
        true ->
            list_to_float(
                float_to_list(
                    try list_to_float(Number)
                    catch
                        _:_ -> list_to_float(before_sign(Number))
                    end,
                    ?PRECISION(Precision)
                )
            );
        false ->
            try list_to_integer(Number) of
                Num -> to_float(Num)
            catch
                _:_ ->
                    list_to_float(
                        float_to_list(
                            list_to_float(before_sign(Number)),
                            ?PRECISION(Precision)
                        )
                    )
            end


    end;
to_float(Number, Precision) when is_binary(Number) -> to_float(binary_to_list(Number), Precision).

% Following piece of code proposed by Richard A. O'Keefe in erlang-questions mailing
% list to deal with C, Ada, Smalltalk and some Fortran format of float numbers.
% The general problem is that Erlang more pedantic about compact floats representation
% and can convert only "1.0e-8" format but cannot convert things like "1e-8".
%
% Link to the thread: http://erlang.org/pipermail/erlang-questions/2017-February/091614.html
before_sign("+" ++ Chars) ->
    after_sign(Chars);
before_sign("-" ++ Chars) ->
    "-" ++ after_sign(Chars);
before_sign(Chars) ->
    after_sign(Chars).

after_sign([Char|Chars]) when
        Char >= $0,
        Char =< $9 ->
    [Char|after_digit(Chars)];
after_sign("." ++ Chars) ->
    "0." ++ after_dot(Chars).

after_digit([Char|Chars]) when
        Char >= $0,
        Char =< $9 ->
    [Char|after_digit(Chars)];
after_digit("_" ++ Chars) ->
    after_digit(Chars);
after_digit("." ++ Chars) ->
    "." ++ after_dot(Chars);
after_digit(Chars) ->
    ".0" ++ after_fraction(Chars).

after_dot([Char|Chars]) when
        Char >= $0,
        Char =< $9 ->
    [Char|after_fraction(Chars)];
after_dot(Chars) ->
    "0" ++ after_fraction(Chars).

after_fraction([Char|Chars]) when
        Char >= $0,
        Char =< $9 ->
    [Char | after_fraction(Chars)];
after_fraction("_" ++ Chars) ->
    after_fraction(Chars);
after_fraction([Char|Chars]) when
        Char =:= $e;
        Char =:= $d;
        Char =:= $q;
        Char =:= $E;
        Char =:= $D;
        Char =:= $Q ->
    "e" ++ after_e(Chars);

after_fraction(Chars) ->
    "e0" ++ after_e_digits(Chars).

after_e("+" ++ Chars) ->
    after_e_sign(Chars);
after_e("-" ++ Chars) ->
    "-" ++ after_e_sign(Chars);
after_e(Chars) ->
    after_e_sign(Chars).

after_e_sign([Char|Chars]) when
        Char >= $0,
        Char =< $9 ->
    [Char|after_e_digits(Chars)].

after_e_digits([Char|Chars]) when
        Char >= $0,
        Char =< $9 ->
    [Char|after_e_digits(Chars)];
after_e_digits("_" ++ Chars) ->
    after_e_digits(Chars);
after_e_digits([Char]) when
        Char =:= $f ;
        Char =:= $l ;
        Char =:= $F ;
        Char =:= $L ->
    "";
after_e_digits([]) ->
    "".
%------- end of Richard A. O'Keefe proposal for floats -------

% @doc convert convertable positive integers, binaries, lists to positive floats.
% if can't convert due negative value it throw error cant_convert_to_positive_float
-spec to_positive_float(Number) -> Result
    when
        Number :: float() | integer() | nonempty_list() | binary(),
        Result :: float().

to_positive_float(Number) ->
    case to_float(Number) of
        Num when Num > 0 -> Num;
        Num when Num == 0 -> 0.0;
        _ -> throw({error, {cant_convert_to_positive_float, Number}})
    end.

%------------- end of convert any numbers to float -------------

%=============== convert any numbers to integers ===============
% @doc convert convertable floats, binaries, lists to integer.
% Attention: it works with [{decimals,8}, compact] precision parameter for floats.
% It means it will silent convert float 123.000000001 to 123 integer but throw error
% while try convert 123.00001.
-spec to_integer(Number) -> Result
    when
        Number :: integer() | float() | nonempty_list() | binary(),
        Result :: integer().

to_integer(Number) when is_integer(Number) -> Number;
to_integer(Number) when is_float(Number) -> to_integer(float_to_list(Number, ?PRECISION));
to_integer(Number) when is_list(Number) ->
    case catch list_to_integer(Number) of
        {_, _} ->
            case string:to_integer(Number) of
                {SI, ".0"} -> SI;
                {SI, ".00"} -> SI;
                {SI, ".000"} -> SI;
                {SI, ".0000"} -> SI;
                {SI, ".00000"} -> SI;
                {SI, ".000000"} -> SI;
                {SI, ".0000000"} -> SI;
                {SI, ".00000000"} -> SI;
                {SI, [46|Sublist]} ->
                    case lists:sublist(Sublist,8) of
                        "00000000" -> SI;
                        _ -> throw({error, {cant_convert_to_integer, Number}})
                    end
            end;
        Result -> Result
    end;
to_integer(Number) when is_binary(Number) ->
    case catch erlang:binary_to_integer(Number) of
        {_, _} -> to_integer(erlang:binary_to_float(Number));
        Result -> Result
    end.

% @doc convert convertable positive floats, binaries, lists to positive integer.
% if can't convert due negative value it throw error cant_convert_to_positive_integer
% if can't convert about some wrong type, it throw error cant_convert_to_integer
-spec to_positive_integer(Number) -> Result
    when
        Number :: integer() | float() | nonempty_list() | binary(),
        Result :: non_neg_integer().

to_positive_integer(Number) ->
    case to_integer(Number) of
        Num when Num > 0 -> Num;
        Num when Num == 0 -> 0;
        _ -> throw({error, {cant_convert_to_positive_integer, Number}})
    end.

%----------- end of convert any numbers to integers ------------

%================ convert anything to binary ===================
% @doc convert convertable floats, integers, atoms, lists and terms to binaries.
% Attention: it works with [{decimals,8}, compact] precision parameter for floats.
% It means it will convert floats 123.123000 to <<"123.123">> and strip the remaining zeros.
-spec to_binary(Data) -> Result
    when
        Data :: binary() | atom() | nonempty_list() | integer() | float() | term(),
        Result :: binary().

to_binary(Data) when is_binary(Data) -> Data;
to_binary(Data) when is_atom(Data) -> erlang:atom_to_binary(Data, utf8);
to_binary(Data) when is_list(Data) ->
    try erlang:list_to_binary(Data) of
        Value -> Value
    catch
        _:_ ->
            lists:foldr(fun(Value, Acc) -> X = to_binary(Value), <<X/binary, Acc/binary>> end, <<>>, Data)
    end;
to_binary(Data) when is_integer(Data) -> erlang:integer_to_binary(Data);
to_binary(Data) when is_float(Data) -> erlang:float_to_binary(Data, ?PRECISION);
to_binary(Data) when is_tuple(Data) -> to_binary(lists:flatten(io_lib:format("~p", [Data])));
to_binary(Data) -> erlang:term_to_binary(Data).

%-------------- end of convert anything to binary --------------

%================ convert anything to boolean ==================
% @doc converts convertable floats, integers, atoms, lists, binaries to boolean.
% it can convert <<"1">>/1/"1"/true to atom true, <<"0">>/0/"0"/false to atom false, etc.
-spec to_boolean(Data) -> Result
    when
        Data :: binary() | nonempty_list() | integer() | float() | 'true' | 'false',
        Result :: 'true' | 'false'.

to_boolean(<<"1">>) -> true;
to_boolean(<<"1.0">>) -> true;
to_boolean("1") -> true;
to_boolean("1.0") -> true;
to_boolean(1) -> true;
to_boolean(1.0) -> true;
to_boolean(true) -> true;

to_boolean(<<"0">>) -> false;
to_boolean(<<"0.0">>) -> false;
to_boolean("0") -> false;
to_boolean("0.0") -> false;
to_boolean(0) -> false;
to_boolean(0.0) -> false;
to_boolean(false) -> false.

%-------------- end of convert anything to boolean--------------
