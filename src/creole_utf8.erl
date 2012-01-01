-module(creole_utf8).

-export([to_string/2, from_string/2]).

to_string(Bytes, ErrFn) ->
    to_string_impl(Bytes, ErrFn, []).

to_string_impl(<<>>, _, Acc) ->
    lists:flatten(lists:reverse(Acc));
to_string_impl(Bytes, ErrFn, Acc) ->
    case to_unicode(Bytes) of
        fail ->
            {S, Rest, Continue} = ErrFn(Bytes),
            case Continue of
                true -> to_string_impl(Rest, ErrFn, [S|Acc]);
                false -> {abort, to_string_impl([], ErrFn, [S|Acc]), Rest}
            end;
        {Code, Rest} ->
            to_string_impl(Rest, ErrFn, [Code|Acc])
    end.

from_string(String, ErrFn) when is_list(String) ->
    from_string_impl(String, ErrFn, []).

from_string_impl([], _, Acc) ->
    binary:list_to_bin(lists:reverse(Acc));
from_string_impl(String, ErrFn, Acc) ->
    case unicode:characters_to_binary(String, utf8) of
        Bin when is_binary(Bin) ->
            from_string_impl([], ErrFn, [Bin | Acc]);
        {error, Bin, Rest} ->
            {S, Rest2, Continue} = ErrFn(Rest),
            case Continue of
                true ->
                    from_string_impl(Rest2, ErrFn, [S, Bin | Acc]);
                false ->
                    {abort, from_string_impl([], ErrFn, [S, Bin | Acc]), Rest2}
            end
    end.

to_unicode(<<C, Rest/binary>>) when C < 16#80 -> 
    {C, Rest};
to_unicode(<<2#10:2/unit:1, _:6/unit:1, _/binary>>) ->
    fail;
to_unicode(<<2#110:3/unit:1, B1:5/unit:1, 
             2#10:2/unit:1, B2:6/unit:1, Rest/binary>>) ->
    case is_redundant(B1, B2, 5) of
        true -> fail;
        false ->
            {(B1 bsl 6) + B2, Rest}
    end;
to_unicode(<<2#1110:4/unit:1, B1:4/unit:1, 
             2#10:2/unit:1, B2:6/unit:1, 
             2#10:2/unit:1, B3:6/unit:1, Rest/binary>>) ->
    case is_redundant(B1, B2, 4) of
        true -> fail;
        false ->
            {(B1 bsl 12) + (B2 bsl 6) + B3, Rest}
    end;
to_unicode(<<2#11110:5/unit:1, B1:3/unit:1, 
             2#10:2/unit:1, B2:6/unit:1, 
             2#10:2/unit:1, B3:6/unit:1, 
             2#10:2/unit:1, B4:6/unit:1, Rest/binary>>) ->
    case is_redundant(B1, B2, 3) of
        true -> fail;
        false ->
            {(B1 bsl 18) + (B2 bsl 12) + (B3 bsl 6) + B4, Rest}
    end;    
to_unicode(_) ->
    fail.

is_redundant(B1, B2, FirstBitN) ->
    B1 =:= 0 andalso B2 < (1 bsl (FirstBitN+1)).
