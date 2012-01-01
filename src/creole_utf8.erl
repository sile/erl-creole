-module(creole_utf8).

-export([to_string/2,
         from_string/2]).

to_string(Bytes, Fn) when is_list(Bytes) ->
    to_string(list_to_binary(Bytes), Fn);
to_string(Bytes, Fn) ->
    to_string_impl(Bytes, Fn, []).

to_string_impl(Bytes, Fn, Acc) ->
    %% XXX: <<207,2,168,226,156,144,63,100>> とかを入力にするとバグっぽい・・・
    %% => 自作した方が安全？
    case unicode:characters_to_list(Bytes, utf8) of
        Str when is_list(Str) ->
            [Str | Acc];
        {error, Str, Rest} -> 
            {S, Rest2, Continue} = Fn(Rest),
            case Continue of
                true ->
                    to_string_impl(Rest2, Fn, [S, Str | Acc]);
                false ->
                    {abort, to_string_impl([], Fn, [S, Str | Acc]), Rest2}
            end;
        {imcomplate, Str, Rest} = S ->
            {S, Rest2, Continue} = Fn(Rest),
            case Continue of
                true ->
                    to_string_impl(Rest2, Fn, [S, Str | Acc]);
                false ->
                    {abort, to_string_impl([], Fn, [S, Str | Acc]), Rest2}
            end
    end.

from_string(String, Fn) when is_list(String) ->
    binary:list_to_bin(lists:reverse(from_string_impl(String, Fn, []))).

from_string_impl(String, Fn, Acc) ->
    case unicode:characters_to_binary(String, utf8) of
        Bin when is_binary(Bin) ->
            [Bin | Acc];
        {error, Bin, Rest} ->
            {S, Rest2, Continue} = Fn(Rest),
            case Continue of
                true ->
                    from_string_impl(Rest2, Fn, [S, Bin | Acc]);
                false ->
                    {abort, from_string_impl(Rest2, Fn, [S, Bin | Acc]), Rest2}
            end
    end.
