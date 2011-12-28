-module(creole).

-export([from_string/2, from_string/3,
         to_string/2, to_string/3,
         replace/1]).

from_string(String, Encoding) ->
    from_string(String, Encoding, replace($?)).

from_string(String, Encoding, ErrFn) ->
    case Encoding of
        utf8 -> 
            creole_utf8:from_string(String, ErrFn);
        jis -> % TODO
            creole_jis:from_string(String, ErrFn);
        _ ->
            creole_general:from_string(String, Encoding, ErrFn)
    end.

to_string(Bytes, Encoding) ->
    to_string(Bytes, Encoding, replace($?)).

to_string(Bytes, Encoding, ErrFn) when is_list(Bytes) ->
    to_string(list_to_binary(Bytes), Encoding, ErrFn);
to_string(Bytes, Encoding, ErrFn) ->
    case Encoding of
        utf8 -> 
            creole_utf8:to_string(Bytes, ErrFn);
        jis -> % TODO
            creole_jis:to_string(Bytes, ErrFn);
        _ ->
            creole_general:to_string(Bytes, Encoding, ErrFn)
    end.

replace(Char) ->
    fun (<<_:1/binary, Rest/binary>>) ->
            {Char, Rest};
        ([_|Rest]) ->
            {Char, Rest}
    end.
