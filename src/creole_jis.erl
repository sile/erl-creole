-module(creole_jis).

-export([from_string/2, to_string/2]).

from_string(String, ErrFn) ->
    from_string_impl(String, ErrFn, ascii, []).

from_string_impl([], ErrFn, Mode, Acc) when Mode=/=ascii ->
    from_string_impl([], ErrFn, ascii, [<<"\e(B">>|Acc]);
from_string_impl([], _, _, Acc) ->
    binary:list_to_bin(lists:reverse(Acc));
from_string_impl([C|Rest], ErrFn, Mode, Acc) when C =< 16#FF ->
    case Mode of
        ascii ->
            from_string_impl(Rest, ErrFn, ascii, [C|Acc]);
        _ ->
            from_string_impl(Rest, ErrFn, ascii, [C,<<"\e(b">>|Acc])
    end;
from_string_impl([C|Rest]=Str, ErrFn, Mode, Acc) ->
    case {creole_to_jisx_0208_1983:to_bytes(C), Mode} of
        {fail, _} -> 
            {S, Rest2, Continue} = ErrFn(Str),
            S2 = case Mode of
                     ascii -> S;
                     _ -> [<<"\e(b">>|S] % XXX: It is incorrect if S isn't ascii sequence.
                 end,
            case Continue of
                true ->
                    from_string_impl(Rest2, ErrFn, ascii, [S2|Acc]);
                false ->
                    {abort, from_string_impl([], ErrFn, ascii, [S2|Acc]), Rest2}
            end;
        {Bytes, jisx_0208_1983} ->
            from_string_impl(Rest, ErrFn, jisx_0208_1983, [Bytes|Acc]);
        {Bytes, _} ->
            from_string_impl(Rest, ErrFn, jisx_0208_1983, [Bytes,<<"\e$B">>|Acc])
    end.

to_string(Bytes, ErrFn) ->
    to_string_impl(Bytes, ErrFn, ascii, []).

to_string_impl(<<>>, _, _, Acc) ->
   lists:flatten(lists:reverse(Acc));
to_string_impl(Bytes, ErrFn, Mode, Acc) ->
    case Bytes of
        %% ASCII
        <<"\e(B", Rest/binary>> ->  
            to_string_impl(Rest, ErrFn, ascii, Acc);

        %% JIS X 0201-1976 Roman Set
        <<"\e(J", Rest/binary>> -> 
            to_string_impl(Rest, ErrFn, roman, Acc);
        
        %% JIS X 0208-1978
        <<"\e$@", Rest/binary>> ->
            %% XXX: using jisx_0208_1983 instead of jisx_0208_1978
            to_string_impl(Rest, ErrFn, jisx_0208_1983, Acc);
        
        %% JIS X 0208-1983
        <<"\e$B", Rest/binary>> ->  
            to_string_impl(Rest, ErrFn, jisx_0208_1983, Acc);

        %% Non Escape sequence
        _ ->
            to_string_impl2(Bytes, ErrFn, Mode, Acc)
    end.

to_string_impl2(<<B:1/binary, Rest/binary>>=Bytes, ErrFn, ascii, Acc) ->
    case binary:first(B) of
        C when C =< 16#FF -> 
            to_string_impl(Rest, ErrFn, ascii, [C | Acc]);
        _ ->
            handle_error(Bytes, ErrFn, ascii, Acc)
    end;
to_string_impl2(<<B:1/binary, Rest/binary>>=Bytes, ErrFn, roman, Acc) ->
    case binary:first(B) of
        $\ ->
            to_string_impl(Rest, ErrFn, ascii, [65509 | Acc]); % FULLWIDTH_YEN_SIGN
        $~ ->
            to_string_impl(Rest, ErrFn, ascii, [65507 | Acc]); % FULLWIDTH_MACRON
        C when C =< 16#FF -> 
            to_string_impl(Rest, ErrFn, ascii, [C | Acc]);
        _ ->
            handle_error(Bytes, ErrFn, roman, Acc)
    end;
to_string_impl2(Bytes, ErrFn, jisx_0208_1983, Acc) ->
    ErrFn2 = fun (<<"\e", _/binary>>=Bs) ->
                     {[], Bs, false};
                 (Bs) ->
                     ErrFn(Bs)
             end,
    case creole_general:to_string(Bytes, jisx_0208_1983, ErrFn2) of
        {abort, PartialResult, Rest} ->
            to_string_impl(Rest, ErrFn, jisx_0208_1983, [PartialResult | Acc]);
        Result ->
            to_string_impl(<<>>, ErrFn, jisx_0208_1983, [Result | Acc])
    end.

handle_error(Bytes, ErrFn, Mode, Acc) ->
    case ErrFn(Bytes) of
        {S, Rest, true} ->
            to_string_impl(Rest, ErrFn, Mode, [S | Acc]);
        {S, Rest, false} ->
            {abort, to_string_impl(<<>>, ErrFn, Mode, [S | Acc]), Rest}
    end.
