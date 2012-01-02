-module(creole_iso_2022_jp_1).

-export([from_string/2, to_string/2]).

-define(ESC_ASCII, "\e(B").
-define(ESC_ROMAN, "\e(J").
-define(ESC_JISX_0208_1978, "\e$@").
-define(ESC_JISX_0208_1983, "\e$B").
-define(ESC_JISX_0212_1990, "\e$(D").

from_string(String, ErrFn) ->
    from_string_impl(String, ErrFn, ascii, []).

from_string_impl([], ErrFn, Mode, Acc) when Mode=/=ascii ->
    from_string_impl([], ErrFn, ascii, [?ESC_ASCII|Acc]);
from_string_impl([], _, _, Acc) ->
    binary:list_to_bin(lists:reverse(Acc));
from_string_impl([C|Rest], ErrFn, Mode, Acc) when C < 16#80 ->
    case Mode of
        ascii ->
            from_string_impl(Rest, ErrFn, ascii, [C|Acc]);
        _ ->
            from_string_impl(Rest, ErrFn, ascii, [C,?ESC_ASCII|Acc])
    end;
from_string_impl([C|Rest]=Str, ErrFn, Mode, Acc) ->
    case try_convert(C) of
        fail -> 
            {S, Rest2, Continue} = ErrFn(Str),
            S2 = case Mode of
                     ascii -> S;
                     _ -> [?ESC_ASCII, S] % XXX: It is incorrect if S isn't ascii sequence.
                 end,
            case Continue of
                true ->
                    from_string_impl(Rest2, ErrFn, ascii, [S2|Acc]);
                false ->
                    {abort, from_string_impl([], ErrFn, ascii, [S2|Acc]), Rest2}
            end;
        {jisx_0208_1983, Bytes} ->
            case Mode of
                jisx_0208_1983 ->
                    from_string_impl(Rest, ErrFn, jisx_0208_1983, [Bytes|Acc]);
                _ ->
                    from_string_impl(Rest, ErrFn, jisx_0208_1983, [Bytes,?ESC_JISX_0208_1983|Acc])
            end;
        {jisx_0212_1990, Bytes} ->
            case Mode of
                jisx_0208_1990 ->
                    from_string_impl(Rest, ErrFn, jisx_0208_1990, [Bytes|Acc]);
                _ ->
                    from_string_impl(Rest, ErrFn, jisx_0208_1990, [Bytes,?ESC_JISX_0212_1990|Acc])
            end
    end.

try_convert(Code) ->
    case creole_to_jisx_0208_1983:to_bytes(Code) of
        fail ->
            case creole_to_jisx_0212_1990:to_bytes(Code) of
                fail -> fail;
                Bytes -> {jisx_0212_1990, Bytes}
            end;
        Bytes ->
            {jisx_0208_1983, Bytes}
    end.

to_string(Bytes, ErrFn) ->
    to_string_impl(Bytes, ErrFn, ascii, []).

to_string_impl(<<>>, _, _, Acc) ->
   lists:flatten(lists:reverse(Acc));
to_string_impl(Bytes, ErrFn, Mode, Acc) ->
    case Bytes of
        %% ASCII
        <<?ESC_ASCII, Rest/binary>> ->  
            to_string_impl(Rest, ErrFn, ascii, Acc);

        %% JIS X 0201-1976 Roman Set
        <<?ESC_ROMAN, Rest/binary>> -> 
            to_string_impl(Rest, ErrFn, roman, Acc);
        
        %% JIS X 0208-1978
        <<?ESC_JISX_0208_1978, Rest/binary>> ->
            %% XXX: using jisx_0208_1983 instead of jisx_0208_1978
            to_string_impl(Rest, ErrFn, jisx_0208_1983, Acc);
        
        %% JIS X 0208-1983
        <<?ESC_JISX_0208_1983, Rest/binary>> ->  
            to_string_impl(Rest, ErrFn, jisx_0208_1983, Acc);

        %% JIS X 0212-1990
        <<?ESC_JISX_0212_1990, Rest/binary>> ->
            to_string_impl(Rest, ErrFn, jisx_0212_1990, Acc);
        
        %% Non Escape sequence
        _ ->
            to_string_impl2(Bytes, ErrFn, Mode, Acc)
    end.

to_string_impl2(<<C, Rest/binary>>=Bytes, ErrFn, ascii, Acc) ->
    case C of
        _ when C < 16#80 -> 
            to_string_impl(Rest, ErrFn, ascii, [C | Acc]);
        _ ->
            handle_error(Bytes, ErrFn, ascii, Acc)
    end;
to_string_impl2(<<C, Rest/binary>>=Bytes, ErrFn, roman, Acc) ->
    case C of
        $\ ->
            to_string_impl(Rest, ErrFn, ascii, [65509 | Acc]); % FULLWIDTH_YEN_SIGN
        $~ ->
            to_string_impl(Rest, ErrFn, ascii, [65507 | Acc]); % FULLWIDTH_MACRON
        C when C < 16#80 -> 
            to_string_impl(Rest, ErrFn, ascii, [C | Acc]);
        _ ->
            handle_error(Bytes, ErrFn, roman, Acc)
    end;
to_string_impl2(Bytes, ErrFn, Mode, Acc) ->
    ErrFn2 = fun (<<"\e", _/binary>>=Bs) ->
                     {[], Bs, false};
                 (Bs) ->
                     ErrFn(Bs)
             end,
    case creole_general:to_string(Bytes, Mode, ErrFn2) of
        {abort, PartialResult, Rest} ->
            to_string_impl(Rest, ErrFn, Mode, [PartialResult | Acc]);
        Result ->
            to_string_impl(<<>>, ErrFn, Mode, [Result | Acc])
    end.

handle_error(Bytes, ErrFn, Mode, Acc) ->
    case ErrFn(Bytes) of
        {S, Rest, true} ->
            to_string_impl(Rest, ErrFn, Mode, [S | Acc]);
        {S, Rest, false} ->
            {abort, to_string_impl(<<>>, ErrFn, Mode, [S | Acc]), Rest}
    end.
