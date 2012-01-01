-module(creole_jis).

-export([from_string/2, to_string/2]).

from_string(_String, _ErrFn) ->
    ok.

to_string(String, ErrFn) ->
    to_string_impl(String, ErrFn, ascii, []).

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
