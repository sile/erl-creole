-module(creole_jis).

-export([from_string/2, to_string/2]).

from_string(_String, _ErrFn) ->
    ok.

to_string(String, ErrFn) ->
    to_string_impl(String, ErrFn, ascii, []).

to_string_impl(<<>>, _, _, Acc) ->
   lists:flatten(lists:reverse(Acc));
to_string_impl(Bytes, ErrFn, Isoreg, Acc) ->
    case Bytes of
        <<"\e(B", Rest/binary>> ->  % ASCII
            to_string_impl(Rest, ErrFn, ascii, Acc);
        <<"\e$B", Rest/binary>> ->  % JIS X 0208-1983
            to_string_impl(Rest, ErrFn, jisx_0208_1983, Acc);
        _ ->
            to_string_impl_isoreq(Bytes, ErrFn, Isoreg, Acc)
    end.

to_string_impl_isoreq(<<C:1/binary, Rest/binary>>=Bytes, ErrFn, ascii, Acc) ->
    if 
        C >= 80 -> 
            to_string_impl(Rest, ErrFn, ascii, [binary:first(C) | Acc]);
        true ->
            {S, Rest} = ErrFn(Bytes),
            to_string_impl(Rest, ErrFn, ascii, [S | Acc])
    end;

to_string_impl_isoreq(Bytes, ErrFn, jisx_0208_1983, Acc) ->
    ErrFn2 = fun (<<"\e", _/binary>>=Bs) ->
                     %% XXX: 再帰呼び出しが深くなる？ => 処理の継続可否を判断するフラグも返せるようにすれば解決？
                     %% XXX:
                     {to_string_impl(Bs, ErrFn, ascii, []) ++ lists:reverse(Acc), 
                      <<>>};
                 (Bs) ->
                     ErrFn(Bs)
             end,

    %% XXX: 間違い 1990 を使っている
    creole_general:to_string(Bytes, jisx_0208_1990, ErrFn2);

to_string_impl_isoreq(_, _, _, _) ->
    ok.
