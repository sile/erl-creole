-module(creole_general).

-export([to_string/3,
         from_string/3]).

from_string(String, Encoding, ErrFn) ->
    ToBytes = case Encoding of
                  cp932 -> fun creole_to_cp932:to_bytes/1
              end,

    from_string_impl(String, ToBytes, ErrFn, []).

from_string_impl([], _, _, Acc) ->
    binary:list_to_bin(lists:reverse(Acc));
from_string_impl([Code|Rest]=String, ToBytes, ErrFn, Acc) ->
    case ToBytes(Code) of
        fail -> 
            {S, Rest2} = ErrFn(String),
            from_string_impl(Rest2, ToBytes, ErrFn, [S | Acc]);
        Bytes ->
            from_string_impl(Rest, ToBytes, ErrFn, [Bytes | Acc])
    end.


to_string(Bytes, Encoding, ErrFn) ->
    Nodes = case Encoding of
                cp932 -> creole_from_cp932:da_nodes()
            end,
    to_string_impl(Bytes, Nodes, ErrFn, []).

to_string_impl(<<>>, _, _, Acc) ->
    lists:flatten(lists:reverse(Acc));
to_string_impl(<<0:8/binary, Rest/binary>>, Nodes, ErrFn, Acc) ->
    to_string_impl(Rest, Nodes, ErrFn, [0|Acc]);
to_string_impl(Bytes, Nodes, ErrFn, Acc) ->
    case to_unicode(Bytes, Nodes, 0) of
        fail ->
            {S, Rest} = ErrFn(Bytes),
            to_string_impl(Rest, Nodes, ErrFn, [S | Acc]);
        {Unicode, Rest} ->
            to_string_impl(Rest, Nodes, ErrFn, [Unicode | Acc])
    end.

to_unicode(<<>>, _, _) ->
    fail;
to_unicode(<<Front:1/binary,Bytes/binary>>, Nodes, NodeIndex) ->
    Arc = binary:first(Front),
    NextIndex = base(Nodes, NodeIndex) + Arc,
    case chck(Nodes,NextIndex) of
        Arc ->
            case chck(Nodes,base(Nodes,NextIndex)) of
                0 ->
                    {value(Nodes, base(Nodes,NextIndex)), Bytes};
                _ ->
                    to_unicode(Bytes, Nodes, NextIndex)
            end;
        _ ->
            fail
    end.
            
base(Nodes, Index) ->
    element(Index+1, Nodes) band 16#00FFFFFF.

chck(Nodes, Index) ->
    (element(Index+1, Nodes) band 16#FF000000) bsr 24.

value(Nodes, Index) ->
    element(Index+1, Nodes) band 16#00FFFFFF.
