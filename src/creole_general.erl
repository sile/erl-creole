-module(creole_general).

-export([to_unicode_string/2]).

to_unicode_string(Octets, Encoding) ->
    Nodes = case Encoding of
                cp932 -> creole_from_cp932:da_nodes()
            end,
    to_unicode_string_impl(Octets, Nodes, []).

to_unicode_string_impl([], _, Acc) -> lists:reverse(Acc);
to_unicode_string_impl(Octets, Nodes, Acc) ->
    {Unicode, Rest} = to_unicode(Octets, Nodes, 0),
    to_unicode_string_impl(Rest, Nodes, [Unicode|Acc]).

to_unicode([Arc|Octets], Nodes, NodeIndex) ->   
    NextIndex = base(Nodes,NodeIndex)+Arc,
    
    case chck(Nodes,NextIndex) of
        Arc -> 
            case chck(Nodes,base(Nodes,NextIndex)) of
                0 ->
                    {value(Nodes, base(Nodes,NextIndex)), Octets};
                _ ->
                    to_unicode(Octets, Nodes, NextIndex)
            end;
        _ ->
            %% invalid sequence
            {$?, Octets}
    end.
            

base(Nodes, Index) ->
    element(Index+1, Nodes) band 16#00FFFFFF.

chck(Nodes, Index) ->
    (element(Index+1, Nodes) band 16#FF000000) bsr 24.

value(Nodes, Index) ->
    element(Index+1, Nodes) band 16#00FFFFFF.
