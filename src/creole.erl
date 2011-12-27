-module(creole).

-export([to_unicode_string/2,
         from_unicode_string/2]).

to_unicode_string(Bytes, utf8) ->
    unicode:characters_to_list(ensure_binary(Bytes), utf8);
to_unicode_string(_, _) ->
    todo.

from_unicode_string(Str, utf8) ->
    unicode:characters_to_binary(Str);
from_unicode_string(_, _) ->
    todo.

%% to_cp932(_) -> ok.
%% from_cp932(_) -> ok.

%% to_sjis(_) -> ok.
%% from_sjis(_) -> ok.

%% to_jis(_) -> ok.
%% from_jis(_) -> ok.

%% to_eucjp(_) -> ok.
%% from_eucjp(_) -> ok.

%% to_eucjp_ms(_) -> ok.
%% from_eucjp_ms(_) -> ok.
    
ensure_binary(X) when is_list(X) ->
    list_to_binary(X);
ensure_binary(X) when is_binary(X) ->
    X.
