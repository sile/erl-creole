-module(test).

-export([test/1, test2/1]).

test(Path) ->
    {ok,In} = file:open(Path,[read, binary]),
    each_line(In),
    file:close(In).

each_line(In) ->
    case file:read_line(In) of
        eof ->
            ok;
        {ok, Line} ->
            case unicode:characters_to_list(Line,utf8) of
                {error, Reason, _} ->
                    io:format("# ERROR: ~p~n", [Reason]);
                List ->
                    creole_general:from_unicode_string(List, cp932)
            end,
            %io:format("# ~p~n", [creole_general:from_unicode_string(unicode:characters_to_list(Line,utf8), cp932)]),
            each_line(In)
            %ok
    end.

test2(Path) ->
    {ok,In} = file:open(Path,[read]),
    each_line2(In),
    file:close(In).

each_line2(In) ->
    case file:read_line(In) of
        eof ->
            ok;
        {ok, Line} ->
            iconv_api:utf8_to_jis(Line),
            %io:format("# ~p~n", [iconv_api:utf8_to_cp932(Line)]),
            %ok
            each_line(In)
    end.
