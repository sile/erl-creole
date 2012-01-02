-module(creole_sample).

-export([conv_file/4]).

%% FromEncoding形式のFromFileの内容を、ToEncoding形式に変換してToFileに出力する
conv_file(FromFile, FromEncoding, ToFile, ToEncoding) ->
    case file:read_file(FromFile) of
        {error, Reason} ->
            {error, Reason};
        {ok, Contents} ->
            UnicodeString = creole:to_string(Contents, FromEncoding),
            MultiByteString = creole:from_string(UnicodeString, ToEncoding),
            file:write_file(ToFile, MultiByteString)
    end.
