-module(creole).

-export([from_string/2, from_string/3,
         to_string/2, to_string/3,
         convert/3,
         replace/1]).

%% マルチバイト文字列を別のエンコーディングのマルチバイト文字列に変換する
%% 変換不能な文字列は"?"で代替される
convert(Bytes, FromEncoding, ToEncoding) ->
    convert(Bytes, FromEncoding, ToEncoding, replace($?)).

%% マルチバイト文字列を別のエンコーディングのマルチバイト文字列に変換する
%% 変換不能な文字に遭遇した場合の挙動は ErrFn によって制御される
convert(Bytes, FromEncoding, ToEncoding, ErrFn) ->
    from_string(to_string(Bytes, FromEncoding, ErrFn), ToEncoding, ErrFn).

%% ユニコード文字列から指定のマルチバイト文字列(バイナリ)に変換する
%% 変換不能な文字は"?"で代替される
from_string(String, Encoding) ->
    from_string(String, Encoding, replace($?)).

%% ユニコード文字列から指定のマルチバイト文字列(バイナリ)に変換する
%% 変換不能な文字に遭遇した場合の挙動は ErrFn によって制御される
from_string(String, Encoding, ErrFn) ->
    case Encoding of
        utf8 -> creole_utf8:from_string(String, ErrFn);
        jis -> creole_jis:from_string(String, ErrFn);
        iso_2022_jp_1 -> creole_iso_2022_jp_1:from_string(String, ErrFn);
        _ -> creole_general:from_string(String, Encoding, ErrFn)
    end.

%% 指定のマルチバイト文字列(バイナリ)からユニコード文字列に変換する
%% 変換不能なバイト列は"?"で代替される
to_string(Bytes, Encoding) ->
    to_string(Bytes, Encoding, replace($?)).

%% 指定のマルチバイト文字列(バイナリ)からユニコード文字列に変換する
%% 変換不能なバイト列に遭遇した場合の挙動は ErrFn によって制御される
to_string(Bytes, Encoding, ErrFn) when is_list(Bytes) ->
    to_string(list_to_binary(Bytes), Encoding, ErrFn);
to_string(Bytes, Encoding, ErrFn) ->
    case Encoding of
        utf8 -> creole_utf8:to_string(Bytes, ErrFn);
        jis -> creole_jis:to_string(Bytes, ErrFn);
        iso_2022_jp_1 -> creole_iso_2022_jp_1:to_string(Bytes, ErrFn);
        _ -> creole_general:to_string(Bytes, Encoding, ErrFn)
    end.

%% from_string/to_string に渡すErrFn引数用の関数を生成する関数
%% 不正な文字/バイト列に遭遇した場合の代替とする文字を引数に取る
replace(Char) ->
    fun (<<_, Rest/binary>>) ->
            {Char, Rest, true};
        ([_|Rest]) ->
            {Char, Rest, true}
    end.
