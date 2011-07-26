-module(proper_stdgen).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

lowercase_latin_char() ->
    integer(97,122).

uppercase_latin_char() ->
    integer(65,90).

latin_char() ->
    oneof([lowercase_latin_char(), uppercase_latin_char()]).

number_char() ->
    integer(48,57).
