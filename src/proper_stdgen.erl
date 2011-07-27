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

posix_filename() ->
    non_empty(list(frequency([
                              {10, latin_char()},
                              {5, number_char()},
                              {3,$_},
                              {1,$.},
                              {2,$-}]))).

posix_filepath() ->
    ?LET(Path, non_empty(list(posix_filename())),
         string:join(Path, "/")).

posix_abs_filepath() ->
    ?LET(Path, non_empty(list(posix_filename())),
         filename:absname("/" ++ string:join(Path, "/"))).


