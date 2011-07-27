-module(proper_stdgen).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

unique(List) ->
    ?SUCHTHAT(L, List,
              length(lists:usort(L)) =:= length(L)).

lowercase_latin_char() ->
    integer(97,122).

uppercase_latin_char() ->
    integer(65,90).

latin_char() ->
    oneof([lowercase_latin_char(), uppercase_latin_char()]).

number_char() ->
    integer(48,57).

bytestring() ->
    list(byte()).

binary_string(IO) ->
    ?LET(Io, IO,
         iolist_to_binary(Io)).

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
    frequency([{10, "/"++posix_filepath()}, {1, "/"}]).

