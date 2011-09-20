-module(proper_stdgen).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

unique(List) ->
    ?SUCHTHAT(L, List,
              length(lists:usort(L)) =:= length(L)).

subset([]) ->
    [];
subset({'$type', _} = List) ->
    ?LET(L, List,
         subset(L));
subset(List) ->
    ?LET(Indices, unique(list(integer(1, length(List)))),
        [ lists:nth(Index, List) || Index <- Indices ]).

lowercase_latin_char() ->
    integer($a, $z).

uppercase_latin_char() ->
    integer($A, $Z).

latin_char() ->
    oneof([lowercase_latin_char(), uppercase_latin_char()]).

number_char() ->
    integer($0, $9).

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

local_part_non_special_char() ->
    oneof([latin_char(),
           number_char(),
           $!, $#, $$, $%, $&, $',
           $*, $+, $-, $/, $=, $?,
           $^, $_, $`, ${, $|, $},
           $~]).

local_part_slash_special_char() ->
    [$\\, oneof([32, 92, 34])].

local_part_special_char() ->
    %% (),:;<>@[]
    oneof([
           local_part_slash_special_char(),
           $(, $), $,, $:, $;, $<, $>, $@, $[, $]
          ]).

local_part_maybe_special_char_list() ->
    [$", non_empty(list(
                     frequency([
                                {10, local_part_non_special_char()},
                                {5, local_part_special_char()}
                           ])
                    )), $"].

email_local_part() ->
    ?LET(LocalPart,
         ?SUCHTHAT(LocalPart,
                   frequency([{100,
                               non_empty(list(
                                           frequency([
                                                      {100, local_part_non_special_char()},
                                                      {2, $.},
                                                      {1, [$., local_part_maybe_special_char_list(), $.]}
                                                     ])))},
                              {1,
                               local_part_maybe_special_char_list()}]),
                   %% rules for dots
                   begin
                       LocalPartF = lists:flatten(LocalPart),
                       ((string:chr(LocalPartF, $.) > 1 andalso
                         string:rchr(LocalPartF, $.) < length(LocalPartF))
                        orelse
                        string:chr(LocalPartF, $.) =:= 0) andalso
                           string:str(LocalPartF, "..") =:= 0
                   end),
         lists:flatten(LocalPart)).


label() ->
    ?SUCHTHAT(Label,
              non_empty(list(oneof([latin_char(),
                                    number_char(),
                                    $-]))),
              ((string:chr(Label, $-) > 1 andalso
                string:rchr(Label, $-) < length(Label)) orelse
               string:chr(Label, $-) =:= 0) andalso
              length(Label) < 64).

email_domain() ->
    frequency([{20,
                ?SUCHTHAT(Hostname,
                          ?LET(Labels,
                               non_empty(list(label())),
                               string:join(Labels, ".")),
                          length(Hostname) < 256)},
               {1,
                ?LET({A,B,C,D}, {integer(0,255),
                                 integer(0,255),
                                 integer(0,255),
                                 integer(0,255)},
                     "[" ++ string:join([integer_to_list(A),
                                         integer_to_list(B),
                                         integer_to_list(C),
                                         integer_to_list(D)],".") ++ "]")}
               %% TODO: add support for IPv6
               ]).

email() ->
    ?SUCHTHAT(Email,
              ?LET({LocalPart, Domain}, {email_local_part(), email_domain()},
                   string:join([LocalPart, Domain], "@")),
              length(Email) < 255).

unicode_char() ->
    ?SUCHTHAT(C, char(), C < 16#D800 orelse C > 16#DFFF).

utf8_char() -> unicode_char().
utf16_char() -> unicode_char().
utf32_char() -> unicode_char().

utf8_string() -> unicode_string().
utf16_string() -> unicode_string().
utf32_string() -> unicode_string().

utf8_bin() -> unicode_bin(utf8).
utf16_bin() -> unicode_bin(utf16).
utf32_bin() -> unicode_bin(utf32).

unicode_string() ->
    ?LET(S, list(unicode_char()), unicode:characters_to_list(S, unicode)).

unicode_bin(OutEncoding) ->
    ?LET(S, list(unicode_char()),
         unicode:characters_to_binary(S, unicode, OutEncoding)).

