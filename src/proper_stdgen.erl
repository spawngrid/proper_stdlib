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

utf8_char() ->
    ?LET(Byte, byte(),
         binary_to_list(unicode:characters_to_binary([Byte]))).

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

local_part_non_special_char() ->
    oneof([
           integer(65,90), %% a-z
           integer(97,122), %% A-Z
           integer(48,57), %% 0-9
           %% !#$%&'*+-/=?^_`{|}~ 
           33, 
           integer(35,39), 
           42, 
           43, 
           45, 
           47, 
           61, 
           63, 
           integer(94,96), 
           integer(123,126)]).

local_part_slash_special_char() ->
    [$\\, oneof([32, 92, 34])].

local_part_special_char() ->
    %% (),:;<>@[]
    oneof([
           local_part_slash_special_char(),
           40, 41, 44, 58, 59, 60, 62, 64, 91, 93
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
              non_empty(list(oneof([
                                    integer(65,90), %% a-z
                                    integer(97,122), %% A-Z
                                    integer(48,57), %% 0-9,
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
                ?LET({A,B,C,D}, {integer(0,255),integer(0,255),integer(0,255),integer(0,255)},
                     "[" ++ string:join([integer_to_list(A),
                                         integer_to_list(B),
                                         integer_to_list(C),
                                         integer_to_list(D)],".") ++ "]")}
               %% TODO: add support for IPv6
               ]).

email() ->
    ?LET({LocalPart, Domain}, {email_local_part(), email_domain()},
         string:join([LocalPart, Domain], "@")).

