-module(proper_ct_SUITE).
-include_lib("proper/include/proper.hrl").
-include_lib("proper_stdlib/include/proper_ct.hrl").
-compile(export_all).

all() -> proper_ct:testcases(?MODULE).

init_per_testcase(tc_prop_foo, Config) ->
    [{proper, [{numtests, 10}]} | Config].

prop_foo() ->
    ?FORALL(A, integer(), A == A).
