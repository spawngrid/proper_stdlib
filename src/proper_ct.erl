-module(proper_ct).
-export([parse_transform/2]).
-export([tc_proper/2]).
-export([testcases/1]).

-include_lib("common_test/include/ct.hrl").

-record(state, {
		    properties = []
	     }).
parse_transform(Forms, _Options) ->
	#state{ properties = Properties } = collect_properties(Forms, #state{}),
	{HdrForms, Clauses} = lists:splitwith(fun(T) when element(1, T) =/= function -> true; (_) -> false end, Forms),
 	HdrForms ++
	tc_exports(Properties) ++
	Clauses ++
	tc_clauses(Properties).

tc_output([C], []) when is_integer(C) ->
   io:format(user, [C], []);
tc_output(Format, Data) ->
   ct:log(Format, Data),
   io:format(user, Format, Data).

tc_proper(Prop, Config) ->
	 Options = proplists:get_value(proper, Config, []),
   true = proper:quickcheck(Prop, Options ++ [{on_output, fun tc_output/2}]).

testcases(Module) ->
	 [ list_to_atom("tc_" ++ atom_to_list(Name)) || Name <- proper_util:exported_properties(Module) ].

%% parse_transform

collect_properties([], State) ->
	State;
collect_properties([Form|Rest], State) ->
	State1 = collect_form(Form, State),
  collect_properties(Rest, State1).

collect_form({attribute, _Line, export, Funs}, #state{ properties = Properties } = State) ->
	NewProperties = [ Name || {Name, 0} <- Funs, lists:prefix(proper_util:property_prefix(), atom_to_list(Name)) ],
	State#state{ properties = NewProperties ++ Properties };
collect_form(_, State) ->
	State.

tc_exports(Properties) ->
	[{attribute, 0, export, 
			[ {list_to_atom(string:join(["tc", atom_to_list(Name)],"_")),1} || Name <- Properties ]}].

tc_clauses(Properties) ->
	[ begin
				Atom = list_to_atom(string:join(["tc", atom_to_list(Name)],"_")),
				{function, 0, Atom, 1, [{clause, 0, [{var, 0, 'Config'}], [],
									[
										{call,0,{remote,0,{atom, 0, ?MODULE},{atom, 0, tc_proper}},[{call, 0, {atom, 0, Name}, []}, {var, 0, 'Config'}]}
									]}]}
		end || Name <- Properties ].
