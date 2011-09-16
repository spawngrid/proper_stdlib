-module(proper_util).
-export([exported_properties/1, property_prefix/0]).
-include_lib("proper/include/proper_internal.hrl").

property_prefix() -> ?PROPERTY_PREFIX.

-spec exported_properties(module()) -> [atom()].
exported_properties(Module) ->
  [ Name || {Name, 0} <- Module:module_info(exports), lists:prefix(?PROPERTY_PREFIX, atom_to_list(Name)) ].



