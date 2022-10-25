-module(tasks).

-export([put/3]).

% map functions: put, remove, replace and search map, map structure is #{{Name, Surname} => [{Name, Surname}]}

put(Key, Value, Map) ->
    case Map of
        #{ Key := _ } -> Map#{ Key := Value };
        #{} -> Map#{ Key => Value }
    end.

search(Key, Map) ->
    case Map of
        #{ Key := Value } -> Value
    end.

replace(Key, Value, Map) ->
    case Map of
        #{ Key := _ } -> Map#{ Key := Value }
    end.

remove(Key, Map) ->
    maps:remove(Key, Map).
