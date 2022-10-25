-module(tasks_implemented).

-export([put/3, get/2, replace/3, remove/2]).

% map functions: put, remove, replace and search map, map structure is #{{Name, Surname} => [{Name, Surname}]}

put({Name, Surname}, Value, Map) ->
    maps:put({Name, Surname}, Value, Map).

get({Name, Surname}, Map) ->
    maps:get({Name, Surname}, Map).

replace({Name, Surname}, Value, Map) ->
    maps:update({Name, Surname}, Value, Map).

remove({Name, Surname}, Map) ->
    maps:remove({Name, Surname}, Map).
