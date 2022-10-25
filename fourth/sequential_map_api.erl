-module(sequential_map_api).

-export([start_server/0, put/2, get/1, replace/2, remove/1]).

start_server() ->
    Pid = spawn(distributed_map, loop, []),
    register(map_server, Pid).

start_client({get, Key}) -> 
    io:format("GETTING: ~w~n", [Key]),
    map_server!{self(), {get, Key}},
    receive
        Value -> 
            io:format("Value: ~w~n", [Value]),
            Value
    end;
start_client(Message) -> 
    map_server!{self(), Message}.
        
put({Name, Surname}, Value) ->
    start_client({put, {Name, Surname}, Value}).

get({Name, Surname}) ->
    start_client({get, {Name, Surname}}).

replace({Name, Surname}, Value) ->
    start_client({replace, {Name, Surname}, Value}).

remove({Name, Surname}) ->
    start_client({remove, {Name, Surname}}).
