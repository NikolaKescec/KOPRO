-module(load_balancer).

-export([start_balancer/0, put/2, get/1, replace/2, remove/1, balancer_loop/2]).

start_balancer() ->
    Pid1 = spawn('x@x', distributed_map, loop, []),
    Pid2 = spawn('y@y', distributed_map, loop, []),
    BalancerPid = spawn(load_balancer, balancer_loop, [Pid1, Pid2]),
    register(balancer, BalancerPid).

balancer_loop(Pid1, Pid2) ->
    receive
        {From, {Action, {Name, Surname}}} ->
            SurnameLength = string:length(Surname),
            if
                SurnameLength > 3 -> Pid1!{From, {Action, {Name, Surname}}};
                true -> Pid2!{From, {Action, {Name, Surname}}}
            end,
            balancer_loop(Pid1, Pid2);
        {From, {Action, {Name, Surname}, Value}} ->
            SurnameLength = string:length(Surname),
            if
                SurnameLength > 3 -> Pid1!{From, {Action, {Name, Surname}, Value}};
                true -> Pid2!{From, {Action, {Name, Surname}, Value}}
            end,
            balancer_loop(Pid1, Pid2)
    end.

start_client({get, Key}) -> 
    io:format("GETTING: ~w~n", [Key]),
    balancer!{self(), {get, Key}},
    receive
        Value -> 
            io:format("Value: ~w~n", [Value]),
            Value
    end;
start_client(Message) -> 
    balancer!{self(), Message}.
        
put({Name, Surname}, Value) ->
    start_client({put, {Name, Surname}, Value}).

get({Name, Surname}) ->
    start_client({get, {Name, Surname}}).

replace({Name, Surname}, Value) ->
    start_client({replace, {Name, Surname}, Value}).

remove({Name, Surname}) ->
    start_client({remove, {Name, Surname}}).
