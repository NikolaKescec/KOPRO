% Napraviti proces koji prima poruke za stavljanje, brisanje, zamjenu i pretravanje mape (ključ n-torka ime i prezime
% vrijednost je lista prijatelja - ntorka ime i prezime)

-module(distributed_map).

-export([loop/0]).

-import(preparation, [put/3, get/2, replace/3, remove/2]).

loop() -> 
    loop(#{}).

loop(Map) ->
    io:format("Map: ~p~n", [Map]),
    receive
        {_, {put, Key, Value}} ->
            io:format("PUT: ~w~w~n", [Key, Value]),
            loop(preparation:put(Key, Value, Map));
        {From, {get, Key}} ->
            io:format("GET: ~w~n", [Key]),
            From!preparation:get(Key, Map),
            loop(Map);
        {_, {replace, Key, Value}} ->
            io:format("REPLACE: ~w~w~n", [Key, Value]),
            loop(preparation:put(Key, Value, Map));
        {_, {remove, Key}} ->
            io:format("REMOVE: ~w~n", [Key]),
            loop(preparation:remove(Key, Map));
        stop -> 
            true
    end.



