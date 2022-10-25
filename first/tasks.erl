-module(tasks).

-export([addList/2, addListSorted/2, addListSortedWithFunction/3, addTupleToListSortedByKey/2, searchList/2, createNode/1, insert/2, searchTree/2]).

% 1. napraviti funkciju koja dodaje element (broj) u listu (svejedno koje mjesto); 
% argumenti su lista i broj
addList(List, Number) ->
    List ++ [Number].

% 2. napraviti funkciju koja dodaje element (broj) u listu (svejedno koje mjesto) sortirano uzlazno;
% argumenti su lista i broj; 
% pretpostavka je da je ulazna lista sortirana po uzlaznom redu
addListSorted(List, Number) ->
    addListSorted(List, Number, []).

addListSorted([Element | Rest], Number, Accumulator) when Element < Number ->
    addListSorted(Rest, Number, Accumulator ++ [Element]);
addListSorted(RemainingList, Number, Accumulator) ->
    Accumulator ++ [Number] ++ RemainingList.

% 3. koristiti funckiju viseg reda
addListSortedWithFunction(List, Number, Function) ->
    addListSortedWithFunction(List, Number, [], Function).

addListSortedWithFunction(RemainingList, Number, Accumulator, Function) ->
    [Element | Rest] = RemainingList,
    Boolean = Function(Element, Number),
    if
        Boolean -> addListSortedWithFunction(Rest, Number, Accumulator ++ [Element], Function);
        true -> Accumulator ++ [Number] ++ RemainingList
    end.

% Sort rjesenje:
% addListSortedWithFunction(List, Number, SortOrderFunction) -> 
%    lists:sort(SortOrderFunction, List ++ [Number]).

% 4. napraviti funkciju koja dodaje n-torke (tuple) u obliku {key, value} u listu, a lista je sortirana po kljucevima
addTupleToListSortedByKey(List, Tuple) ->
    addTupleToListSortedByKey(List, Tuple, []).

addTupleToListSortedByKey([{Key1, V1} | Rest], {Key2, V2}, Accumulator) when Key1 < Key2 ->
    addTupleToListSortedByKey(Rest, {Key2, V2}, Accumulator ++ [{Key1, V1}]);
addTupleToListSortedByKey(RemainingList, Tuple, Accumulator) ->
    Accumulator ++ [Tuple] ++ RemainingList.

% Sort rjesenje:
% addTupleToListSortedByKey(List, Tuple) ->
%     lists:sort(fun({K1, _}, {K2, _}) -> K1 < K2 end, List ++ [Tuple]).

% 5.napraviti funkciju pretraživanja liste tupleova po ključu. Argumenti su lista i ključ, a vraća vrijednost.
searchList([{TupleKey, Value}|_], Key) when TupleKey =:= Key ->
    Value;
searchList([_|Rest], Key) ->
    searchList(Rest, Key).

% 6. napraviti stablo (funkcije dodavanja, pretraživanja elemenata u stablu) 
createNode(Value) -> 
    {Value, {nil, nil}}.


insert({NodeValue, {LeftChild, RightChild}}, Value) when NodeValue < Value ->
    {NodeValue, {insert(LeftChild, Value), RightChild}};
insert({NodeValue, {LeftChild, RightChild}}, Value) when NodeValue > Value ->
    {NodeValue, {LeftChild, insert(RightChild, Value)}};
insert(nil, Value) -> 
    createNode(Value).


searchTree({NodeValue, {LeftChild, RightChild}}, Value) when NodeValue =:= Value ->
    {NodeValue, {LeftChild, RightChild}};
searchTree({NodeValue, {LeftChild, _}}, Value) when NodeValue < Value -> 
    searchTree(LeftChild, Value);
searchTree({NodeValue, {_, RightChild}}, Value) when NodeValue > Value -> 
    searchTree(RightChild, Value).