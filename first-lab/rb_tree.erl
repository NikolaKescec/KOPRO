-module(rb_tree).

-export([start_server/0, add/1, add/2, get/1, get/2, delete/1, delete/2]).

% In rb tree these rules must apply:
% -root has to be black
% -no two adjacent (direct descendants) red nodes
% -count of black nodes in each paths is the same
%
% nil nodes are also considered black

% INSERTION IN RB tree
% Left insert
add_value(Value, {NodeValue, Color, LeftChild, RightChild}) when Value < NodeValue ->
    balance({NodeValue, Color, add_value(Value, LeftChild), RightChild});
% Right insert
add_value(Value, {NodeValue, Color, LeftChild, RightChild}) when Value > NodeValue ->
    balance({NodeValue, Color, LeftChild, add_value(Value, RightChild)});
% Node insertion on the right place
add_value(Value, nil) ->
    {Value, red, nil, nil}.
% Initial insertion
add(Value, Node) ->
    black_root(add_value(Value, Node)).

% G -> Grandparent
% P -> Parent
% U -> Uncle
% N -> Current node

% I4: In case that the child node is red and root is also red
black_root({Value, _, LeftChild, RightChild}) ->
    {Value, black, LeftChild, RightChild}.


% I2: black G, red P and red U -> recolor P and U to black and recolor G to red
balance({GValue, black, {UValue, red, ULeftChild, URightChild},  {PValue, red, PLeftChild, PRightChild}}) ->
    {GValue, red, {UValue, black, ULeftChild, URightChild},  {PValue, black, PLeftChild, PRightChild}};


% I5: black G, red P and black U with inner red N -> apply correct rotation on the child and finish with case I6
% Right inner child of G variant
balance({GValue, black, {PValue, red, PLeftChild, {NValue, red, NLeftChild, NRightChild}},  nil}) ->
    balance({GValue, black, {NValue, red, {PValue, red, PLeftChild, NLeftChild}, NRightChild},  nil}); % -> left roration
balance({GValue, black, {PValue, red, PLeftChild, {NValue, red, NLeftChild, NRightChild}},  {UValue, black, ULeftChild, URightChild}}) ->
    balance({GValue, black, {NValue, red, {PValue, red, PLeftChild, NLeftChild}, NRightChild},  {UValue, black, ULeftChild, URightChild}}); % -> left roration
% left inner child of G variant
balance({GValue, black, nil,  {PValue, red, {NValue, red, NLeftChild, NRightChild}, PRightChild}}) ->
    balance({GValue, black, nil, {NValue, red, NLeftChild, {PValue, red, NRightChild, PRightChild}}}); % -> right rotation
balance({GValue, black, {UValue, black, ULeftChild, URightChild}, {PValue, red, {NValue, red, NLeftChild, NRightChild}, PRightChild}}) ->
    balance({GValue, black,  {UValue, black, ULeftChild, URightChild}, {NValue, red, NLeftChild, {PValue, red, NRightChild, PRightChild}}}); % -> right rotation


% I6: black G, red P and black U with outer red N -> apply correct rotation and recolor
% left outer child of G variant
balance({GValue, black, {PValue, red, {NValue, red, NLeftChild, NRightChild}, PRightChild}, nil}) ->
    {PValue, black, {NValue, red, NLeftChild, NRightChild}, {GValue, red, PRightChild, nil}}; % -> right rotation
balance({GValue, black, {PValue, red, {NValue, red, NLeftChild, NRightChild}, PRightChild}, {UValue, black, ULeftChild, URightChild}}) ->
    {PValue, black, {NValue, red, NLeftChild, NRightChild}, {GValue, red, PRightChild, {UValue, black, ULeftChild, URightChild}}}; % -> right rotation
% Right outer child of G variant
balance({GValue, black,  nil, {PValue, red, PLeftChild, {NValue, red, NLeftChild, NRightChild}}}) ->
    {PValue, black, {GValue, red, nil, PLeftChild}, {NValue, red, NLeftChild, NRightChild}}; % -> left roration
balance({GValue, black, {UValue, black, ULeftChild, URightChild}, {PValue, red, PLeftChild, {NValue, red, NLeftChild, NRightChild}}}) ->
    {PValue, black, {GValue, red, {UValue, black, ULeftChild, URightChild}, PLeftChild}, {NValue, red, NLeftChild, NRightChild}}; % -> left roration


%All cases pass
balance(Node) ->
    Node.

% DELETION IN RB TREE
% No children case
delete(Value, {NodeValue, _, nil, nil}) when Value =:= NodeValue->
    nil;
% One child case left case
delete(Value, {NodeValue, _, LeftChild, nil}) when Value =:= NodeValue->
    rebalance(LeftChild);
% One child case right case
delete(Value, {NodeValue, _, nil, RightChild}) when Value =:= NodeValue->
    rebalance(RightChild);
% First successor in right subtree
delete(Value, {NodeValue, Color, LeftChild, RightChild}) when Value =:= NodeValue->
    {FoundValue, _, _, _} = find_greatest_smallest(LeftChild),
    io:format("Found value: ~w~n", [FoundValue]),
    rebalance({FoundValue, Color, delete(FoundValue, LeftChild), RightChild});

delete(Value, {NodeValue, Color, LeftChild, RightChild}) when Value < NodeValue->
    rebalance({NodeValue, Color, delete(Value, LeftChild), RightChild});
delete(Value, {NodeValue, Color, LeftChild, RightChild}) when Value > NodeValue->
    rebalance({NodeValue, Color, LeftChild, delete(Value, RightChild)}).


find_greatest_smallest({NodeValue, Color, nil, nil}) ->
    {NodeValue, Color, nil, nil};
find_greatest_smallest({_, _, _, RightChild}) ->
    find_greatest_smallest(RightChild).

% P -> Parent
% S -> Sibling
% C -> Close nephew
% D -> Distant nephew
% N -> Current node

% D1: P is black, S is black, C is black, D is black -> repaint S red
% Left variant
rebalance({PValue, black, nil, {SValue, black, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}}) ->
    io:format("D1~n"),
    {PValue, black, nil, {SValue, red, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}};
rebalance({PValue, black, {NValue, black, NLeftChild, NRightChild}, {SValue, black, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}}) ->
    io:format("D1~n"),
    {PValue, black, {NValue, black, NLeftChild, NRightChild}, {SValue, red, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}};
% Right variant
rebalance({PValue, black, {SValue, black, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}, nil}) ->
    io:format("D1~n"),
    {PValue, black, {SValue, red, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}, nil};
rebalance({PValue, black, {SValue, black, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}, {NValue, black, NLeftChild, NRightChild}}) ->
    {PValue, black, {SValue, red, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}, {NValue, black, NLeftChild, NRightChild}};


% D3: P is black, S is red, C is black, D is black -> rotate S around P, exchange S and P colors
% Left variant
rebalance({PValue, black, nil, {SValue, red, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}}) ->
    io:format("D3~n"),
    {SValue, black, {PValue, red, nil, {CValue, black, CLeftChild, CRightChild}}, {DValue, black, DLeftChild, DRightChild}};
rebalance({PValue, black, {NValue, black, NLeftChild, NRightChild}, {SValue, red, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}}) ->
    io:format("D3~n"),
    {SValue, black, {PValue, red, {NValue, black, NLeftChild, NRightChild}, {CValue, black, CLeftChild, CRightChild}}, {DValue, black, DLeftChild, DRightChild}};
% Right variant
rebalance({PValue, black, {SValue, red, {DValue, black, DLeftChild, DRightChild}, {CValue, black, CLeftChild, CRightChild}}, nil}) ->
    io:format("D3~n"),
    {SValue, black, {DValue, black, DLeftChild, DRightChild}, {PValue, red, {CValue, black, CLeftChild, CRightChild}, nil}};
rebalance({PValue, black, {SValue, red, {DValue, black, DLeftChild, DRightChild}, {CValue, black, CLeftChild, CRightChild}}, {NValue, black, NLeftChild, NRightChild}}) ->
    io:format("D3~n"),
    {SValue, black, {DValue, black, DLeftChild, DRightChild}, {PValue, red, {CValue, black, CLeftChild, CRightChild}, {NValue, black, NLeftChild, NRightChild}}};


% D4: P is red, S is black, C is black, D is black -> exchange colors of P and S
% Left variant
rebalance({PValue, red, nil, {SValue, black, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}}) ->
    io:format("D4~n"),
    {PValue, black, nil, {SValue, red, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}};
rebalance({PValue, red, {NValue, black, NLeftChild, NRightChild}, {SValue, black, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}}) ->
    io:format("D4~n"),
    {PValue, black, {NValue, black, NLeftChild, NRightChild}, {SValue, red, {CValue, black, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}};
% Right variant
rebalance({PValue, red, {SValue, black, {DValue, black, DLeftChild, DRightChild}, {CValue, black, CLeftChild, CRightChild}}, nil}) ->
    io:format("D4~n"),
    {PValue, black, {SValue, red, {DValue, black, DLeftChild, DRightChild}, {CValue, black, CLeftChild, CRightChild}}, nil};
rebalance({PValue, red, {SValue, black, {DValue, black, DLeftChild, DRightChild}, {CValue, black, CLeftChild, CRightChild}}, {NValue, black, NLeftChild, NRightChild}}) ->
    io:format("D4~n"),
    {PValue, black, {SValue, red, {DValue, black, DLeftChild, DRightChild}, {CValue, black, CLeftChild, CRightChild}}, {NValue, black, NLeftChild, NRightChild}};


% D5: P is either red or black, S is black, C is red, D is black -> rotate C around S, color S black
% Left variant
rebalance({PValue, PColor, nil, {SValue, black, {CValue, red, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}}) ->
    io:format("D5~n"),
    rebalance({PValue, PColor, nil, {CValue, black, CLeftChild, {SValue, red, CRightChild, {DValue, black, DLeftChild, DRightChild}}}});
rebalance({PValue, PColor, {NValue, black, NLeftChild, NRightChild}, {SValue, black, {CValue, red, CLeftChild, CRightChild}, {DValue, black, DLeftChild, DRightChild}}}) ->
    io:format("D5~n"),
    rebalance({PValue, PColor, {NValue, black, NLeftChild, NRightChild}, {CValue, black, CLeftChild, {SValue, red, CRightChild, {DValue, black, DLeftChild, DRightChild}}}});
% Right variant
rebalance({PValue, PColor, {SValue, black, {DValue, black, DLeftChild, DRightChild}, {CValue, red, CLeftChild, CRightChild}}, nil}) ->
    io:format("D5~n"),
    rebalance({PValue, PColor, {CValue, black, {SValue, red, {DValue, black, DLeftChild, DRightChild}, CLeftChild}, CRightChild}, nil});
rebalance({PValue, PColor, {SValue, black, {DValue, black, DLeftChild, DRightChild}, {CValue, red, CLeftChild, CRightChild}}, {NValue, black, NLeftChild, NRightChild}}) ->
    io:format("D5~n"),
    rebalance({PValue, PColor, {CValue, black, {SValue, red, {DValue, black, DLeftChild, DRightChild}, CLeftChild}, CRightChild}, {NValue, black, NLeftChild, NRightChild}});


% D6: Parent is either red or black, S is black, D is red, C is not important -> rotate S around P, exchange S and P colors, paint D black
% Left variant
rebalance({PValue, PColor, nil, {SValue, black, SLeftChild, {DValue, red, DLeftChild, DRightChild}}}) ->
    io:format("D6~n"),
    {SValue, PColor, {PValue, black, nil, SLeftChild}, {DValue, black, DLeftChild, DRightChild}};
rebalance({PValue, PColor, {NValue, black, NLeftChild, NRightChild}, {SValue, black, SLeftChild, {DValue, red, DLeftChild, DRightChild}}}) ->
    io:format("D6~n"),
    {SValue, PColor, {PValue, black, {NValue, black, NLeftChild, NRightChild}, SLeftChild}, {DValue, black, DLeftChild, DRightChild}};
% Right variant
rebalance({PValue, PColor, {SValue, black, {DValue, red, DLeftChild, DRightChild}, SRightChild}, nil}) ->
    io:format("D6~n"),
    {SValue, PColor, {DValue, black, DLeftChild, DRightChild}, {PValue, black, SRightChild, nil}};
rebalance({PValue, PColor, {SValue, black, {DValue, red, DLeftChild, DRightChild}, SRightChild}, {NodeValue, black, NLeftChild, NRightChild}}) ->
    io:format("D6~n"),
    {SValue, PColor, {DValue, black, DLeftChild, DRightChild}, {PValue, black, SRightChild, {NodeValue, black, NLeftChild, NRightChild}}};


rebalance(Node) ->
    Node.


% RB SEARCH
get({NodeValue, {LeftChild, RightChild}}, Value) when NodeValue =:= Value ->
    {NodeValue, {LeftChild, RightChild}};
get({NodeValue, {LeftChild, _}}, Value) when NodeValue < Value ->
    get(LeftChild, Value);
get({NodeValue, {_, RightChild}}, Value) when NodeValue > Value ->
    get(RightChild, Value);
get(_, Value) ->
    {notFound, Value}.

test_add() ->
    Tree = add(6, nil),
    Tree1 = add(1, Tree),
    Tree2 = add(4, Tree1),
    Tree3 = add(2, Tree2),
    Tree4 = add(8, Tree3),
    add(9, Tree4).


test_delete() ->
    T = test_add(),
    io:format("Delete 6~n"),
    delete(6, T).
    % io:format("Delete 6~n"),
    % delete(6, T1).

loop() -> 
    loop(nil).

loop(Tree) ->
    io:format("Tree: ~p~n", [Tree]),
    receive
        {_, {add, Value}} ->
            io:format("PUT: ~w~n", [Value]),
            loop(add(Value, Tree));
        {From, {get, Value}} ->
            io:format("GET: ~w~n", [Value]),
            From!get(Value, Tree),
            loop(Tree);
        {_, {delete, Value}} ->
            io:format("DELETE: ~w~n", [Value]),
            loop(delete(Value, Tree));
        stop -> 
            true
    end.


start_server() ->
    Pid = spawn(rb_tree, loop, []),
    register(rb_tree_server, Pid).

start_client({get, Value}) -> 
    rb_tree_server!{self(), {get, Value}},
    receive
        Value -> 
            io:format("Value: ~w~n", [Value]),
            Value
    end;
start_client(Message) -> 
    rb_tree_server!{self(), Message}.
        
add(Value) ->
    start_client({add, Value}).

get(Value) ->
    start_client({get, Value}).

delete(Value) ->
    start_client({remove, Value}).
