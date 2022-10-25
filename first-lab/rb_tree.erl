-module(rb_tree).

-export([test_add/0, add/2, get/2, delete/2]).

% In rb tree these rules must apply:
% -root has to be black
% -no two adjacent (direct descendants) red nodes
% -count of blac nodes in each paths is the same
% 
% nil nodes are also considered black

% G -> Grandparent
% P -> Parent
% U -> Uncle
% N -> Current node


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
delete(_, _) ->
    {}.

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
    Tree3 = add(8, Tree2),
    add(9, Tree3).
