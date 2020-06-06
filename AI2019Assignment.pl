%s00160273 - Konrad Dryniewicz - AI 2019 CA Assignment

%Declarations in order to be able to modify variables after runtime.
:- dynamic goal/1.
:- dynamic item/2.
:- dynamic hal9000/3.
:- dynamic connected/3.

%SouthCorridor
connected(c0,cs1,open).
connected(cs1,c101,open).
connected(c101,c103,open).
connected(c103,c105,open).
connected(c105,c107,open).
connected(c107,c109,open).
connected(c109,c111,open).

%WestCorridor
connected(c109,c113,open).
connected(c113,c115,open).
connected(c115,c117,open).
connected(c117,c118,open).
connected(c118,cc118,open).

%NorthCorridor
connected(c118,c119,open).
connected(c119,c121,open).
connected(c121,c123,open).
connected(c123,c125,open).
connected(c125,c127,open).
connected(c127,c129,open).
connected(c129,c131,open).

%EastCorridor
connected(c131,c132,open).
connected(c132,c133,open).
connected(c133,c0,open).

%SouthRooms
connected(r101,c101,locked).
connected(r103,c103,locked).
connected(r105,c105,locked).
connected(r107,c107,locked).
connected(r111,c111,locked).

%EastRooms
connected(r113,c113,locked).
connected(r115,c115,locked).
connected(r117,c117,open).

%NorthRooms
connected(canteen,cc118,locked).
connected(r119,c119,locked).
connected(r121,c121,locked).
connected(r123,c123,locked).
connected(r125,c125,locked).
connected(r127,c127,locked).
connected(r129,c129,locked).
connected(r131,c131,locked).

%Labs
connected(lab1,cs1,locked).
connected(lab1,c101,locked).
connected(lab2,c103,locked).
connected(lab2,c103,locked).
connected(lab2,c107,locked).
connected(lab1,lab2,locked).
connected(lab3,lab2,locked).
connected(lab4,lab1,locked).
connected(c123,lab3,locked).
connected(c125,lab4,locked).
connected(c129,lab4,locked).


%Robot
hal9000(c0, 150, []).

%Items
item(key, r117).
item(coffee, canteen).


%Checks if two positions are open, so if either or are open.
connected_to(Pos1,Pos2) :-
(
    connected(Pos1,Pos2,open)
;   connected(Pos2,Pos1,open)
).
%Checks similarly as above but also if robot has the key.
connected_to(Pos1,Pos2) :-
(
    is_holding(key), 
    connected(Pos1,Pos2,_)
;   is_holding(key),
    connected(Pos2,Pos1,_)    
).

%Checks if robot has the item in its list, if so, deletes it and adds a new inventory with the item.
delete_item(Item) :-
    (
        is_holding(Item), %Goes down to check if HAL9000 carries the key in its inventory
        retract(hal9000(Position, Energy, ItemList)), %
        delete(ItemList,Item, NewList),
        asserta(hal9000(Position, Energy, NewList)),
        format("The Item: ~a was removed from inventory.", [Item])
    ).

%Checks if Robot has the item in its list and if it has, informs of it, 
%otherwise adds it to the list and the robot is appended with new list.
pickup_item(Item) :-
    (
        is_holding(Item),
        format("Robot already has this item.\n")
    ;   retract(hal9000(Pos,Energy,ItemList)),
        append(ItemList, Item, NewList),
        asserta(hal9000(Pos,Energy, NewList)),
        format("The Hal9000 robot has picked up ~a \n", [Item])
    ).

%Tells robot to move to r117 (where the key is) and then calls on pickup_item to add 'Key' to list that robot has.
pickup_key() :-
    solve(r117,key),
    pickup_item(key).

%Checks if either combination of two positions are open.
is_open(Pos1, Pos2) :-
    (
        connected(Pos1,Pos2, open)
    ;   connected(Pos2,Pos1, open)
    ).

%Checks if either combination of two positions are closed.
is_closed(Pos1,Pos2) :-
    (
           connected(Pos1,Pos2,closed)
      ;    connected(Pos2, Pos1,closed)   
    ).

%Checks if either combination of two positions are locked.
is_locked(Pos1,Pos2) :-
    (
           connected(Pos1,Pos2,locked)
      ;    connected(Pos2, Pos1,locked)   
    ).

%Checks the list that was assigned to robot to see if it has the Item.
is_holding(Item) :-
    hal9000(_,_,List),
    member(Item,List).


%Used to carry out actions by the robot
do_actions([]).

do_actions([Pos|Rest]) :-
hal9000(Position, _, _),
unlock_if_req(Position,Pos),
move(Pos),
do_actions(Rest).

%Declaring the empty predicate
unlock_if_req(_,_).

%Checks if door needs to be unlocked first then are opened and robot's energy is decreased.
unlock_if_req(Pos1,Pos2) :-
    Pos1 \== Pos2,
    is_locked(Pos1,Pos2),
    unlock_door(Pos1, Pos2),
    format("Unlocking doors between ~a and ~a \n", [Pos1, Pos2]),
    open_door(Pos1,Pos2),
    format("Opening doors between ~a and ~a \n", [Pos1, Pos2]),
    decrease_energy(5).

%Checks if door is closed and then opens it and robot's energy is decreased.
unlock_if_req(Pos1,Pos2) :-
    Pos1 \== Pos2,
    is_closed(Pos1,Pos2),
    open_door(Pos1,Pos2),
    format("Opening doors between ~a and ~a \n", [Pos1, Pos2]),
    decrease_energy(3).

%Checks if the position robot is in and target position are locked (in either order) and if robot has 'key' item in its list in order to unlock door.
unlock_door(Pos1,Pos2) :-
    (
        connected(Pos1,Pos2,locked),
        is_holding(key),
        retract(connected(Pos1,Pos2,locked)),
        assertz(connection(Pos1,Pos2,closed))
    ;   connected(Pos2,Pos1,locked),
        is_holding(key),
        retract(connected(Pos2,Pos1,locked)),
        assertz(connection(Pos2,Pos1,closed))
    ).

%Checks if position that robot is moving from is closed or locked and changes it to opened in order for the robot to be able to move there.
open_door(Pos1,Pos2) :-
    (
        connected(Pos1,Pos2,closed),
        retract(connected(Pos1,Pos2,closed)),
        assertz(connection(Pos1,Pos2,open))
    ;   connected(Pos2,Pos1,locked),
        retract(connected(Pos2,Pos1,closed)),
        assertz(connection(Pos2,Pos1,open))
    ).
%Checks if target position is Locked, through checking list that robot has if it contains key.
target_locked(Target) :-
    is_locked(_,Target),
\+ is_holding(key).

%Moves the player position one at a time.
move([]).

move(Position) :-
    (
        hal9000(Position, _, _),
        format("\nMoving from ~a \n", [Position])
        ;   hal9000(PrevPosition, Energy, Items),
        format("Moving towards ~a \n", [Position]),
        retract(hal9000(PrevPosition, Energy, Items)), NewEnergy is Energy - 1,
        asserta(hal9000(Position,NewEnergy,Items))
    ).

%Predicates used along with the "run" predicate in order to perform task of delivery of an item from point A to point B.

%Takes in an Item and a location to deliver object to.
deliver(Item, Location) :-
    item(Item, ItemPos), 
    target_locked(Location), 
    format("You will need the key to reach the goal.\n"),
    pickup_key(),
    solve(ItemPos,_),
    pickup_item(Item),
    solve(Location),
    delete_item(Item),
    format("The ~a was delivered to ~a \n", [Item, Location]).

deliver(Item, Location) :-
    item(Item, ItemPos),
    target_locked(ItemPos),
    format("You will need the key to reach the goal.\n"),
    pickup_key(),
    solve(ItemPos,_),
    pickup_item(Item),
    solve(Location),
    delete_item(Item),
    format("The ~a was delivered to ~a \n", [Item, Location]).

deliver(Item, Location) :-
    item(Item, ItemPos),
    solve(ItemPos,_),
    pickup_item(Item),
    solve(Location, _),
    delete_item(Item),
    format("The ~a was delivered to ~a \n", [Item, Location]).

%Check if robot is currently at target position.
solve(CurrentTarget) :-
    hal9000(Position,_,_),
    Position == CurrentTarget,
    format("The Hal9000 robot is already at the target position, ~a \n", [CurrentTarget]).

%Check the location and for the key
solve(CurrentTarget,key) :-
    hal9000(_,_,Items),
    \+ member(key,Items),
    format("\nCurrent Target is ~a \n", [CurrentTarget]),
    asserta(goal(CurrentTarget)),
    hal9000(Position,_,_),
    solve_key(Position, Solution),
    reverse(Solution, SolReversed),
    do_actions(SolReversed),
    retract(goal(CurrentTarget)),
    format("The Hal9000 robot has arrived at the target ~a \n",[CurrentTarget]).

solve(CurrentTarget,_) :-
    format("\nCurrent Target is ~a \n", [CurrentTarget]),
    asserta(goal(CurrentTarget)),
    hal9000(Position,_,_),
    solveBestCostGoal(Position,Solution),
    Solution=[_,List],
    reverse(List, SolReversed),
    do_actions(SolReversed),
    retract(goal(CurrentTarget)),
    format("The Hal9000 robot has arrived at the target ~a \n",[CurrentTarget]).

% Breadth First Search Code (Use to find the Key):

% solve( Start, Solution):
%    Solution is a path (in reverse order) from Start to a goal

solve_key( Start, Solution)  :-
    breadthfirst( [ [Start] ], Solution).
  
  % breadthfirst( [ Path1, Path2, ...], Solution):
  %   Solution is an extension to a goal of one of paths
  
  breadthfirst( [ [Node | Path] | _], [Node | Path])  :-
    goal(Node).
  
  breadthfirst( [Path | Paths], Solution)  :-
    extend( Path, NewPaths),
    append( Paths, NewPaths, Paths1),
    breadthfirst( Paths1, Solution).
  
  extend( [Node | Path], NewPaths)  :-
    setof( [NewNode, Node | Path],
           (connected_to( Node, NewNode),\+member( NewNode, [Node | Path] ) ),
           NewPaths),
    !.
  
 extend(_, [] ). % setof failed: Node has no successor


 %Best Cost First Search Code:
 %We use it to get solution for the paths.
 solveBestCostGoal(Start,Sol) :-
    bestCost(Start,[[0,[Start]]],Sol).

bestCost(Node,[[Gcost,[Node|Path]]|_],[Gcost,[Node|Path]]) :-
    goal(Node).

bestCost(Node, [[Gcost,[Node|Path]]|Rest],Sol) :-
    setof([NewCost,[Successor,Node|Path]],
        Successor^Cost^(connected_to(Node,Successor),
        getCost(Node, Successor, Cost),
        \+member( NewNode, [Node | Path] ),
        NewCost is Gcost + Cost),NewFringe),
    append(NewFringe,Rest,NewFrontier),
    sort(NewFrontier,[[NewGcost,[NewNode|PreviousNodes]]|Others]),
    bestCost(NewNode,[[NewGcost,[NewNode|PreviousNodes]]|Others],Sol).

% Discard current top node as bagof has failed
bestCost(_,[_,[Gcost,[NextNode|Path]]|Rest],Sol) :-
  bestCost(NextNode,[[Gcost,[NextNode|Path]]|Rest],Sol).

%Predicate to check for the cost depending if the target is either open, closed or locked (each cost will be different.)
getCost(Pos1, Pos2,Cost) :-
    is_open(Pos1,Pos2),
    Cost is 1.

getCost(Pos1, Pos2,Cost) :-
    is_closed(Pos1,Pos2),
    Cost is 4.

getCost(Pos1, Pos2,Cost) :-
    is_locked(Pos1,Pos2),
    Cost is 6.

%Predicate used to decrease energy that is used on different actions
decrease_energy(Amount) :-
    retract(hal9000(Pos, Energy, Items)),
    NewEnergy is Energy-Amount,
    asserta(hal9000(Pos, NewEnergy, Items)).

%This is an entry predicate which is used to run other method predicates such as Deliver coffee etc.

run([]).

run([Target|Backlog]) :-
    Target,
    run(Backlog),
    !.
