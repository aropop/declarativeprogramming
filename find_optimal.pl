:- module(find_optimal, [find_optimal/1]).
:- use_module(cost).

% Generate solutions
gen_schedule(Schedule) :-
    findall(E, exam(E, _), Exams),
    maplist(gen_event, Exams, Events),
    Schedule = schedule(Events).
gen_event(Ex, event(Ex, Room, Day, Hour)) :-
    room(Room, _),
    first_day(FDay),
    last_day(LDay),
    between(FDay, LDay, Day),
    availability(Room, Day, SHour, EHour), % Make sure to no go through every possible hour 1-24
    between(SHour, EHour, Hour),
    duration(Ex, Dur),
    End is Hour + Dur,
    End =< EHour.

:- dynamic best/2.

find_optimal(_) :-
    assert(best(nil,99999999999)),  % Assuming that no schedule has a bigger cost then this
    gen_schedule(S),
    cost(S,CostS),
    update_best(S,CostS),
    fail.
find_optimal(S) :-
    best(S,_),
    retract(best(_,_)).

update_best(S,CostS) :-
    best(_,LowestCost),
    CostS < LowestCost,
    !,
    retract(best(_,_)),
    assert(best(S,CostS)).
update_best(_,_).
