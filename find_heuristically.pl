:- module(find_heuristically, [find_heuristically/1]).
:- use_module([cost, is_valid]).

% Generate random solutions
gen_schedule(Schedule) :-
    findall(E, exam(E, _), Exams),
    maplist(gen_event, Exams, Events),
    Schedule = schedule(Events).

% Generate a single random event
gen_event(Ex, event(Ex, Room, Day, Hour)) :- % guarantees instanciated event through random
    room(Room, _),
    first_day(FDay),
    last_day(LDay),
    random_between(FDay, LDay, Day),
    availability(Room, Day, SHour, EHour),
    duration(Ex, Dur),
    Diff is EHour - Dur,
    random_between(SHour, Diff, Hour).

% Randomizes a single event
randomize_event(event(Ex, Room, _, _), NewE) :-
    gen_event(Ex, event(Ex, Room, D, H)),
    NewE = event(Ex, Room, D, H).

% Calculate a new schedule by randomizing an old one
calc_new_schedule(schedule(OldEvs), schedule(NewEvs)) :-
    maplist(randomize_event, OldEvs, PosNewEvs),
    (is_valid(schedule(PosNewEvs)),
     NewEvs = PosNewEvs; % Normally by going randomly find one valid schedule eventualy
     calc_new_schedule(schedule(OldEvs), schedule(NewEvs))).

% Randomizes the events and takes the best out of N
% Initial schedule is passed just to randomize
maximize_over_n(0, _, nil, 99999999999).
maximize_over_n(N, InitS, BestSch, BestCost) :-
    N1 is N-1,
    maximize_over_n(N1, InitS, BestSoFarSch, BestSoFarCost),
    calc_new_schedule(InitS, NewS),
    cost(NewS, NewC),
    % Replace if better
    ((NewC < BestSoFarCost,
      BestCost = NewC,
      BestSch = NewS);
     (BestCost = BestSoFarCost,
      BestSch = BestSoFarSch)).

% Generate a valid random schedule
gen_valid(S) :-
    is_valid(OS),
    calc_new_schedule(OS, S),
    is_valid(S).

% Find Heuristically will find an Approximate best solution
find_heuristically(S) :-
    gen_valid(InitS),
    cost(InitS, InitC),
    localsearch(InitS, InitC, S).

% Local search loop
localsearch(InitS, CostI,  S) :-
% Calc new scheds
    maximize_over_n(5, InitS, NewSched, CostN), % Compare with 5 random valid schedules
    ((CostI  =< CostN,
      InitS = S);
     (localsearch(NewSched, CostN, S))).
