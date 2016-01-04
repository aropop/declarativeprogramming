:- module(print, [pretty_print/1]).
:- use_module(util).

% Print schedule
%--------------------
% Prints a single exam
print_exam([]) :- format('~n', []).
print_exam([event(Ex, _, _, St)|Exs]) :-
    duration(Ex, Dur),
    End is St + Dur,
    format('~a at ~a until ~a, ', [Ex, St, End]),
    print_exam(Exs).

compare_room(Room, event(_, Room, _, _)).

% Prints all events on a single day per room
print_day([]) :- format('~n', []).
print_day(Lst) :-
    Lst = [event(_, R, _, _)|Events],
    include(compare_room(R), Lst, EventsInRoom),
    room(R, RoomName),
    format('Room ~w: ',[RoomName]),
    print_exam(EventsInRoom),
    exclude(compare_room(R), Events, EventsInOtherRooms),
    print_day(EventsInOtherRooms).

compare_day(Day, event(_,_,Day,_)). % Used for include and exclude of events

% Prints all events per day
print_loop([]).
print_loop(Lst) :-
    Lst = [event(_, _, D, _)|Events],
    include(compare_day(D), Lst, EventsOnDay), % Only use the events on this day
    format('Day ~w: ~n', [D]),
    print_day(EventsOnDay), % Print all events on this day
    % Continue with other days
    exclude(compare_day(D), Events, EventsOnOtherDays),
    print_loop(EventsOnOtherDays).

% Main predicate, first sorts then prints all exams
pretty_print(schedule(EventList)) :-
    predsort(event_compare, EventList, SortedEvents),
    print_loop(SortedEvents).
