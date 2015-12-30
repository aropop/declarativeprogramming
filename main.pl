:- use_module(library(apply)).
delete_one(_, [], []).
delete_one(Term, [Term|Tail], Tail).
delete_one(Term, [Head|Tail], [Head|Result]) :-
    delete_one(Term, Tail, Result).

% Sums a list
list_sum([Item], Item).
list_sum([Item1,Item2 | Tail], Total) :-
    list_sum([Item1+Item2|Tail], Total).

% converts en atom to a number so it can be compared
exam_to_num(Exam, Num) :-
    atom_codes(Exam, [_|Codes]),
    list_sum(Codes, Num).

% Conversions for schedules, only converts if Start is an integer
schedule_to_exam_lst(schedule([]), []).
schedule_to_exam_lst(schedule([event(E, _, _, St)|Tail]), [E|Res]) :-
    integer(St),
    schedule_to_exam_lst(schedule(Tail), Res).
% Converts a schedule to a list of rooms
schedule_to_room_lst(schedule([]), []).
schedule_to_room_lst(schedule([event(_, R, _, _)|Tail]), [R|Res]) :- schedule_to_room_lst(schedule(Tail), Res).
% Invert parameter order for maplist
has_exam_inv(E,C) :- has_exam(C,E).
% Converts a schedule to a list of Courses
schedule_to_course_lst(Schedules, Courses) :- schedule_to_exam_lst(Schedules, Exams),
                                              maplist(has_exam_inv, Exams,  Courses).
% Checks if all exams are in a schedule and wether it consits of integer start hours
check_all(Schedule) :- schedule_to_exam_lst(Schedule, Lst),
                       findall(Exam, exam(Exam,_), Exams),
                       permutation(Exams, Lst).


% Checks if an element is onlly once in a list
is_scheduled_once(Exam, Lst) :- delete_one(Exam, Lst, R),
                                not(member(Exam, R)).
% Takes two lists, Takes an element of the first one and checks if it is only
% Scheduled once. The second list is thus a list of all exams
schedule_map([], _).
schedule_map([El|Tail], Total) :- is_scheduled_once(El, Total),
                                  schedule_map(Tail, Total).
check_once(Schedule) :- schedule_to_exam_lst(Schedule, Lst),
                        schedule_map(Lst, Lst).

% Checks if a room can fit the people in a course
can_take_students(Room, Course) :- capacity(Room, Cap),
                                   findall(S, follows(S, Course), Students),
                                   length(Students, Numstudents),
                                   Cap >= Numstudents.
% Execute room check on a schedule
capacity_map([], _).
capacity_map([R|Rlist], [C|Clist]) :- can_take_students(R, C),
                                      capacity_map(Rlist, Clist).
check_capacity(Schedule) :- schedule_to_course_lst(Schedule, Clist),
                            schedule_to_room_lst(Schedule, Rlist),
                            capacity_map(Rlist, Clist).

% Check if the room is available
is_available(Room, Day, Start, Duration) :- End is Start + Duration,
                                            availability(Room, Day, ForseenSt, ForseenEnd),
                                            ForseenEnd >= End,
                                            Start >= ForseenSt.
check_availability(schedule([])).
check_availability(schedule([event(E, R, D, H)|Elist])) :- duration(E, Dur),
                                                           is_available(R, D, H, Dur),
                                                           check_availability(schedule(Elist)).

% Check 2 exams same time in same room
overlap(Start1, End1, Start2, End2) :-
    ((Start1 =< Start2, Start2 < End1);
     (Start2 =< Start1, Start1 < End2)).
overlap(event(E1, R, D, H1), event(E2, R, D, H2)) :- duration(E1, Dur1),
                                                     duration(E2, Dur2),
                                                     End1 is H1+Dur1,
                                                     End2 is H2+Dur2,
                                                     overlap(H1, End1, H2, End2).
fit(E1, E2) :- not(overlap(E1, E2)).
fit_lst(_, []).
fit_lst(E1, [E2|Rst]) :- fit(E1, E2),
                         fit_lst(E1, Rst).
check_two_exams_same_room(schedule([])).
check_two_exams_same_room(schedule([H|T])) :- fit_lst(H, T),
                                              check_two_exams_same_room(schedule(T)).


% Check student at the same time
check_against_others(_, _, [], _, _, _). % Rec end
check_against_others(Students, Teacher, [event(Ex, _, Day, Start2)|Events], Start, End, Day) :- % Unifies if days are the same
    % Check overlap
    duration(Ex, Dur),
    has_exam(Course, Ex),
    End2 is Start2+Dur,
    overlap(Start, End, Start2, End2), % Overlaps, check if same student
    findall(S, follows(S, Course), Students2),
    intersection(Students, Students2, Ints),
    length(Ints, L),
    L == 0,
    teaches(Teacher2, Course),
    Teacher2 \== Teacher,
    check_against_others(Students, Events, Start, End, Day).
check_against_others(Students, Teacher, [event(_, _, Day1, _)|Events], Start, End, Day2) :- % Days are different so no overlap possible
    Day1 \== Day2,
    check_against_others(Students, Teacher, Events, Start, End, Day2).

check_same_time(schedule([])).
check_same_time(schedule([event(Ex, _, Day, Start)|Tail])) :-
    has_exam(Course, Ex),
    duration(Ex, Dur),
    teaches(Teacher, Course),
    findall(S, follows(S, Course), Students),
    End is Start+Dur,
    check_against_others(Students, Teacher, Tail, Start, End, Day),
    check_same_time(schedule(Tail)).

% Do least demanding tests first
is_valid(Schedule) :- check_all(Schedule),
                      check_once(Schedule),
                      check_capacity(Schedule),
                      check_availability(Schedule),
                      check_two_exams_same_room(Schedule),
                      check_same_time(Schedule).

% Compare 2 events
event_compare(C, event(E1, _, Day1, Start1), event(E2, _, Day2, Start2)) :-
    (compare(C, Day1, Day2), C \= = );
    (compare(C, Start1, Start2), C \= =);
    (exam_to_num(E1, Num1), exam_to_num(E2, Num2), compare(C, Num1, Num2)).

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

compare_day(Day, event(_,_,Day,_)).

% Prints all events per day
print_loop([]).
print_loop(Lst) :-
    Lst = [event(_, _, D, _)|Events],
    include(compare_day(D), Lst, EventsOnDay),
    format('Day ~w: ~n', [D]),
    print_day(EventsOnDay),
    exclude(compare_day(D), Events, EventsOnOtherDays),
    print_loop(EventsOnOtherDays).

% Main predicate, first sorts then prints all exams
pretty_print(schedule(EventList)) :-
    predsort(event_compare, EventList, SortedEvents),
    print_loop(SortedEvents).

% cost(Scedule, Cost) :- eachsoftconstraint(Schedule, Penalty),
%                        Cost is Penalty.


schedule([event(e1,r1,1,10),event(e2,r2,1,10)]).
