:- use_module(library(apply)).
delete_one(_, [], []).
delete_one(Term, [Term|Tail], Tail).
delete_one(Term, [Head|Tail], [Head|Result]) :-
  delete_one(Term, Tail, Result).

% Conversions for schedules
schedule_to_exam_lst(schedule([]), []).
schedule_to_exam_lst(schedule([event(E, _, _, _)|Tail]), [E|Res]) :- schedule_to_exam_lst(schedule(Tail), Res).

schedule_to_room_lst(schedule([]), []).
schedule_to_room_lst(schedule([event(_, R, _, _)|Tail]), [R|Res]) :- schedule_to_room_lst(schedule(Tail), Res).

has_exam_inv(E,C) :- has_exam(C,E).
schedule_to_course_lst(Schedules, Courses) :- schedule_to_exam_lst(Schedules, Exams),
                                              maplist(has_exam_inv, Exams,  Courses).
% Checks if all exams are in a schedule
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
check_against_others(_,[], _, _, _). % Rec end
check_against_others(Students, [event(Ex, _, Day, Start2)|Events], Start, End, Day) :- % Unifies if days are the same
    % Check overlap
    duration(Ex, Dur),
    has_exam(Course, Ex),
    End2 is Start2+Dur,
    overlap(Start, End, Start2, End2), % Overlaps, check if same student
    findall(S, follows(S, Course), Students2),
    intersection(Students, Students2, Ints),
    L is length(Ints),
    L = 0,
    check_against_others(Students, Events, Start, End, Day).
check_against_others(Students, [_|Events], Start, End, Day) :- % Days are different so no overlap possible
    check_against_others(Students, Events, Start, End, Day).

check_same_time(schedule([])).
check_same_time(schedule([event(Ex, _, Day, Start)|Tail])) :-
    has_exam(Course, Ex),
    duration(Ex, Dur),
    findall(S, follows(S, Course), Students),
    End is Start+Dur,
    check_against_others(Students, Tail, Start, End, Day),
    check_same_time(schedule(Tail)).

% Do least demanding tests first
is_valid(Schedule) :- check_all(Schedule),
                      check_once(Schedule),
                      check_capacity(Schedule),
                      check_availability(Schedule),
                      check_two_exams_same_room(Schedule),
                      check_same_time(Schedule).
% cost(Scedule, Cost) :- eachsoftconstraint(Schedule, Penalty),
%                        Cost is Penalty.


schedule([event(e1,r1,1,10),event(e2,r2,1,10)]).
