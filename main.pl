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
% Check if 2 times overlap
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
% Check 2 exams same time in same room
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
check_against_others(Students, Teacher, [event(Ex, _, Day1, Start1)|Events], Start2, End2, Day2) :- % Not overlapping
    ((Day1 \== Day2);
     (Day1 == Day2,
      duration(Ex, Dur),
      End1 is Start1 + Dur,
      not(overlap(Start2, End2, Start1, End1)))),
    check_against_others(Students, Teacher, Events, Start2, End2, Day2).

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
is_valid(Schedule) :- check_all(Schedule),!,
                      check_once(Schedule),!,
                      check_capacity(Schedule),!,
                      check_availability(Schedule),!,
                      check_two_exams_same_room(Schedule),!,
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
lunch_break_cost(Students, Teacher, TCost, SCost) :-
    sc_lunch_break(Teacher, TCost),
    maplist(sc_lunch_break, Students, PLst),
    list_sum(PLst, SCost).

period_cost(Teacher, St, End, Day, Cost) :-
    sc_no_exam_in_period(Teacher, Day, St2, End2, Cost),
    overlap(St, End, St2, End2).
period_cost(_, _, _, _, 0).

exam_on_day_cost([], _, _, _, _, 0).
exam_on_day_cost([Pid|Persons], Exam, Day, Start, End, Cost) :-
    sc_not_in_period(Pid, Exam, Day, Start2, End2, Penalty),
    overlap(Start, End, Start2, End2),
    exam_on_day_cost(Persons, Exam, Day, Start, End, BuildCost),
    Cost is Penalty + BuildCost.
exam_on_day_cost([_|Persons], Exam, Day, Start, End, Cost) :-
    exam_on_day_cost(Persons, Exam, Day, Start, End, Cost).

exam_b2b_cost(_, T1, T2, 0) :- T1 \= T2.
exam_b2b_cost([], _, _, 0).
exam_b2b_cost([Pid|Persons], End, St, Cost) :-
    sc_b2b(Pid, Penalty),
    exam_b2b_cost(Persons, End, St, BuildCost),
    Cost is Penalty + BuildCost.
exam_b2b_cost([_|Persons], End, St, Cost) :-
    exam_b2b_cost(Persons, End, St, Cost).

% finds all b2b exams and samedayexams
b2b_exam(event(Ex, _, Day, Start), Events, B2BExams, SameDayExams) :-
    duration(Ex, Dur),
    End is Start + Dur,
    findall(Y, member(event(Y, _, Day, _), Events), SameDayExams), !,
    findall(X, member(event(X, _, Day, End), SameDayExams), B2BExams).

% Abstract predicate
% Pred is a predicate that returns a Penalty for a given Student (e.g. sc_b2b)
% It will return the cost for each pair and for each student and each pair of exams accumulated
on_same_day_student_cost(_, _, [], 0).
on_same_day_student_cost(Pred, Students, [Ex|Lst], Cost) :-
    has_exam(Course, Ex),
    findall(S, follows(S, Course), OtherStudents),
    intersection(Students, OtherStudents, Overlap),
    maplist(Pred, Overlap, Costs),
    list_sum(Costs, CurCost),
    on_same_day_student_cost(Pred, Students, Lst, BuildCost),
    Cost is CurCost + BuildCost.


b2b_student_cost(Students, Lst, Cost) :-
    on_same_day_student_cost(sc_b2b, Students, Lst, Cost).

% Same day cost for students
same_day_student_cost(People, Lst, Cost) :-
    on_same_day_student_cost(sc_same_day, People, Lst, Cost).

% Abstract predicate
% Pred is a predicate that returns a Penalty for a given Teacher (e.g. sc_b2b)
% It will return the cost for each pair of exams accumulated
on_same_day_teacher_cost(_, _, [], 0).
on_same_day_teacher_cost(Pred, Teacher, [Ex|Lst], Cost) :-
    has_exam(Course, Ex),
    teaches(TeacherFound, Course),
    ((TeacherFound = Teacher,
      call(Pred, Teacher, Penalty),
      on_same_day_teacher_cost(Pred, Teacher, Lst, BuildCost),
      Cost is Penalty + BuildCost);
     (on_same_day_teacher_cost(Pred, Teacher, Lst, Cost))).

% B2B cost for teachers
b2b_teacher_cost(Teacher, Lst, Cost) :-
    on_same_day_teacher_cost(sc_b2b, Teacher, Lst, Cost).

% Same day cost for teachers
same_day_teacher_cost(Teacher, Lst, Cost) :-
    on_same_day_teacher_cost(sc_same_day, Teacher, Lst, Cost).

% Loop over all exams and calculate the cost for students and exams
cost_loop([], 0, 0).
cost_loop(EventLst, StCost, TCost) :-
    % Find all information about the exam
    EventLst = [event(Ex, Rm, Day, St)|Evnts],
    has_exam(Course, Ex),
    teaches(Teacher, Course),
    duration(Ex, Dur),
    End is St+Dur,
    findall(S, follows(S, Course), Students),
    % Calculate the cost for each soft constraint
    lunch_break_cost(Students, Teacher, LBTcost, LBScost),
    period_cost(Teacher, St, End, Day, PeriodCost),
    exam_on_day_cost(Students, Ex, Day, St, End, OnDayCostStudent),
    exam_on_day_cost([Teacher], Ex, Day, St, End, OnDayCostTeacher),
    b2b_exam(event(Ex, Rm, Day, St), Evnts, B2BExams, SameDayExams), !,
    b2b_student_cost(Students, B2BExams, B2bStCost),
    b2b_teacher_cost(Teacher, B2BExams, B2bTCost),
    b2b_student_cost(Students, SameDayExams, SdStCost),
    b2b_teacher_cost(Teacher, SameDayExams, SdTCost),
    % Go deeper in recursion
    cost_loop(Evnts, BuildStcost, BuildTCost),
    % Sum the costs from this iteration and the previously calculated ones
    StCost is BuildStcost + LBScost + OnDayCostStudent + B2bStCost + SdStCost,
    TCost is BuildTCost + LBTcost + PeriodCost + OnDayCostTeacher + B2bTCost + SdTCost.

% Calculate the cost for a schedule
cost(schedule(Events), Cost) :-
    is_valid(schedule(Events)), !, % If not valid don't bother
    cost_loop(Events, StCost, TCost), % Calculate the cost for students and Exams independently
    findall(S, student(S, _), Students),
    findall(S, lecturer(S, _), Teachers),
    length(Students, NumSt),
    length(Teachers, NumT),
    % Calculate sum as given in the assignment
    Cost is (StCost/(NumSt*2)) + (TCost/NumT).
