:- use_module(library(apply)).

% Deletes one element from a list
delete_one(_, [], []).
delete_one(Term, [Term|Tail], Tail).
delete_one(Term, [Head|Tail], [Head|Result]) :-
    delete_one(Term, Tail, Result),
    !.

% Sums a list
list_sum([], 0).
list_sum([Item], Item).
list_sum([Item1,Item2 | Tail], Total) :-
    list_sum([Item1+Item2|Tail], Total).

% Build/3 +Val, +N, -List
% Builds a list of length N with all elements equal to Val
build(_,0,[]).
build(X,N1,[X|L]) :-
    N1>0,
    N is N1 - 1,
    build(X,N,L).

% take/5 take(-From, -To, -FromList, +Taken, +Rest)
take(0, 0, _, []).
take(0, N, [X|L], [X|O]) :-
    N1 is N - 1,
    take(0, N1, L, O).
take(F, N, [_|Xs], O) :-
    F1 is F - 1,
    N1 is N - 1,
    take(F1, N1, Xs, O).

% converts en atom to a number so it can be compared
exam_to_num(Exam, Num) :-
    atom_codes(Exam, [_|Codes]),
    list_sum(Codes, Num).

% Conversions for schedules, only converts if Start is an integer
schedule_to_exam_lst(schedule([]), []).
schedule_to_exam_lst(schedule([event(E, _, _, St)|Tail]), [E|Res]) :-
    integer(St),
    schedule_to_exam_lst(schedule(Tail), Res),
    !.

% Converts a schedule to a list of rooms
schedule_to_room_lst(schedule([]), []).
schedule_to_room_lst(schedule([event(_, R, _, _)|Tail]), [R|Res]) :-
    schedule_to_room_lst(schedule(Tail), Res).
% Invert parameter order for maplist
has_exam_inv(E,C) :- has_exam(C,E).
% Converts a schedule to a list of Courses
schedule_to_course_lst(Schedules, Courses) :- schedule_to_exam_lst(Schedules, Exams),
                                              maplist(has_exam_inv, Exams,  Courses).
% Checks if all exams are in a schedule and wether it consists of integer start hours
check_all(Schedule) :- schedule_to_exam_lst(Schedule, Lst),
                       !,
                       findall(Exam, exam(Exam,_), Exams),
                       permutation(Exams, Lst).


% Checks if an element is only once in a list
is_scheduled_once(Exam, Lst) :- delete_one(Exam, Lst, R),
                                not(member(Exam, R)).
% Takes two lists, Takes an element of the first one and checks if it is only
% Scheduled once. The second list is thus a list of all exams
schedule_map([], _).
schedule_map([El|Tail], Total) :- is_scheduled_once(El, Total),
                                  schedule_map(Tail, Total),
                                  !.
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
    overlap(Start, End, Start2, End2), % Overlaps, check if same student or teacher
    findall(S, follows(S, Course), Students2),
    intersection(Students, Students2, Ints),
    length(Ints, L),
    L == 0,
    teaches(Teacher2, Course),
    Teacher2 \== Teacher,
    check_against_others(Students, Teacher, Events, Start, End, Day).
check_against_others(Students, Teacher, [event(Ex, _, Day1, Start1)|Events], Start2, End2, Day2) :- % Not overlapping
    ((Day1 \== Day2);
     (Day1 == Day2,
      duration(Ex, Dur),
      End1 is Start1 + Dur,
      not(overlap(Start2, End2, Start1, End1)))),
    %Still have to go deeper n the recursion (might overlap with others)
    check_against_others(Students, Teacher, Events, Start2, End2, Day2).

% Checks wether 2 exams are at the same time
check_same_time(schedule([])).
check_same_time(schedule([event(Ex, _, Day, Start)|Tail])) :-
    % Get the info about the first exam
    has_exam(Course, Ex),
    duration(Ex, Dur),
    teaches(Teacher, Course),
    findall(S, follows(S, Course), Students),
    End is Start+Dur,
    % Check with the other exams in the list
    check_against_others(Students, Teacher, Tail, Start, End, Day),
    % Go deeper in the recursion
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

% Calculate cost for a lunchbreak both for teachers and students
lunch_break_cost(Students, Teacher, TCost, SCost) :-
    sc_lunch_break(Teacher, TCost),
    maplist(sc_lunch_break, Students, PLst),
    list_sum(PLst, SCost).

% Calculate the cost for a given teacher in a period
period_cost(Teacher, St, End, Day, Cost) :-
    sc_no_exam_in_period(Teacher, Day, St2, End2, Cost),
    overlap(St, End, St2, End2).
period_cost(_, _, _, _, 0). % If cannot unify with above it's null

% Not in period soft constraint cost
exam_on_day_cost([], _, _, _, _, 0). % End recursion
exam_on_day_cost([Pid|Persons], Exam, Day, Start, End, Cost) :-
    sc_not_in_period(Pid, Exam, Day, Start2, End2, Penalty),
    overlap(Start, End, Start2, End2),
    exam_on_day_cost(Persons, Exam, Day, Start, End, BuildCost),
    Cost is Penalty + BuildCost. % Build in backtracking
exam_on_day_cost([_|Persons], Exam, Day, Start, End, Cost) :-
    % Cannot unify so go through with next person
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
    % Calculate all b2b and same day exams
    b2b_exam(event(Ex, Rm, Day, St), Evnts, B2BExams, SameDayExams), !,
    % Calculate the cost for each soft constraint
    ((overlap(St, End, 12, 13), % If it overlaps lunchbreak, calculate cost
      lunch_break_cost(Students, Teacher, LBTcost, LBScost));
     (LBTcost is 0, % Otherwise lunch break cost is 0
      LBScost is 0)),
    period_cost(Teacher, St, End, Day, PeriodCost),
    exam_on_day_cost(Students, Ex, Day, St, End, OnDayCostStudent),
    exam_on_day_cost([Teacher], Ex, Day, St, End, OnDayCostTeacher),
    b2b_student_cost(Students, B2BExams, B2bStCost),
    b2b_teacher_cost(Teacher, B2BExams, B2bTCost),
    same_day_student_cost(Students, SameDayExams, SdStCost),
    same_day_teacher_cost(Teacher, SameDayExams, SdTCost),
    % Go deeper in recursion
    cost_loop(Evnts, BuildStcost, BuildTCost),
    % Sum the costs from this iteration and the previously calculated ones
    StCost is BuildStcost + LBScost + OnDayCostStudent + B2bStCost + SdStCost,
    TCost is BuildTCost + LBTcost + PeriodCost + OnDayCostTeacher + B2bTCost + SdTCost.

% Helpers for correction loop (see below)
% Sets off free indices
set_off(L, 0, L, 0).
set_off([], Days, [], Days).
set_off([1|Lst], Days, [0|OLst], RDay) :-
    D1 is Days - 1,
    D1 >= 0,
    set_off(Lst, D1, OLst, RDay).
set_off([0|Lst], Days, [0|OLst], RDay) :-
    set_off(Lst, Days, OLst, RDay).

% Delays the setting off of indeices
set_off_from(0, Lst, Num, Res, ResDays) :- set_off(Lst, Num, Res, ResDays).

set_off_from(N, [El|Lst], Num, [El|Res], ResDays) :-
    N1 is N - 1,
    N1 >= 0,
    set_off_from(N1, Lst, Num, Res, ResDays).


% Keep a FreeList form length End-St, each index represents a day in the exam
% period, The list is initialized on 1's as being a free day to correct.
% Each exam on a day will set the amount of days need to correct beyond the inddex
% to zero. If no days are left the penalty will be increased.
% Eg 5 days exam
% [1,1,1,1,1], 0
% Exam on day 3 takes 2 days to correct
% [1,1,1,0,0], 0
% Exam on day 1 Takes 1 day to correct
% [1,0,1,0,0], 0
% Exam on day 3 takes 3 days to correct
% [1,0,1,0,0] 3*penalty
correction_loop([], St, End, FreeList, 0) :-
    Len is (End - St)+1,
    build(1, Len, FreeList).
correction_loop([event(Ex, _, Day, _)|Exs], St, End, FreeList, Penalty) :-
    sc_correction_time(Ex, CorT),
    has_exam(Course, Ex),
    teaches(Teacher, Course),
    correction_loop(Exs, St, End, BuiltFreeList, BuiltPenalty),
    %DayIdx is Day -1,
    %set_off_from(DayIdx, BuiltFreeList, CorT, FreeList, ResDays),
    set_off_from(Day, BuiltFreeList, CorT, FreeList, ResDays),
    sc_correction_penalty(Teacher, Cost),
    Penalty is (ResDays*Cost) + BuiltPenalty.

findall_events_for_a_teacher(Events, Teacher, ExamEvents) :-
    findall(Ev, (teaches(Teacher, Course), has_exam(Course, Ex), member(event(Ex,Rm,Dy,St), Events), Ev = event(Ex, Rm, Dy, St)), ExamEvents).
day_correction_loop(EvLst, Penalty) :-
    first_day(St),
    last_day(End),
    correction_loop(EvLst, St, End, _, Penalty).

% Similar to the correction loop but we have to reverse the freelist and the days
study_loop([], _, St, End, FreeList, 0) :-
    correction_loop([], St, End, FreeList, 0).
study_loop([event(Ex, _, Day, _)|Exs], Student, Start, End, FreeList, Penalty) :-
    sc_study_time(Ex, StudyT),
    study_loop(Exs, Student,  Start, End, BuiltFreeList, BuiltPenalty), !,
    reverse(BuiltFreeList, FlippedFreeList),
    DayReversed is (End - Day) + 1,
    DayIdx is DayReversed,
    set_off_from(DayIdx, FlippedFreeList, StudyT, ToBeFlippedFreeList, ResDays),
    reverse(ToBeFlippedFreeList, FreeList),
    sc_study_penalty(Student, Cost),
    Penalty is (ResDays*Cost) + BuiltPenalty.

% findall exam events for a student in a list of events
findall_events_for_a_student(Events, Student, ExamEvents) :-
    findall(Ev, (follows(Student, Course), has_exam(Course, Ex), member(event(Ex,Rm,Dy,St), Events), Ev = event(Ex, Rm, Dy, St)), ExamEvents).
% Fill in the days so it can be used in maplist
day_study_loop(EvLst, Student, Penalty) :-
    first_day(Start),
    last_day(End),
    study_loop(EvLst, Student, Start, End, _, Penalty).

% Calculate the costs for soft contstraints where we dont have to loop over the
% Events traditionally. (correction time and study time)
cost_no_loop(Events, StCost, TCost) :-
    % For each teacher, find his exams and calculate the correction time penalty
    findall(T, lecturer(T, _), Teachers),
    maplist(findall_events_for_a_teacher(Events), Teachers, Exams),
    maplist(day_correction_loop, Exams, Penalties),
    list_sum(Penalties, TCost), % Sum penalties
    % For each student, find his exams and calculate the study time penalty
    findall(S, student(S, _), Students),
    maplist(findall_events_for_a_student(Events), Students, SExams),
    maplist(day_study_loop, SExams, Students, Penalties2),
    list_sum(Penalties2, StCost) % Sum Penalties
    .


% Calculate the cost for a schedule
cost(schedule(Events), Cost) :-
    is_valid(schedule(Events)), !, % If not valid don't bother
    cost_loop(Events, StCost, TCost), % Calculate the cost for students and Exams independently
    cost_no_loop(Events, StCost2, TCost2),
    findall(S, student(S, _), Students),
    findall(S, lecturer(S, _), Teachers),
    length(Students, NumSt),
    length(Teachers, NumT),
    TotStCost is StCost + StCost2,
    TotTCost is TCost + TCost2,
    % Calculate sum as given in the assignment
    Cost is ((TotStCost/(NumSt)) + (TotTCost/NumT))/2.



% Generate solutions
gen_schedule(Schedule) :-
    findall(E, exam(E, _), Exams),
    maplist(gen_event, Exams, Events),
    Schedule = schedule(Events).
gen_event(Ex, Event) :-
    room(Room, _),
    first_day(FDay),
    last_day(LDay),
    between(FDay, LDay, Day),
    availability(Room, Day, SHour, EHour), % Make sure to no go through every possible hour 1-24
    between(SHour, EHour, Hour),
    Event = event(Ex, Room, Day, Hour).

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
