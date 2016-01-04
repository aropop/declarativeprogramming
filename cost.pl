:- module(cost, [cost/2]).
:- use_module([is_valid, util]).

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


% finds all b2b exams and samedayexams
b2b_exam(event(Ex, _, Day, Start), Events, B2BExams, SameDayExams) :-
    duration(Ex, Dur),
    End is Start + Dur,
    findall(Y, member(event(Y, _, Day, _), Events), SameDayExams), !,
    findall(X, (member(event(X, _, Day, End), Events); member(event(X, _, Day, MySt), Events), duration(X, MyDur), Start is MySt + MyDur), B2BExams).

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
    TCost is BuildTCost + LBTcost + PeriodCost + OnDayCostTeacher + B2bTCost + SdTCost,
    !.


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
