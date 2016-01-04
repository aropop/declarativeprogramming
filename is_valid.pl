:- module(is_valid, [is_valid/1]).
:- use_module(util).

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

% Check if a single room is available at a certain moment
is_available(Room, Day, Start, Duration) :-
    End is Start + Duration,
    availability(Room, Day, ForseenSt, ForseenEnd),
    ForseenEnd >= End,
    Start >= ForseenSt.

% Loop over all rooms to check whether they are available
check_availability(schedule([])).
check_availability(schedule([event(E, R, D, H)|Elist])) :-
    duration(E, Dur),
    is_available(R, D, H, Dur),
    check_availability(schedule(Elist)).

% Check 2 exams same time in same room
check_two_exams_same_room(schedule([])).
check_two_exams_same_room(schedule([H|T])) :-
    fit_lst(H, T), % Check if the exam fits against the other exams in the list
    check_two_exams_same_room(schedule(T)).


% Check for a group of students and a teacher if there is an exam at the same time
check_against_others(_, _, [], _, _, _). % Rec end
check_against_others(Students, Teacher, [event(Ex, _, Day, Start2)|Events], Start, End, Day) :- % Unifies if days are the same
    % Check overlap
    duration(Ex, Dur),
    has_exam(Course, Ex),
    End2 is Start2+Dur,
    overlap(Start, End, Start2, End2), % Overlaps, check if same student or teacher
    findall(S, follows(S, Course), Students2),
    intersection(Students, Students2, Ints), % intersect original group of students and students of exam checked
    length(Ints, L),
    L == 0, % The intersect should have no students
    teaches(Teacher2, Course),
    Teacher2 \== Teacher, % Teacher of these exams should be different
    check_against_others(Students, Teacher, Events, Start, End, Day). % check recursively
% Not overlapping (Exams are not held at the same time), we should still continue the recursion to check against all other exams
check_against_others(Students, Teacher, [event(Ex, _, Day1, Start1)|Events], Start2, End2, Day2) :-
    ((Day1 \== Day2); % Only unify if not overlapping
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
