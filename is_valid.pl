:- module(is_valid, [is_valid/1]).
:- use_module(util).



% Checks if all exams are in a schedule and whether they are only scheduled once
check_all(schedule(Events)) :-
    findall(Exam, exam(Exam,_), Exams), % Makes sure only to get all exams once
    length(Exams, L), % Make sure length of lists is equal
    length(Lst, L),
    maplist(exam_from_event, Events, Lst),
    permutation(Exams, Lst). % Enforces that all exams are scheduled and scheduled only once


% Checks if the scheduled rooms can fit the people in the courses
check_capacity(schedule([])). % No events is always true
check_capacity(schedule([event(Ex, Room, _, _) | Evnts])) :-
    has_exam(Course, Ex),
    capacity(Room, Cap),
    getstudents(Course, Students), % Find all students for this course
    length(Students, Numstudents),
    Cap >= Numstudents, % Check if the amount of students fit the capacity
    check_capacity(schedule(Evnts)). % Check recursively


% Loop over all rooms to check whether they are available
check_availability(schedule([])).
check_availability(schedule([event(E, R, D, H)|Elist])) :-
    last_day(LastDay),
    first_day(FirstDay),
    between(FirstDay, LastDay, D), % Day should lie within boundaries
    duration(E, Dur), % Room should be available during the whole session
    availability(R, D, ForseenSt, ForseenEnd),
    Difference is ForseenEnd - Dur,
    between(ForseenSt, Difference, H),
    check_availability(schedule(Elist)).


% Check for a group of students, a teacher and a room if there is an exam at the same time
check_against_others(_, _, _, [], _, _, _). % Rec end
check_against_others(Students, Teacher, Room, [event(Ex, OtherRoom, Day, Start2)|Events], Start, End, Day) :- % Unifies if days are the same
    % Check overlap
    duration(Ex, Dur),
    has_exam(Course, Ex),
    End2 is Start2+Dur,
    overlap(Start, End, Start2, End2), % Overlaps, check if same student or teacher or room
    Room \== OtherRoom,
    teaches(Teacher2, Course),
    Teacher2 \== Teacher, % Teacher of these exams should be different
    getstudents(Course, Students2),
    intersection(Students, Students2, Ints), % intersect original group of students and students of exam checked
    length(Ints, L),
    L == 0, % The intersect should have no students
    check_against_others(Students, Teacher, Room, Events, Start, End, Day). % check recursively
% Not overlapping (Exams are not held at the same time), we should still continue the recursion to check against all other exams
check_against_others(Students, Teacher, Room, [event(Ex, _, Day1, Start1)|Events], Start2, End2, Day2) :-
    ((Day1 \== Day2); % Only unify if not overlapping
     (Day1 == Day2,
      duration(Ex, Dur),
      End1 is Start1 + Dur,
      not(overlap(Start2, End2, Start1, End1)))),
    !,
    %Still have to go deeper in the recursion (others might overlap with others)
    check_against_others(Students, Teacher, Room, Events, Start2, End2, Day2).


% Checks wether 2 exams are at the same time
check_same_time(schedule([])).
check_same_time(schedule([event(Ex, Room, Day, Start)|Tail])) :-
    % Get the info about the first exam
    has_exam(Course, Ex),
    duration(Ex, Dur),
    teaches(Teacher, Course),
    getstudents(Course, Students),
    %findall(S, follows(S, Course), Students),
    End is Start+Dur,
    % Check with the other exams in the list
    check_against_others(Students, Teacher, Room, Tail, Start, End, Day),
    % Go deeper in the recursion
    check_same_time(schedule(Tail)).

% Do least demanding tests first
is_valid(Schedule) :- check_all(Schedule),
                      check_capacity(Schedule),
                      !,
                      check_availability(Schedule),
                      check_same_time(Schedule).
