:- module(util, [delete_one/3, list_sum/2, build/3, take/4, exam_to_num/2, event_compare/3, schedule_to_exam_lst/2, schedule_to_course_lst/2, schedule_to_room_lst/2, overlap/4, has_exam_inv/2, fit_lst/2, fit/2]).

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

% Compare 2 events
event_compare(C, event(E1, _, Day1, Start1), event(E2, _, Day2, Start2)) :-
    (compare(C, Day1, Day2), C \= = );
    (compare(C, Start1, Start2), C \= =);
    (exam_to_num(E1, Num1), exam_to_num(E2, Num2), compare(C, Num1, Num2)).

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

% Converts a schedule to a list of Courses
schedule_to_course_lst(Schedules, Courses) :- schedule_to_exam_lst(Schedules, Exams),
                                              maplist(has_exam_inv, Exams,  Courses).


% Check if 2 times overlap
overlap(Start1, End1, Start2, End2) :-
  ((Start1 =< Start2, Start2 < End1);
   (Start2 =< Start1, Start1 < End2)).
overlap(event(E1, R, D, H1), event(E2, R, D, H2)) :- duration(E1, Dur1),
                                                   duration(E2, Dur2),
                                                   End1 is H1+Dur1,
                                                   End2 is H2+Dur2,
                                                   overlap(H1, End1, H2, End2).

% Checks if 2 events fit
fit(E1, E2) :- not(overlap(E1, E2)).

% Checks if an event fits in a list
fit_lst(_, []).
fit_lst(E1, [E2|Rst]) :-
    fit(E1, E2),
    fit_lst(E1, Rst).

% Inverse predicate of has_exam
has_exam_inv(A, B) :- has_exam(B, A).