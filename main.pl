is_valid(Schedule) :- eachhardconstraint(Schedule).
cost(Scedule, Cost) :- Cost is eachsoftconstraint(Schedule, Penalty).
