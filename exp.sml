datatype EXP = k of int | plus of EXP * EXP | times of EXP * EXP;

k;
plus;
times;

fun eval (k x) = x | eval (plus (x, y)) = eval x + eval y | eval (times (x, y)) = eval x * eval y;