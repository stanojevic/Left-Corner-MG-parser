%   File   : g5.pl
%   Author : Milos Stanojevic

[one] :: [=b, a].
[two] :: [=d, =a, c, -w].
[three] :: [d].
[four] :: [b, -k].
[five] :: [=c, +k, +w, e].

startCategory(e).

%%%%%% EXAMPLES
% parse([one,two,three,four,five], L).
% parse([one,two,three,four,five],[shift(A,B),lc1(merge3),shift(C,D),lc1(merge1),c(shift(E,F)),c2(lc1(merge2)),c(shift(G,H)),lc2(merge3),c(shift(I,J)),lc1(move1),lc1(move1)]).
