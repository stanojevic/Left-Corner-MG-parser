%   File   : g3dt.pl like g3.pl but with derivation tree arguments
%   Author : Milos Stanojevic
%   This grammar for "one two three four five" uses lc1(merge2)

[one] :: [=b(B), a('*'/['one::=b a'/[],B])].
[two] :: [=d(D), =a(A), c('*'/[A,'*'/['two::=d =a c -w'/[],D]]), -w].
[three] :: [d('three::d'/[])].
[four] :: [b('four::b -k'/[]), -k].
[five] :: [=c(C), +k, +w, e(o/[o/['*'/['five::=c +k +w e'/[],C]]])].

startCategory(e(_)).

%%%%%% EXAMPLES
% parse([one,two,three,four,five], L).
