%   File   : g2.pl
%   Author : E Stabler
%   Updated: Mar 00
%      grammar for the copy language {XX| X\in{a,b}*}

[] :: ['T',-r,-l].            [] :: [='T',+r,+l,'T'].
[a] :: [='T',+r,'A',-r].      [b] :: [='T',+r,'B',-r].
[a] :: [='A',+l,'T',-l].      [b] :: [='B',+l,'T',-l].

startCategory('T').

/*

parse([a,a],[shift([], ['T', -r, -l]), lc2(merge3), shift([a], [='A', +l, 'T', -l]), lc1(merge3), c(shift([a], [='T', +r, 'A', -r])), c(lc1(move2)), lc1(move1), lc2(merge3), c(shift([], [='T', +r, +l, 'T'])), lc1(move1), lc1(move1)]).

parse([a,b,a,b],[shift([], ['T', -r, -l]), lc2(merge3), shift([a], [='A', +l, 'T', -l]), lc1(merge3), shift([b], [='B', +l, 'T', -l]), lc1(merge3), c(shift([a], [='T', +r, 'A', -r])), c(lc1(move2)), lc1(move1), lc2(merge3), c(shift([b], [='T', +r, 'B', -r])), c(lc1(move1)), lc1(move1), lc2(merge3), c(shift([], [='T', +r, +l, 'T'])), lc1(move1), lc1(move1)]).

parse([a,b,b,a,b,b],[shift([], ['T', -r, -l]), lc2(merge3), shift([a], [='A', +l, 'T', -l]), lc1(merge3), shift([b], [='B', +l, 'T', -l]), lc1(merge3), shift([b], [='B', +l, 'T', -l]), lc1(merge3), c(shift([a], [='T', +r, 'A', -r])), c(lc1(move2)), lc1(move1), lc2(merge3), c(shift([b], [='T', +r, 'B', -r])), c(lc1(move1)), lc1(move1), lc2(merge3), c(shift([b], [='T', +r, 'B', -r])), c(lc1(move1)), lc1(move1), lc2(merge3), c(shift([], [='T', +r, +l, 'T'])), lc1(move1), lc1(move1)]) .

*/
