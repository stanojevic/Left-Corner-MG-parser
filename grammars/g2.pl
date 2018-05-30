%   File   : g2.pl
%   Author : E Stabler
%   Updated: Mar 00
%   This grammar uses remnant movement to define a non-context-free
%      copy language { bot X X top| X \in {a,b}* }

[bot] :: ['T',-r,-l].        [top] :: [='T',+r,+l,'T'].
[a] :: [='T',+r,'A',-r].     [b] :: [='T',+r,'B',-r].
[a] :: [='A',+l,'T',-l].     [b] :: [='B',+l,'T',-l].

startCategory('T').

/*

?- parse([bot,a,a,top],L).
L = [shift([bot], ['T', -r, -l]), lc2(merge3), shift([a], [='A', +l, 'T', -l]), lc1(merge3), c(shift([a], [='T', +r, 'A', -r])), c(lc1(move2)), lc1(move1), lc2(merge3), c(shift([top], [='T', +r, +l, 'T'])), lc1(move1), lc1(move1)] 

?- parse([bot,a,b,a,b,top],L).
L = [shift([bot], ['T', -r, -l]), lc2(merge3), shift([a], [='A', +l, 'T', -l]), lc1(merge3), shift([b], [='B', +l, 'T', -l]), lc1(merge3), c(shift([a], [='T', +r, 'A', -r])), c(lc1(move2)), lc1(move1), lc2(merge3), c(shift([b], [='T', +r, 'B', -r])), c(lc1(move1)), lc1(move1), lc2(merge3), c(shift([top], [='T', +r, +l, 'T'])), lc1(move1), lc1(move1)] 

parse([bot,a,a,top],[shift([bot], ['T', -r, -l]), lc2(merge3), shift([a], [='A', +l, 'T', -l]), lc1(merge3), c(shift([a], [='T', +r, 'A', -r])), c(lc1(move2)), lc1(move1), lc2(merge3), c(shift([top], [='T', +r, +l, 'T'])), lc1(move1), lc1(move1)]).

parse([bot,a,b,a,b,top],[shift([bot], ['T', -r, -l]), lc2(merge3), shift([a], [='A', +l, 'T', -l]), lc1(merge3), shift([b], [='B', +l, 'T', -l]), lc1(merge3), c(shift([a], [='T', +r, 'A', -r])), c(lc1(move2)), lc1(move1), lc2(merge3), c(shift([b], [='T', +r, 'B', -r])), c(lc1(move1)), lc1(move1), lc2(merge3), c(shift([top], [='T', +r, +l, 'T'])), lc1(move1), lc1(move1)]).

*/
