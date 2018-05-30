%   File   : g2dt.pl = g2 but with derivation tree arguments
%   Author : E Stabler
%   Updated: Mar 00
%   This grammar uses remnant movement to define a non-context-free copy language,
%   using tree arguments to produce an explicit derivation tree

[bot] :: ['T'('bot::T -r -l'/[]),-r,-l].
[top] :: [='T'(T),+r,+l,'T'(o/[o/['*'/['top::=T +r +l'/[],T]]])].
[a] :: [='T'(T),+r,'A'(o/['*'/['a::=T +r A -r'/[],T]]),-r].
[b] :: [='T'(T),+r,'B'(o/['*'/['b::=T +r B -r'/[],T]]),-r].
[a] :: [='A'(A),+l,'T'(o/['*'/['a::=A +l T -l'/[],A]]),-l].
[b] :: [='B'(B),+l,'T'(o/['*'/['b::=B +l T -l'/[],B]]),-l].

startCategory('T'(_)).

/*

?- parse([bot,a,a,top],L).

?- parse([bot,a,b,a,b,top],L).

*/
