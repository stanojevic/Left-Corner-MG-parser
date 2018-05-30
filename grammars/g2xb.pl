%   File   : g2xb.pl = g2 but with X-bar tree arguments
%   Author : E Stabler
%   Updated: Mar 00
%   This grammar uses remnant movement to define a non-context-free, copy-like language,
%   using tree arguments to produce an explicit derivation tree

[bot] :: ['T'('TP'(I)/[]),-r('TP'(I)/[]),-l('TP'(I)/[bot/[]])].
[top] :: [='T'(T),+r(R),+l(L),'T'('TP'/[L,'T'''/[R,'T'''/['T'/[top/[]],T]]])].
[a] :: [='T'(T),+r(R),'A'('AP'(I)/[]),-r('AP'(I)/[R,'A'''/['A'/[a/[],T]]])].
[b] :: [='T'(T),+r(R),'B'('BP'(I)/[]),-r('BP'(I)/[R,'B'''/['B'/[b/[],T]]])].
[a] :: [='A'(A),+l(L),'T'('TP'(I)/[]),-l('TP'(I)/[L,'T'''/['T'/[a/[],A]]])].
[b] :: [='B'(B),+l(L),'T'('TP'(I)/[]),-l('TP'(I)/[L,'T'''/['T'/[b/[],B]]])].

startCategory('T'(_)).

/*

?- parse([bot,a,a,top],L).

?- parse([bot,a,b,a,b,top],L).

*/
