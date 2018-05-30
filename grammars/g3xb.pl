%   File   : g3xb.pl like g3.pl but with X-bar derived tree arguments
%   Author : Milos Stanojevic
%   This grammar for "one two three four five" uses lc1(merge2)

[one] :: [=b(B), a(aP/[a/[one/[]],B])].
[two] :: [=d(D), =a(A), c(cP(I)/[]), -w(cP(I)/[A,'c'''/[c/[two/[]],D]])].
[three] :: [d(dP/[three/[]])].
[four] :: [b(bP(I)/[]), -k(bP(I)/[b/[four/[]]])].
[five] :: [=c(C), +k(K), +w(W), e(eP/[W,'e'''/[K,'e'''/[e/[five/[]],C]]])].

startCategory(e(_)).

%%%%%% EXAMPLES
% parse([one,two,three,four,five], L).
