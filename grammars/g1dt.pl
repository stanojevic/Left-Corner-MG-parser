%   File   : g1dt.pl
%   Author : E Stabler and M Stanojevic
%   This is the same as grammar g1.pl, except that each selector/selectee
%    feature carries a derivation tree argument.
%   Note also that the licensees and licensors do not have arguments.

    % These tree arguments could be *generated* from the grammar,
    % but here I insert them by hand.

[] :: [=v(V),c('*'/['e::=v c'/[],V])].
[] :: [=v(V),+wh,c('o'/['*'/['e::=v c'/[],V]])].
['Aca'] :: [d('Aca::d'/[])].
['Bibi'] :: [d('Bibi::d'/[])].
[knows] :: [=c(C),=d(D),v('*'/['*'/['knows::=c =d v'/[],C],D])].
[likes] :: [=d(D1),=d(D2),v('*'/['*'/['likes::=d =d v'/[],D1],D2])].
[what] :: [d('what::d -wh'/[]),-wh].
[and] :: [=c(C1),=c(C2),c('*'/['*'/[and/[],C1],C2])].

startCategory(c(_)).

%%%%%% EXAMPLES
% parse(['Bibi',likes,'Aca'],[shift([],[=v(_),c(_)]),lc1(merge1),shift(['Bibi'],_),c1(lc2(merge2)),shift([likes],_),c1(lc1(merge1)),c(shift(['Aca'],_))]).

% parse([what,'Aca',likes],[shift([what],_),lc2(merge3),shift([],[=v(_),+wh,c(_)]),lc1(merge1),shift(['Aca'],_),c3(lc2(merge2)),c(shift([_],_)),lc1(move1)]).

/*
parse(['Bibi',knows,what,'Aca',likes],[
                     shift([],[=v(_),c(_)]),
                     lc1(merge1),
                     shift(['Bibi'],_),
                     c1(lc2(merge2)),
                     shift([knows],_),
                     c1(lc1(merge1)),
                     shift([what],_),
                     lc2(merge3),
                     shift([],[=v(_),+wh,c(_)]),
                     lc1(merge1),
                     shift(['Aca'],_),
                     c3(lc2(merge2)),
                     c(shift([_],_)),
                     c(lc1(move1))]).
*/
