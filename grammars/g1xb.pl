%   File   : g1xb.pl
%   Author : E Stabler and M Stanojevic
%   This is the same as grammar fg18.pl, except that each syntactic
%    feature carries an X-bar tree argument.
%   Note also that the licensees and licensors also pass an argument
%    from one to the other.

    % These tree arguments could be *generated* from the grammar,
    % but here I insert them by hand.

[] :: [=v(V),c('CP'/['C'/[e/[],V]])].
[] :: [=v(V),+wh(WH),c('CP'/[WH,'C'''/['C'/[e/[]],V]])].
['Aca'] :: [d('DP'/['Aca'/[]])].
['Bibi'] :: [d('DP'/['Bibi'/[]])].
[knows] :: [=c(C),=d(D),v('VP'/[D,'V'''/['V'/[knows/[]],C]])].
[likes] :: [=d(D1),=d(D2),v('VP'/[D2,'V'''/['V'/[likes/[]],D1]])].
[what] :: [d('DP'(TraceIndex)/[]),-wh('DP'(TraceIndex)/[what/[]])].
[and] :: [=c(C1),=c(C2),c('CP'/[C2,'C'''/[and/[],C1]])].

startCategory(c(_)).

%%%%%% EXAMPLES

% parse(['Bibi',likes,'Aca'],[shift([],[=v(_),c(_)]),lc1(merge1),shift(['Bibi'],_),c1(lc2(merge2)),shift([likes],_),c1(lc1(merge1)),c(shift(['Aca'],_))]).

% parse([what,'Aca',likes],[shift([what],_),lc2(merge3),shift([],[=v(_),+wh(_),c(_)]),lc1(merge1),shift(['Aca'],_),c3(lc2(merge2)),c(shift([_],_)),lc1(move1)]).

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
                     shift([],[=v(_),+wh(_),c(_)]),
                     lc1(merge1),
                     shift(['Aca'],_),
                     c3(lc2(merge2)),
                     c(shift([_],_)),
                     c(lc1(move1))]).
*/
