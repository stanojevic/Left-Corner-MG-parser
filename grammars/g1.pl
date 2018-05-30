%   File   : g1.pl
%   Author : Milos Stanojevic

[] :: [=v,c].
[] :: [=v,+wh,c].
['Aca'] :: [d].
['Bibi'] :: [d].
[knows] :: [=c,=d,v].
[likes] :: [=d,=d,v].
[what] :: [d,-wh].
[and] :: [=c,=c,c].

startCategory(c).

%%%%%% EXAMPLES
% parse(['Bibi',likes,'Aca'],[shift([],[=v,c]),lc1(merge1),shift(['Bibi'],_),c1(lc2(merge2)),shift([likes],_),c1(lc1(merge1)),c(shift(['Aca'],_))]).

%?- parse(['Bibi',knows,what,'Aca',likes],L).
%L = [shift([], [=v, c]), lc1(merge1), shift(['Bibi'], [d]), c1(lc2(merge2)), shift([knows], [=c, =d, v]), c1(lc1(merge1)), shift([what], [d, -wh]), lc2(merge3), shift([], [=v, +wh, c]), lc1(merge1), shift(['Aca'], [d]), c3(lc2(merge2)), c(shift([likes], [=d, =d, v])), c(lc1(move1))] .

%?- parse(['Bibi',knows,what,'Aca',knows,'Bibi',knows,'Aca',likes],L).
%L = [shift([], [=v, c]), lc1(merge1), shift(['Bibi'], [d]), c1(lc2(merge2)), shift([knows], [=c, =d, v]), c1(lc1(merge1)), shift([what], [d, -wh]), lc2(merge3), shift([], [=v, +wh, c]), lc1(merge1), shift(['Aca'], [d]), c1(lc2(merge2)), shift([knows], [=c, =d, v]), c1(lc1(merge1)), shift([], [=v, c]), c1(lc1(merge1)), shift(['Bibi'], [d]), c1(lc2(merge2)), shift([knows], [=c, =d, v]), c1(lc1(merge1)), shift([], [=v, c]), c1(lc1(merge1)), shift(['Aca'], [d]), c3(lc2(merge2)), c(shift([likes], [=d, =d, v])), c(lc1(move1))] .

% parse([what,'Aca',likes],[shift([what],_),lc2(merge3),shift([],[=v,+wh,c]),lc1(merge1),shift(['Aca'],_),c3(lc2(merge2)),c(shift([_],_)),lc1(move1)]).

/*
parse(['Bibi',knows,what,'Aca',likes],[
                     shift([],[=v,c]),
                     lc1(merge1),
                     shift(['Bibi'],_),
                     c1(lc2(merge2)),
                     shift([knows],_),
                     c1(lc1(merge1)),
                     shift([what],_),
                     lc2(merge3),
                     shift([],[=v,+wh,c]),
                     lc1(merge1),
                     shift(['Aca'],_),
                     c3(lc2(merge2)),
                     c(shift([_],_)),
                     c(lc1(move1))]).

1. shift([],[=v,c]) [Bibi,knows,what,Aca,likes]
    0,0,(::),[=v,c],[]
2. lc1(merge1) [Bibi,knows,what,Aca,likes]
    0,_16102,_16108,[v],_16116->0,_16102,(:),[c],_16116
3. shift([Bibi],[d]) [knows,what,Aca,likes]
    0,1,(::),[d],[]
    0,_16102,_16108,[v],_16116->0,_16102,(:),[c],_16116
4. c1(lc2(merge2)) [knows,what,Aca,likes]
    1,_16102,(:),[=d,v],_16116->0,_16102,(:),[c],_16116
5. shift([knows],[=c,=d,v]) [what,Aca,likes]
    1,2,(::),[=c,=d,v],[]
    1,_16102,(:),[=d,v],_16116->0,_16102,(:),[c],_16116
6. c1(lc1(merge1)) [what,Aca,likes]
    2,_16102,_16480,[c],_16116->0,_16102,(:),[c],_16116
7. shift([what],[d,-wh]) [Aca,likes]
    2,3,(::),[d,-wh],[]
    2,_16102,_16480,[c],_16116->0,_16102,(:),[c],_16116
8. lc2(merge3) [Aca,likes]
    _16662,_16668,_16674,[=d,v],_16682->_16662,_16668,(:),[v],[(2,3,[-wh])|_16682]
    2,_16102,_16480,[c],_16116->0,_16102,(:),[c],_16116
9. shift([],[=v,+wh,c]) [Aca,likes]
    3,3,(::),[=v,+wh,c],[]
    _16662,_16668,_16674,[=d,v],_16682->_16662,_16668,(:),[v],[(2,3,[-wh])|_16682]
    2,_16102,_16480,[c],_16116->0,_16102,(:),[c],_16116
10. lc1(merge1) [Aca,likes]
    3,_16930,_16936,[v],_16944->3,_16930,(:),[+wh,c],_16944
    _16662,_16668,_16674,[=d,v],_16682->_16662,_16668,(:),[v],[(2,3,[-wh])|_16682]
    2,_16102,_16480,[c],_16116->0,_16102,(:),[c],_16116
11. shift([Aca],[d]) [likes]
    3,4,(::),[d],[]
    3,_16930,_16936,[v],_16944->3,_16930,(:),[+wh,c],_16944
    _16662,_16668,_16674,[=d,v],_16682->_16662,_16668,(:),[v],[(2,3,[-wh])|_16682]
    2,_16102,_16480,[c],_16116->0,_16102,(:),[c],_16116
8. lc2(merge3) [Aca,likes]
    _16662,_16668,_16674,[=d,=d,v],_16682->_16662,_16668,(:),[=d,v],[(2,3,[-wh])|_16682]
    2,_16102,_16480,[c],_16116->0,_16102,(:),[c],_16116
9. shift([],[=v,+wh,c]) [Aca,likes]
    3,3,(::),[=v,+wh,c],[]
    _16662,_16668,_16674,[=d,=d,v],_16682->_16662,_16668,(:),[=d,v],[(2,3,[-wh])|_16682]
    2,_16102,_16480,[c],_16116->0,_16102,(:),[c],_16116
10. lc1(merge1) [Aca,likes]
    3,_16940,_16946,[v],_16954->3,_16940,(:),[+wh,c],_16954
    _16662,_16668,_16674,[=d,=d,v],_16682->_16662,_16668,(:),[=d,v],[(2,3,[-wh])|_16682]
    2,_16102,_16480,[c],_16116->0,_16102,(:),[c],_16116
11. shift([Aca],[d]) [likes]
    3,4,(::),[d],[]
    3,_16940,_16946,[v],_16954->3,_16940,(:),[+wh,c],_16954
    _16662,_16668,_16674,[=d,=d,v],_16682->_16662,_16668,(:),[=d,v],[(2,3,[-wh])|_16682]
    2,_16102,_16480,[c],_16116->0,_16102,(:),[c],_16116
12. c3(lc2(merge2)) [likes]
    4,_16668,_16674,[=d,=d,v],_16682->3,_16668,(:),[+wh,c],[(2,3,[-wh])|_16682]
    2,_16102,_16480,[c],_16116->0,_16102,(:),[c],_16116
13. c(shift([likes],[=d,=d,v])) []
    3,5,(:),[+wh,c],[(2,3,[-wh])]
    2,_16102,_16480,[c],_16116->0,_16102,(:),[c],_16116
14. c(lc1(move1)) []
    0,5,(:),[c],[]
true .

parse(['Aca',likes,'Bibi',and,'Bibi',likes,'Aca'],[shift([], [=v, c]), lc1(merge1), shift(['Aca'], [d]), c1(lc2(merge2)), shift([likes], [=d, =d, v]), c1(lc1(merge1)), c(shift(['Bibi'], [d])), lc2(merge2), shift([and], [=c, =c, c]), c1(lc1(merge1)), shift([], [=v, c]), c1(lc1(merge1)), shift(['Bibi'], [d]), c1(lc2(merge2)), shift([likes], [=d, =d, v]), c1(lc1(merge1)), c(shift(['Aca'], [d]))] ).

parse(['Bibi',knows,what,'Aca',likes,and,'Aca',likes,'Bibi'],[shift([], [=v, c]), lc1(merge1), shift(['Bibi'], [d]), c1(lc2(merge2)), shift([knows], [=c, =d, v]), c1(lc1(merge1)), shift([what], [d, -wh]), lc2(merge3), shift([], [=v, +wh, c]), lc1(merge1), shift(['Aca'], [d]), c3(lc2(merge2)), c(shift([likes], [=d, =d, v])), c(lc1(move1)), lc2(merge2), shift([and], [=c, =c, c]), c1(lc1(merge1)), shift([], [=v, c]), c1(lc1(merge1)), shift(['Aca'], [d]), c1(lc2(merge2)), shift([likes], [=d, =d, v]), c1(lc1(merge1)), c(shift(['Bibi'], [d]))]).

?- parse(['Bibi',knows,what,'Aca',likes,and,'Aca',knows,what,'Bibi',likes],L).
L = [shift([], [=v, c]), lc1(merge1), shift(['Bibi'], [d]), c1(lc2(merge2)), shift([knows], [=c, =d, v]), c1(lc1(merge1)), shift([what], [d, -wh]), lc2(merge3), shift([], [=v, +wh, c]), lc1(merge1), shift(['Aca'], [d]), c3(lc2(merge2)), c(shift([likes], [=d, =d, v])), c(lc1(move1)), lc2(merge2), shift([and], [=c, =c, c]), c1(lc1(merge1)), shift([], [=v, c]), c1(lc1(merge1)), shift(['Aca'], [d]), c1(lc2(merge2)), shift([knows], [=c, =d, v]), c1(lc1(merge1)), shift([what], [d, -wh]), lc2(merge3), shift([], [=v, +wh, c]), lc1(merge1), shift(['Bibi'], [d]), c3(lc2(merge2)), c(shift([likes], [=d, =d, v])), c(lc1(move1))] .

*/
