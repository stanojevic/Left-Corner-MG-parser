# Left-Corner MG parser

This is the implementation of the Arc-Eager Left-Corner Minimalist Grammar parser from the paper:

      Title   : A sound and complete Left-Corner parsing for Minimalist Grammars
      Authors : Miloš Stanojević and Edward Stabler
      In      : Proceedings of ACL 2018 CogACLL Workshop on
                Cognitive Aspects of Computational Language Learning and Processing

## Usage

The files in this directory are for building LC derivations, using SWI-prolog.

Example grammar G1 from the paper, with simple wh-movement, is in grammars/g1.pl

QUICK-START:
1. Install tcl/tk and swi-prolog
  On Mac, both are available from homebrew:
        brew install tcl-tk swi-prolog
  On linux, both are available in standard distributions.
     In Ubuntu linux, do:
        sudo apt-get tcl-dev tk-dev swi-prolog
  On windows 10, install Windows Subsystem Linux (WSL) and
     then get Ubuntu (free) from the Microsoft store,
     and see above
2. Open mglc.pl in an editor, and uncomment just the lexicon: grammars/g1.pl
3. Copy examples out of grammars/g1.pl to your command prompt, for a session
   that looks like this:

              %%%% BEGIN SESSION
              > cd pl
              > swipl
              Welcome to SWI-Prolog (threaded, 64 bits, version 7.6.4)
              SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
              Please run ?- license. for legal details.
              
              For online help and background, visit http://www.swi-prolog.org
              For built-in help, use ?- help(Topic). or ?- apropos(Word).
              
              1 ?- [mglc].
              
              Transitively closing link -- Base size: 10 Closure size: 10
              By SMC, max number of chains: 1
              true.
              
              2 ?- parse(['Bibi',knows,what,'Aca',likes],L).
              L = [shift([], [=v, c]), lc1(merge1), shift(['Bibi'], [d]), c1(lc2(merge2)), shift([knows], [=c, =d, v]), c1(lc1(merge1)), shift([what], [d|...]), lc2(merge3), shift(..., ...)|...] w
              [write]
              L = [shift([], [=v, c]), lc1(merge1), shift(['Bibi'], [d]), c1(lc2(merge2)), shift([knows], [=c, =d, v]), c1(lc1(merge1)), shift([what], [d, -wh]), lc2(merge3), shift([], [=v, +wh, c]), lc1(merge1), shift(['Aca'], [d]), c3(lc2(merge2)), c(shift([likes], [=d, =d, v])), c(lc1(move1))] .
              
              3 ?- ^D
              
              % halt
              ~/Dropbox/share/tex/cacllp18/pl> 
              %%%% END SESSION

Now uncommenting "verbose" ...

              %%%% BEGIN SESSION
              > cd pl
              > swipl
              
              > swipl
              Welcome to SWI-Prolog (threaded, 64 bits, version 7.6.4)
              SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
              Please run ?- license. for legal details.
              
              For online help and background, visit http://www.swi-prolog.org
              For built-in help, use ?- help(Topic). or ?- apropos(Word).
              
              1 ?- [mglc].
              
              Transitively closing link -- Base size: 10 Closure size: 10
              By SMC, max number of chains: 1
              true.
              
              2 ?- parse(['Bibi',knows,what,'Aca',likes],[
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
              |    |    |    |    |    |    |    |    |    |    |    |    |    |    
              1. shift([],[=v,c]) [Bibi,knows,what,Aca,likes]
                  0,0,(::),[=v,c],[]
              2. lc1(merge1) [Bibi,knows,what,Aca,likes]
                  0,_11292,_11298,[v],_11306->0,_11292,(:),[c],_11306
              3. shift([Bibi],[d]) [knows,what,Aca,likes]
                  0,1,(::),[d],[]
                  0,_11292,_11298,[v],_11306->0,_11292,(:),[c],_11306
              4. c1(lc2(merge2)) [knows,what,Aca,likes]
                  1,_11292,(:),[=d,v],_11306->0,_11292,(:),[c],_11306
              5. shift([knows],[=c,=d,v]) [what,Aca,likes]
                  1,2,(::),[=c,=d,v],[]
                  1,_11292,(:),[=d,v],_11306->0,_11292,(:),[c],_11306
              6. c1(lc1(merge1)) [what,Aca,likes]
                  2,_11292,_11646,[c],_11306->0,_11292,(:),[c],_11306
              7. shift([what],[d,-wh]) [Aca,likes]
                  2,3,(::),[d,-wh],[]
                  2,_11292,_11646,[c],_11306->0,_11292,(:),[c],_11306
              8. lc2(merge3) [Aca,likes]
                  _11816,_11822,_11828,[=d|_11842],_11836->_11816,_11822,(:),_11842,[(2,3,[-wh])|_11836]
                  2,_11292,_11646,[c],_11306->0,_11292,(:),[c],_11306
              9. shift([],[=v,+wh,c]) [Aca,likes]
                  3,3,(::),[=v,+wh,c],[]
                  _11816,_11822,_11828,[=d|_11842],_11836->_11816,_11822,(:),_11842,[(2,3,[-wh])|_11836]
                  2,_11292,_11646,[c],_11306->0,_11292,(:),[c],_11306
              10. lc1(merge1) [Aca,likes]
                  3,_12026,_12032,[v],_12040->3,_12026,(:),[+wh,c],_12040
                  _11816,_11822,_11828,[=d|_11842],_11836->_11816,_11822,(:),_11842,[(2,3,[-wh])|_11836]
                  2,_11292,_11646,[c],_11306->0,_11292,(:),[c],_11306
              11. shift([Aca],[d]) [likes]
                  3,4,(::),[d],[]
                  3,_12026,_12032,[v],_12040->3,_12026,(:),[+wh,c],_12040
                  _11816,_11822,_11828,[=d|_11842],_11836->_11816,_11822,(:),_11842,[(2,3,[-wh])|_11836]
                  2,_11292,_11646,[c],_11306->0,_11292,(:),[c],_11306
              12. c3(lc2(merge2)) [likes]
                  4,_11822,_11828,[=d,=d,v],_11836->3,_11822,(:),[+wh,c],[(2,3,[-wh])|_11836]
                  2,_11292,_11646,[c],_11306->0,_11292,(:),[c],_11306
              13. c(shift([likes],[=d,=d,v])) []
                  3,5,(:),[+wh,c],[(2,3,[-wh])]
                  2,_11292,_11646,[c],_11306->0,_11292,(:),[c],_11306
              14. c(lc1(move1)) []
                  0,5,(:),[c],[]
              true 
              .
              
              3 ?- ^D
              
              % halt
              > 
              %%%% END SESSION

Trees can be built by giving tree arguments to syntactic features in the grammars.
This is illustrated by these variations of g1.pl:
  grammars/g1dt.pl  like g1.pl except a the categories have arguments
                        so that a derivation tree is built as a side
			effect of finding the LC parse
  grammars/g1xb.pl  like g1.pl except a the categories have arguments
                        so that an X-bar derived tree is built as a side
			effect of finding the LC parse
If you use either of these latter grammars, a tree will be displayed,
the same tree will be written in latex format to ltree.tex,
and the same tree will be pretty-printed.

So to get a tree display, edit mglc.pl again to use one of g1dt.pl or g1xb.pl,
and copy examples from the respective file to your command prompt.
