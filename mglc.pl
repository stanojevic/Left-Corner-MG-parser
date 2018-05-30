% file: mglc.pl
% origin author : E Stabler and M Stanojevic
% origin date: Mar 2018
% purpose: compute MG left corner parse
%   Tested with SWI-Prolog (threaded, 64 bits, version 7.6.4)

% operator defs - for readability
:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features

:- ['treeDisplay/wish_tree'].  % for displaying tree using tcl/tk
:- ['treeDisplay/latex_tree']. % for writing tree to ltree.tex
:- ['treeDisplay/pptree'].     % for prettyprinting tree to terminal

%%%%%% LEXICON. Uncomment ONE of the following grammars.
%%% TESTS are included in grammar files.

%verbose. % uncomment this line for verbose output. Also see below for "trace"
verbose :- fail.

% uncomment one grammar
:- ['grammars/g1'].    % first simple example with wh-movement
%:- ['grammars/g1dt'].  % same as g1, but with derivation tree arguments
%:- ['grammars/g1xb'].  % same as g1, but with X-bar derived tree arguments
%:- ['grammars/g2'].    % copy language 
%:- ['grammars/g2dt'].  % copy language with derivation tree arguments
%:- ['grammars/g2xb'].  % copy language with X-bar derived tree arguments
%:- ['grammars/g3'].    % example that uses lc1(merge2)
%:- ['grammars/g3dt'].  % same as g3 with derivation tree arguments
%:- ['grammars/g3xb'].  % same as g3 with X-bar derived tree arguments

% NB: Oracle not yet strong enough for backtrack parsing of the empty categories of g2e.
% For now, g2e parses can be specified, as shown in examples in each grammar file.
%:- ['grammars/g2e'].   % copy-like language with empty categories

%%%% ORACLE

merge3prediction(((_L0,_R0,_T0,_Fs0,_M0) -> (_L1,_R1,':',_Fs1,M1))) :-
    nonvar(M1), \+ M1 = [].

% we can look under merge3 predictions for the next predicted category
oracleOK([Focus|Queue],LC) :-
    merge3prediction(Focus), !,
    oracleOK(Queue,LC).

% when queue is empty, implicit prediction is start
oracleOK([],(_L,_R,T,Fs,_M)) :- !,
    startCategory(Start), % this is the initial, implicit prediction
    (Fs=[Start] ; link(T,Fs,[Start])).
oracleOK([],(_ -> (_L,_R,T,Fs,_M))) :- !,
    startCategory(Start), % this is the initial, implicit prediction
    (Fs=[Start] ; link(T,Fs,[Start])).

% predicted mover is antecedent of a (non-merge3) prediction
oracleOK([ ((_L0,_R0,_T0,Fs0,_M0) -> _) |_],(_L1,_R1,T1,Fs1,_M1)) :- !,
    link(T1,Fs1,Fs0).
oracleOK([ ((_L0,_R0,_T0,Fs0,_M0) -> _) |_],(_ -> (_L1,_R1,T1,Fs1,_M1))) :- !,
    link(T1,Fs1,Fs0).

%%%%%% PRECOMPILE VALID FEATURES AND ORACLE FOR LOADED GRAMMAR
% Every valid MG feature sequence is a suffix of the features of some lexical item.  
% Esp. for bigger lexicons, it is important to precompile the valid feature sequences,
%   instead of consulting the lexicon for every check.   

%% newFeat/4 and featClosure/1 for bottom-up memoized calculation of reachable feature sequences
:- dynamic validHeadFeat/2, validMoverFeat/2, link/3.
% newFeat(+OneCorner,-OtherCorner,-Result,-LCRule) 
% NB: Oracle checks head chains. It can be strengthened to check mover chains. (Future work!)
% NB: We should derive newFeat/4 from the definition of the LC rules, lc/3. (Future work!)
% - We collect both head feature sequences and mover feature sequences here,
%   but we keep them separate. (Mover feature sequences always begin with a -F feature)
% - In collecting these Results, we may have to "wait" for the left corner,
%   and in that case, the two corners appear in non-left-corner order,
%   except in the case of merge3, either arg can be left corner of mover Result.
% - In move and merge3 steps, We collect both head chain and mover chain results.
% Because we do not know which corner of merge2, merge3 is needed:
% - When a merge1 or merge2 left-corner is moving, we allow the right-corner to be
%   placed on top of the queue. That is, when we have link(LC,Result)
%   and LC has licensees, we also link(RC,Result) to allow construction
%   of elements in the derived order. We call this the "movingLC" case in comments below
% Because the second arg of merge3 can move:
% - when merge3 produces mover chain [-f,...-g], it is linked to any Fs s.t. [+g|Fs] is valid
newFeat( ([=F|Fs],'::'),        ([F],T),     (Fs,':'), lc1(merge1)         ) :- validHeadFeat([F],T).
newFeat(        ([F],_), ([=F|Fs],'::'),     (Fs,':'), wait(lc1(merge1))   ) :- validHeadFeat([=F|Fs],'::').

newFeat(  ([=F|Fs],':'),        ([F],T),     (Fs,':'), lc1(merge2)         ) :- validHeadFeat([F],T).
newFeat(        ([F],_),  ([=F|Fs],':'),     (Fs,':'), wait(lc1(merge2))   ) :- validHeadFeat([=F|Fs],':').

newFeat(        ([F],_),  ([=F|Fs],':'),     (Fs,':'), lc2(merge2)         ) :- validHeadFeat([=F|Fs],':').
newFeat(  ([=F|Fs],':'),        ([F],T),     (Fs,':'), wait(lc2(merge2))   ) :- validHeadFeat([F],T).

newFeat(     ([=F|_],_),   ([F,G|Gs],T), ([G|Gs],':'), merge3              ) :- validHeadFeat([F,G|Gs],T).
newFeat(    ([=F|Fs],_),   ([F,G|Gs],T),     (Fs,':'), merge3              ) :- validHeadFeat([F,G|Gs],T).
newFeat(   ([F,G|Gs],_),    ([=F|Fs],T), ([G|Gs],':'), wait(merge3)        ) :- validHeadFeat([=F|Fs],T).
newFeat(    ([F,_|_],_),    ([=F|Fs],T),     (Fs,':'), wait(merge3)        ) :- validHeadFeat([=F|Fs],T).

newFeat(    ([+F|Fs],_),         (Gs,T),     (Fs,':'), moved(merge3)       ) :- validHeadFeat(Gs,T), last(Gs,-F).
newFeat(         (Gs,_),    ([+F|Fs],T),     (Fs,':'), wait(moved(merge3)) ) :- last(Gs,-F), validHeadFeat([+F|Fs],T).

newFeat(    ([+F|Fs],_),       ([-F],T),     (Fs,':'), lc1(move1)          ) :- validMoverFeat([-F],T).
newFeat(       ([-F],_),    ([+F|Fs],T),     (Fs,':'), wait(lc1(move1))    ) :- validHeadFeat([+F|Fs],T).

newFeat(    ([+F|Fs],_),  ([-F,G|Gs],T),     (Fs,':'), lc1(move2)          ) :- validMoverFeat([-F,G|Gs],T).
newFeat(   ([-F,_|_],_),    ([+F|Fs],T),     (Fs,':'), wait(lc1(move2))    ) :- validHeadFeat([+F|Fs],T).

% For a standard pseudo-naive computation of closure,
%   we use asserted validHeadfeat,validMoverFeat clauses as the "chart",
%   and pass the "agenda" as an argument. (These are all ground)
%   As the valid feature sequences are found, we generate
%   "links" to elements that can go on top of predictions
% NB: In any feature =F,F,+F,-F, if F is not atomic, we "uninstantiate"
%     its arguments for the oracle. E.g. If those arguments are trees,
%     we don't want the oracle to have all possible instantiated trees!
featClosure([(Fs0,T0)|Agenda0]) :-
    setof((UFs,T),
	  Fs0^T0^Fs1^T1^Fs^T^Rule^UFs0^UFs1^UFs^
	  (
	      newFeat((Fs0,T0),(Fs1,T1),(Fs,T),Rule),
	      uninstantiateFeats(Fs0,UFs0),
	      uninstantiateFeats(Fs1,UFs1),
	      uninstantiateFeats(Fs,UFs),
	      ( Rule=lc1(merge1) -> ensure(link(T0,UFs0,UFs))
	      ; Rule=wait(lc1(merge1)) -> ensure(link(T1,UFs1,UFs))

	      ; Rule=lc1(merge2) -> ensure(link(T0,UFs0,UFs))
	      ; Rule=wait(lc1(merge2)) -> ensure(link(T1,UFs1,UFs))

	      ; Rule=lc2(merge2) -> ensure(link(T0,UFs0,UFs))
	      ; Rule=wait(lc2(merge2)) -> ensure(link(T1,UFs1,UFs))

	      ; Rule=merge3 -> ensure(link(T0,UFs0,UFs)), ensure(link(T1,UFs1,UFs))
	      ; Rule=wait(merge3) -> ensure(link(T0,UFs0,UFs)), ensure(link(T1,UFs1,UFs))
	      ; Rule=moved(merge3) -> ensure(link(T0,UFs0,UFs)), ensure(link(T1,UFs1,UFs))
	      ; Rule=wait(moved(merge3)) -> ensure(link(T0,UFs0,UFs)), ensure(link(T1,UFs1,UFs))

	      ; Rule=lc1(move1) -> ensure(link(T0,UFs0,UFs)), ensure(link(T1,UFs1,UFs))
	      ; Rule=wait(lc1(move1)) -> ensure(link(T0,UFs0,UFs)), ensure(link(T1,UFs1,UFs))

	      ; Rule=lc1(move2) -> ensure(link(T0,UFs0,UFs)), ensure(link(T1,UFs1,UFs))
	      ; Rule=wait(lc1(move2)) -> ensure(link(T0,UFs0,UFs)), ensure(link(T1,UFs1,UFs))
	      ),
	      % now check to see if this result might be something new
	      \+ ( validMoverFeat(UFs,T) ; validHeadFeat(UFs,T) )
	  )
	  ,NewItems), !,
    ensureAllFeats(NewItems),
    append(NewItems,Agenda0,Agenda),
    featClosure(Agenda).
featClosure([_E|Agenda0]) :- featClosure(Agenda0).
featClosure([]).

uninstantiateFeats([],[]).
uninstantiateFeats([=F|Fs],[=U|Us]) :- !,    
    functor(F,Functor,Arity),
    functor(U,Functor,Arity),
    uninstantiateFeats(Fs,Us).
uninstantiateFeats([+F|Fs],[+U|Us]) :- !,
    functor(F,Functor,Arity),
    functor(U,Functor,Arity),
    uninstantiateFeats(Fs,Us).
uninstantiateFeats([-F|Fs],[-U|Us]) :- !,
    functor(F,Functor,Arity),
    functor(U,Functor,Arity),
    uninstantiateFeats(Fs,Us).
uninstantiateFeats([F|Fs],[U|Us]) :-
    functor(F,Functor,Arity),
    functor(U,Functor,Arity),
    uninstantiateFeats(Fs,Us).

ensure(P) :- ( current_predicate(_,P), call(P) -> true
	     ; (verbose, P=link(_,_,_) -> nl,write(P) ; true),
	       assert(P)
	     ).

ensureAllFeats([]).
ensureAllFeats([([-F|Fs],T)|More]) :- !, ensure(validMoverFeat([-F|Fs],T)), ensureAllFeats(More).
ensureAllFeats([(Feats,T)|More]) :- ensure(validHeadFeat(Feats,T)), ensureAllFeats(More).

validFeats :-
    setof((Fs,'::'),W^(W::Fs),LexFeats),
    ensureAllFeats(LexFeats), % the LexFeats clauses act as the "chart"
    featClosure(LexFeats). % and the LexFeats are the initial "agenda"

:- validFeats. % collect these NOW, after loading grammar

ensureAllClosure([]).
ensureAllClosure([(LCT,LCF)-Links|Closure]) :- ensureAllLinks(Links,LCT,LCF), ensureAllClosure(Closure).

ensureAllLinks([],_,_).
ensureAllLinks([(':',AncF)|Links],LCT,LCF) :-  ensure(link(LCT,LCF,AncF)), ensureAllLinks(Links,LCT,LCF).

writeListNL([]).
writeListNL([E|Es]) :- write(E), nl, writeListNL(Es).

% We now transitively close the link relation.
closeLinks :-  setof((LCT,LCFs)-Links,setof((':',Anc),link(LCT,LCFs,Anc),Links),Base),
    (verbose -> nl,write('BASE'), nl, writeListNL(Base) ; true),
    setof((LCT,LCFs,Link),Links^(member((LCT,LCFs)-Links,Base),member(Link,Links)),BaseLinks),
    length(BaseLinks,BaseSize),
    transitive_closure(Base,Closure),
    (verbose -> nl,write('CLOSURE'), nl, writeListNL(Closure) ; true),
    setof((LCT,LCFs,Link),Links^(member((LCT,LCFs)-Links,Closure),member(Link,Links)),ClosureLinks),
    length(ClosureLinks,ClosureSize),
    nl, write('Transitively closing link -- Base size: '), write(BaseSize), write(' Closure size: '), write(ClosureSize), nl,
    ensureAllClosure(Closure).

:- closeLinks. % do this NOW, after loading grammar

removeDuplicates([],[]).
removeDuplicates([H|T0],T) :- member(H,T0), !, removeDuplicates(T0,T).
removeDuplicates([H|T0],[H|T]) :- removeDuplicates(T0,T).

setSMCmax :-
   setof(-U,Ws^Fs^Us^((Ws::Fs), uninstantiateFeats(Fs,Us), member(-U,Us)), Licensees0),
   removeDuplicates(Licensees0,Licensees),
   length(Licensees,N),
   ensure(numberOfLicensees(N)),
   nl, write('By SMC, max number of chains: '), write(N), nl.

:- setSMCmax. % do this NOW, after loading grammar

%%%%%% NOW THE LC STEPS...

% LC input rules: configuration -> configuration,
%   where configuration is (CurrentPosition, RemainingInput, Queue)
%   where queue is a list of terms, the first of which is in "focus",
%   where a term is (Expression) or (Expression1 -> Expression2),
%   where each expression is (LeftPosition,RightPosition,Type,Features,Movers)

step(LCrule,(Position,Input,Queue),(Position,Input,Queue)) :- LCrule==trace, !, trace.

step(LCrule,(Position0,Input0,Queue0),(Position,Input,[Result|Queue])) :-
    splitComposeArgument(LCrule,LCrule0),
    (  [Focus|Queue1]=Queue0,
       lc(LCrule0,Focus,Result0),
       Position=Position0,
       Input=Input0
    ;  shift(Input0,Input,LCrule0,Position0,Position,Result0),
       Queue1=Queue0
    ),

    composeOrNot(LCrule0,Result0,LCrule,Result,Queue1,Queue),

    % check oracle: double neg does check without instantiating
    (\+ \+ oracleOK(Queue,Result)).

% We tentatively assume lexical elements have length 1 or 0 
shift(Input,Input,shift([],Fs),Pos,Pos,(Pos,Pos,'::',Fs,[])) :-
    ([]::Fs).

shift([W|Input],Input,shift([W],Fs),Pos0,Pos,(Pos0,Pos,'::',Fs,[])) :-
    ([W]::Fs),
    Pos is Pos0+1.

%%%%%%%% COMPOSITION RULES...

validTerm((_L,_R,T,Fs,_M)) :- \+ \+ validHeadFeat(Fs,T).

% forward composition
composeOrNot(R,(A -> B),c1(R),(A -> C),Queue0,Queue) :-
    select((B -> C), Queue0, Queue),
    validTerm(B),
    validTerm(C).

% backward composition
composeOrNot(R,(B -> C),c2(R),(A -> C),Queue0,Queue) :-
    select((A -> B), Queue0, Queue),
    validTerm(A),
    validTerm(B).

% forward and backward composition
composeOrNot(R,(B -> C),c3(R),(A -> D),Queue0,Queue) :-
    select((A -> B), Queue0, Queue1),
    select((C -> D), Queue1, Queue),
    validTerm(A),
    validTerm(B),
    validTerm(C),
    validTerm(D).

% compose completed result
composeOrNot(R,A,c(R),B,Queue0,Queue) :-
    select((A -> B), Queue0, Queue),
    validTerm(A),
    validTerm(B).

% no composition
composeOrNot(R,Result,R,Result,Queue,Queue).

% when rules are specified, split off the composition specification
splitComposeArgument(c(R),R).
splitComposeArgument(c1(R),R).
splitComposeArgument(c2(R),R).
splitComposeArgument(c3(R),R).
splitComposeArgument(lc1(R),lc1(R)). % all other cases are the identity
splitComposeArgument(lc2(R),lc2(R)).
splitComposeArgument(shift(A,B),shift(A,B)).

%%%%%%%% UTILITIES FOR THE LC RULES... 
% WE DEFINE A PREDICATE THAT IS ESSENTIALLY SIMILAR TO APPEND,
% BUT WITH DIFFERENT BACKTRACKING BEHAVIOR.
% (THIS KIND OF BEHAVIOR, BEHAVIOR THAT VARIES WITH UNINSTANTIATED VARIABLES,
% IS THE TRICKIEST PART OF PROLOG CODING... TO BE AVOIDED WHENEVER POSSIBLE.
% IT IS GENERALLY ONLY NEEDED FOR CONTROL OF A BACKTRACKING SEARCH.)
% 
% First, given a possibly partially uninstantiated list like
%      L = [A,b,[C|D]|E]
% we define splitExtensibleList so that
%      ?- splitExtensibleList(L,Prefix,Suffix)
% returns something equivalent to the bindings
%      Prefix = [A,b,[C|D]]
%      Suffix = E
% When L = [a|B]
%      ?- splitExtensibleList(L,Prefix,Suffix)
%      Prefix = [a]
%      Suffix = B
% When given a list that is not "extensible", that is, a list which does
% not have a variable tail, we return the empty tail. When L = []
%      ?- splitExtensibleList(L,Prefix,Suffix)
%      Prefix = []
%      Suffix = []
% When L = [a,B]
%      ?- splitExtensibleList(L,Prefix,Suffix)
%      Prefix = [a,B]
%      Suffix = []
% In all cases, splitExtensibleList(L,P,S) implies append(P,S,L).
splitExtensibleList(L,Prefix,Suffix) :- var(L), !, Prefix=[], Suffix=L.
splitExtensibleList([H|T],[H|Prefix],Suffix) :- !, splitExtensibleList(T,Prefix,Suffix).
splitExtensibleList([],[],[]).

% Now given 2 extensible lists
%      L0 = [A,b,[C|D]|E]
%      L1 = [f,G,e(f)|H]
% ?- appendExtensibleLists(L0,L1,L) returns something equivalent to the bindings
%      L = [A,b,[C|D],f,G,e(f)|M]
%      M = E = H
% But if either list is extensible, the result is also extensible. So:
% ?- appendExtensibleLists([],L1,L) returns something equivalent to the bindings
%      L1 = L. 
% In effect, after evaluating appendExtensibleLists(L0,L1,L),
% L0 and L1 share their extensible part, and so, intuitively, 
% no backtracking is needed to extend either L0 or L1.
% Extending L0 is the same as extending L1.
% Since we will only use this when appending Mover lists, we add a check to make
% sure that the instantiated movers do not violate smc ...

smcAppendExtensibleLists(L0,L1,L) :-
    splitExtensibleList(L0,Prefix0,Suffix0),
    splitExtensibleList(L1,Prefix1,Suffix1),
    % the prefixes are NOT extensible, so no backtrack point introduced by:
    append(Prefix0,Prefix1,Prefix),
    smcChains(Prefix),
    % and none of the following should not introduce a backtrack point either:
    ( var(Suffix0), var(Suffix1) -> Suffix0=Suffix1, append(Prefix,Suffix1,L)
    ; var(Suffix0) -> append(Prefix,Suffix1,L2), append(L2,Suffix0,L)
    ; var(Suffix1) -> append(Prefix,Suffix0,L2), append(L2,Suffix1,L)
    ; Suffix0=Suffix1, append(Prefix,Suffix1,L)
    ).

%%%%%% Now the LC rules...
% lc(?Name,+LeftCorner,-Result)

% LC rules: (Name,inputTerm -> outputTerm), where term is expression or (expression -> expression)
%           and expression is (Left,Right,Type,Features,Movers)
lc(lc1(merge1),
  (Left, Mid, '::', [=F|Gamma], []),
%---------------------------------
  ( (Mid, Right, _,  [F], Alphas) -> (Left, Right, ':', Gamma, Alphas) )).

lc(lc1(merge2),
  (Mid, Right, ':', [=F|Gamma], Iotas),
%---------------------------------
  ( (Left, Mid, _,  [F], Alphas) -> (Left, Right, ':', Gamma, Movers) )) :-
    %append(Iotas,Alphas,Movers).
    smcAppendExtensibleLists(Iotas,Alphas,Movers).

lc(lc2(merge2),
  (Left, Mid, _, [F], Iotas),
%---------------------------------
  ( ( Mid, Right, ':',  [=F|Gamma], Alphas) -> (Left, Right, ':', Gamma, Movers) )) :-
    %append(Iotas,Alphas,Movers).
    smcAppendExtensibleLists(Iotas,Alphas,Movers).

lc(lc1(merge3),
  (Left, Right, _, [=F|Gamma], Alphas),
%---------------------------------
  ( (Left0, Right0, T, [F,-G|Fs], Iotas) -> (Left, Right, ':', Gamma, Movers) ) ) :-
    \+ \+ validHeadFeat([F,-G|Fs],T), % ES needed? (does not slow specified search)
    %append([(Left0,Right0,[-G|Fs])|Alphas],Iotas,Movers).
    smcAppendExtensibleLists([(Left0,Right0,[-G|Fs])|Alphas],Iotas,Movers).

lc(lc2(merge3),
  (Left0, Right0, _, [F,-G|Fs], Iotas) ,
%---------------------------------
  ( (Left, Right, T, [=F|Gamma], Alphas) -> (Left, Right, ':', Gamma, Movers) ) ) :-
    \+ \+ validHeadFeat([=F|Gamma],T), % ES needed?
    %append([(Left0,Right0,[-G|Fs])|Alphas],Iotas,Movers).
    smcAppendExtensibleLists([(Left0,Right0,[-G|Fs])|Alphas],Iotas,Movers).

lc(lc1(move1),
  (Mid, Right, ':', [+F|Fs], Movers0),
%---------------------------------
  (Left, Right, ':', Fs, Movers) ) :-
    smcChains(Movers0),
    select((Left,Mid,[-F]), Movers0, Movers).

lc(lc1(move2),
  (Left0, Right0, ':', [+F|Fs], Movers0),
%---------------------------------
  (Left0, Right0, ':', Fs, [(Left1,Right1,[G|Gs])|Movers] ) ) :-
    smcChains(Movers0),
    select((Left1,Right1,[-F,G|Gs]), Movers0, Movers).

%%%%%%%%%%% INCREMENTAL CHECKS OF SMC:

smcChains(Movers) :-
    % first check to make sure that there are not too many mover chains (possibly with var features)
    numberOfLicensees(Max),
    splitExtensibleList(Movers,InstantiatedMovers,_),
    length(InstantiatedMovers,N), N =< Max,
    % then make sure the specified first licensees respect SMC
    \+ smcViolation(InstantiatedMovers).

smcViolation(Movers) :-
    select((_,_,[F0|_]),Movers,Movers1), \+ var(F0),
    select((_,_,[F1|_]),Movers1,_), \+ var(F1),
    F0=F1.

%%%%%%%%%%% TOP LEVEL LOOP

parseSteps(Configuration0,Rules0,Count0) :-
    (  success(Configuration0),
       Rules0=[]
    ;  Rules0=[Rule|Rules],
       step(Rule,Configuration0,Configuration),
       Count1 is Count0+1,
       printConfiguration(Count1,Rule,Configuration),
       %trace,
       parseSteps(Configuration,Rules,Count1)
    ).

success((N,[],[(0,N,_,[C],[])])) :-
    startCategory(C),
    (  atomic(C) -> true   % if there is no tree argument, succeed quietly
    ;  C=..[_Start,Tree],  % if there is a tree argument, latex it and display it 
       numbervars(Tree,0,_),
       wish_tree(Tree),
       latex_tree(Tree),
       nl, pptree(Tree)
    ).

parse(Input,Rules) :- parseSteps((0,Input,[]),Rules,0).

%%%%%%%%%%% PRINT UTILITIES

printFeatures(V) :- var(V), !, write('_Fs').
printFeatures([]).
printFeatures([H|T]) :- write(H), write(' '), printFeatures(T).

printMover((L,R,Fs)) :- 
    printPos(L), write('-'), printPos(R), write(':'), printFeatures(Fs).

printMovers(V) :- var(V), !, write('_M').
printMovers([M]) :- !, printMover(M).
printMovers([M|Movers]) :-
    printMover(M),
    (  var(Movers) -> write('_M')
    ;  (  Movers=[_|_] -> write(','), printMovers(Movers)
       ; true
       )
    ).

printType(X) :- var(X), !, write('.').
printType(X) :- write(X).

printPos(X) :- var(X), !, write('_').
printPos(X) :- write(X).

printExpression(A -> B) :- !,
    write('('), printExpression(A), write(' -> '), printExpression(B), write(')').
   
printExpression((Left,Right,Type,Fs,Movers)) :-
    printPos(Left), write('-'), printPos(Right),
    printType(Type),
    printFeatures(Fs),
    (  var(Movers) -> write('_M')
    ;  (  Movers=[_|_] -> write(','), printMovers(Movers)
       ; true
       )
    ).
   
printConfiguration(Count,Rule,(_Pos,Input,Queue)) :-
    (verbose -> 
	 write(Count), write('. '),
	 write(Rule), write(' '),
	 %write(Pos), write(' '),
	 write(Input), nl,
	 %printExpressions(Queue,4).
	 printLiteralExpressions(Queue,4)
    ; true
    ).

tab(0) :- !.
tab(N) :- write(' '), N1 is N-1, tab(N1).

printExpressions([],_).
printExpressions([E|Es],N) :-
    tab(N),
    printExpression(E), nl,
    printExpressions(Es,N).

printLiteralExpressions([],_).
printLiteralExpressions([E|Es],N) :-
    tab(N),
    write(E), nl,
    printLiteralExpressions(Es,N).
