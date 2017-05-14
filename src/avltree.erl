%% @author aby414
%% @doc @todo Add description to avltree.


-module(avltree).
-export([initBT/0,insertBT/2,deleteBT/2,isEmptyBT/1,equalBT/2, printBT/2]).

%% Ein leerer Baum wird erzeugt.
initBT() -> {}.

%----------------------------------------------------------------------------------

deleteBT({},_Elem) -> {};
deleteBT({W,_H,{},{}},Elem) when W == Elem -> {};
deleteBT({W,_H,{},R},Elem) when W == Elem -> R;
deleteBT({W,_H,L,{}},Elem) when W == Elem -> L;
deleteBT({W,_H,L,R},Elem) ->
	if
		W == Elem -> Small = findSmallest(R),NewR = deleteBT(R,Small),
					NewTree = {Small,maxHeight(L, NewR)+1,L,NewR},
					 checkTree(NewTree); 
		Elem < W -> NewL = deleteBT(L,Elem), NewTree = {W,maxHeight(NewL,R)+1,NewL,R},
					checkTree(NewTree);
		true -> NewR = deleteBT(R,Elem), NewTree = {W,maxHeight(L, NewR)+1,L,NewR},
				checkTree(NewTree)
	end.
		
%% Findet den kleinsten Knoten im Baum
findSmallest({W,_H,{},{}}) -> W;
findSmallest({_W,_H,L,_R}) -> findSmallest(L).


%----------------------------------------------------------------------------------
insertBT(BT,Elem) ->
	case is_number(Elem) of
		true -> insert(BT,Elem);
		_Else -> BT
	end.

insert({},Elem) -> {Elem,1,{},{}};
insert({W,H,L,R},Elem) -> 
	if
		Elem > W -> NewR = insert(R,Elem), NewTree = {W,maxHeight(L, NewR)+1,L,NewR},
					checkTree(NewTree);
		Elem < W -> NewL = insert(L,Elem), NewTree = {W,maxHeight(NewL, R)+1,NewL,R},
					checkTree(NewTree);
		true -> {W,H,L,R}
	end.

checkTree({W,H,L,R}) -> 
	BoolRight = (getHeight(R)-getHeight(L)) == 2,
	BoolLeft = 	(getHeight(R)-getHeight(L)) == -2,
	BoolProblemLeft = (getHeight(getLeftTree(R))-getHeight(getRightTree(R))) > 0,
	BoolProblemRight = (getHeight(getRightTree(L))- getHeight(getLeftTree(L))) > 0,
	if
		(BoolLeft and BoolProblemRight) -> NewL = leftRotation(L), rightRotation({W,maxHeight(NewL, R)+1,NewL,R});
		(BoolRight and BoolProblemLeft) -> NewR = rightRotation(R), leftRotation({W,maxHeight(L,NewR)+1,L,NewR});
		BoolRight -> leftRotation({W,H,L,R});
		BoolLeft -> rightRotation({W,H,L,R});
		true -> {W,H,L,R}
	end.

%%Einfache Linksrotation
leftRotation({W,_H,L,R}) -> 
	LeftTreeOfRight = getLeftTree(R),
	RightTreeOfRight = getRightTree(R),
	LeftTree= {W,maxHeight(L,LeftTreeOfRight)+1,L,LeftTreeOfRight},
	{getValue(R),maxHeight(LeftTree, RightTreeOfRight)+1,LeftTree,getRightTree(R)}.

%%Einfache RechtsRotation
rightRotation({W,_H,L,R}) -> 
	RightTreeOfLeft = getRightTree(L),
	LeftTreeOfLeft = getLeftTree(L),
	RightTree = {W,maxHeight(RightTreeOfLeft,R)+1,RightTreeOfLeft,R},
	{getValue(L),maxHeight(LeftTreeOfLeft, RightTree)+1,LeftTreeOfLeft,RightTree}.

%%Ermittelt den Wert der Wurzel.
getValue({W,_H,_L,_R}) -> W.
		
%Ermittelt den linken Teilbaum.
getLeftTree({}) -> {};
getLeftTree({_W,_H,L,_R}) -> L.

%%Ermittelt den rechten Teilbaum.
getRightTree({}) -> {};
getRightTree({_W,_H,_L,R}) -> R.

%% Ermittelt die Hoehe eines Baums.
getHeight({}) -> 0;
getHeight({_W,H,_L,_R}) -> H.

%% Ermittelt die groesste Hoehe der Kinderknoten.
maxHeight(L,R) -> Bool1 = getHeight(L) > getHeight(R), 
	if 
		Bool1 == true -> getHeight(L);
		true -> getHeight(R)
	end.

%----------------------------------------------------------------------------------

%% Ueberprueft ob der Baum leer ist.
isEmptyBT({}) -> true;
isEmptyBT(_) -> false.

%----------------------------------------------------------------------------------

%% Testet auf strukturelle Gleichheit.
equalBT(BT1,BT2) -> BT1 == BT2.

%----------------------------------------------------------------------------------

printBT(BT,Filename) -> util:logging(Filename, "digraph avltree{"),
						util:logging(Filename, "\n"),
						printTree(BT,Filename),
						util:logging(Filename, "}").
printTree({W,_H,{},{}},Filename) -> util:logging(Filename,integer_to_list(W)), util:logging(Filename, "\n");
printTree({W,_H,L,{}},Filename) -> 
	HeadL = integer_to_list(getValue(L)),
	util:logging(Filename, integer_to_list(W) ++ " -> " ++ HeadL ++" [label = " ++ integer_to_list(getHeight(L)) ++ "]"),
	util:logging(Filename, "\n"),
	printTree(L, Filename);
printTree({W,_H,{},R},Filename)-> 
	HeadR = integer_to_list(getValue(R)),
	util:logging(Filename, integer_to_list(W) ++ " -> " ++ HeadR ++" [label = " ++ integer_to_list(getHeight(R)) ++ "]"),
	util:logging(Filename, "\n"),
	printTree(R, Filename);
printTree({W,_H,L,R},Filename) -> 
	HeightL = integer_to_list(getHeight(L)),
	HeightR = integer_to_list(getHeight(R)),
	HeadL = integer_to_list(getValue(L)),
	HeadR = integer_to_list(getValue(R)),
	util:logging(Filename, integer_to_list(W) ++ " -> " ++ HeadL ++" [label = " ++ HeightL ++ "]"),
	util:logging(Filename, "\n"),
	printTree(L, Filename),
	util:logging(Filename, integer_to_list(W) ++ " -> " ++ HeadR ++ " [label = " ++ HeightR ++ "]"),
	util:logging(Filename, "\n"),
	printTree(R, Filename).


