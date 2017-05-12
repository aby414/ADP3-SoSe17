%% @author aby414
%% @doc @todo Add description to avltree.


-module(avltree).
-export([initBT/0,insertBT/2]).

%% Ein leerer Baum wird erzeugt.
initBT() -> {}.


%% Ueberprueft, ob die Hoehe der Wurzel stimmt.
checkHeight(H,L,R) -> H - maxHeight(L, R) == 1.	


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
