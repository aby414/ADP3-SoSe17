%% @author aby414
%% @doc @todo Add description to avltree.


-module(avltree).
-export([initBT/0,insertBT/2,deleteBT/2,isEmptyBT/1,equalBT/2, printBT/2,isBT/1]).

%% Ein leerer Baum wird erzeugt.
initBT() -> btree:initBT().

%----------------------------------------------------------------------------------

isBT({}) -> true; 
isBT({W,H,L,R}) when is_integer(W) and is_integer(H) -> 
	BoolHeight = checkHeight(H,L,R),
	BoolBalance = checkBalance(L,R),
	{_Min,Max} = getMinMax({W,H,L,R}), 

	if 
		 Max == null -> false;
		 BoolBalance == false -> false;
		 BoolHeight == false -> false;
		 true -> true					
	end;
isBT(_) -> false.

getMinMax({W,1,{},{}}) when is_integer(W)->{W,W};
getMinMax({W,H,L,{}}) when is_integer(H) and is_integer(W) -> 
	{Min,Max} = getMinMax(L),
	BoolBalance = checkBalance(L,{}),
	BoolHeight = checkHeight(H, L,{}),
	if 
		BoolHeight == false -> {null,null};
		BoolBalance == false -> {null,null};
		(Max /= null) and (Max < W) -> {Min,W};
		true -> {null,null}
	end;

getMinMax({W,H,{},R}) when is_integer(H)  and is_integer(W) -> 
	{Min,Max} = getMinMax(R),
	BoolBalance = checkBalance({},R),
	BoolHeight = checkHeight(H,{},R),
	if
		BoolHeight == false -> {null,null};
		BoolBalance == false -> {null,null};
		(Min /= null) and (Min > W) -> {W,Max};
		true -> {null,null}
	end;

getMinMax({W,H,L,R})  when is_integer(H) and is_integer(W)-> 
	{LMin,LMax} = getMinMax(L),
	{RMin,RMax} = getMinMax(R),
	BoolBalance = checkBalance(L,R),
	BoolHeight = checkHeight(H, L,R),
	if
		BoolHeight == false -> {null,null};
		BoolBalance == false -> {null,null};
		(LMax /= null) and (RMin /= null) and (LMax < W) and (RMin > W) -> {LMin,RMax};
		true -> {null, null}
	end;

getMinMax(_) -> {null,null}.

%% Ueberprueft, ob die Hoehe der Wurzel stimmt.
checkHeight(H,L,R) -> (H - maxHeight(L, R)) == 1.	

checkBalance(L,R) -> 
	Num = (getHeight(R)-getHeight(L)),
	case Num of
		1 -> true;
		-1 -> true;
		0 -> true;
		_Else -> false
	end.

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
findSmallest({W,_H,{},_R}) -> W;
findSmallest({_W,_H,L,_R}) -> findSmallest(L).


%----------------------------------------------------------------------------------
insertBT(BT,Elem) ->
	if
		is_number(Elem) == true -> insert(BT,Elem);
		true -> BT
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
		(BoolLeft and BoolProblemRight) -> util:counting1(ddleftrotate),NewL = leftRotation(L), rightRotation({W,maxHeight(NewL, R)+1,NewL,R});
		(BoolRight and BoolProblemLeft) -> util:counting1(ddrightrotate),NewR = rightRotation(R), leftRotation({W,maxHeight(L,NewR)+1,L,NewR});
		BoolRight -> leftRotation({W,H,L,R});
		BoolLeft -> rightRotation({W,H,L,R});
		true -> {W,H,L,R}
	end.

%%Einfache Linksrotation
leftRotation({W,_H,L,R}) -> 
	util:counting1(leftrotate),
	LeftOfRight = getLeftTree(R),
	RightOfRight = getRightTree(R),
	LeftTree= {W,maxHeight(L,LeftOfRight)+1,L,LeftOfRight},
	{getValue(R),maxHeight(LeftTree,RightOfRight)+1,LeftTree,RightOfRight}.

%%Einfache RechtsRotation
rightRotation({W,_H,L,R}) -> 
	util:counting1(rightrotate),
	LeftOfLeft = getLeftTree(L),
	RightOfLeft = getRightTree(L),
	RightTree = {W,maxHeight(RightOfLeft, R)+1, RightOfLeft,R},
	{getValue(L),maxHeight(LeftOfLeft, RightTree)+1,LeftOfLeft,RightTree}.

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
maxHeight(L,R) -> Bool1 = (getHeight(L) > getHeight(R)), 
	if 
		Bool1 == true -> getHeight(L);
		true -> getHeight(R)
	end.

%----------------------------------------------------------------------------------

%% Ueberprueft ob der Baum leer ist.
isEmptyBT(BT) -> btree:isEmptyBT(BT).

%----------------------------------------------------------------------------------

%% Testet auf strukturelle Gleichheit.
equalBT(BT1,BT2) -> btree:equalBT(BT1,BT2).

%----------------------------------------------------------------------------------

printBT(BT,Filename) ->
		 util:logging(Filename, "digraph avltree{"),
		util:logging(Filename, "\n"),
		printTree(BT,Filename),
		util:logging(Filename, "}").
		
printTree({W,_H,{},{}},Filename) -> util:logging(Filename,util:to_String(W)), util:logging(Filename, "\n");
printTree({W,_H,L,{}},Filename) -> 
	HeadL = util:to_String(getValue(L)),
	util:logging(Filename, util:to_String(W) ++ " -> " ++ HeadL ++" [label = " ++ util:to_String(getHeight(L)) ++ "]"),
	util:logging(Filename, "\n"),
	printTree(L, Filename);
printTree({W,_H,{},R},Filename)-> 
	HeadR = util:to_String(getValue(R)),
	util:logging(Filename, util:to_String(W) ++ " -> " ++ HeadR ++" [label = " ++ util:to_String(getHeight(R)) ++ "]"),
	util:logging(Filename, "\n"),
	printTree(R, Filename);
printTree({W,_H,L,R},Filename) -> 
	HeightL = util:to_String(getHeight(L)),
	HeightR = util:to_String(getHeight(R)),
	HeadL = util:to_String(getValue(L)),
	HeadR = util:to_String(getValue(R)),
	util:logging(Filename, util:to_String(W) ++ " -> " ++ HeadL ++" [label = " ++ HeightL ++ "]"),
	util:logging(Filename, "\n"),
	printTree(L, Filename),
	util:logging(Filename, util:to_String(W) ++ " -> " ++ HeadR ++ " [label = " ++ HeightR ++ "]"),
	util:logging(Filename, "\n"),
	printTree(R, Filename).
