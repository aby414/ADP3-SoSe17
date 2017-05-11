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
		true -> Tree = insert(BT,Elem);
		_Else -> BT
	end.

insert({},Elem) -> {Elem,0,{},{}};
insert({W,H,L,R},Elem) -> 
	if
		Elem > W -> NewR = insert(R,Elem), {W,getHeight(NewR)-getHeight(L),L,NewR},setHeight;
		Elem < W -> NewL = insert(L,Elem), {W,getHeight(NewR)-getHeight(L),NewL,R};
		true -> {W,H,L,R}
	end.

		
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
