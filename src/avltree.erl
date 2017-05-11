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
		true -> insert(BT,Elem), setHeight;
		_Else -> BT
	end.

insert({},Elem) -> {Elem,maxHeight({},{})+1,{},{}};
insert({W,H,L,R},Elem) -> 
	if
		Elem > W -> NewR = insert(R,Elem), {W,maxHeight(L, NewR)+1,L,NewR},setHeight;
		Elem < W -> NewL = insert(L,Elem), {W,maxHeight(NewL, R)+1,NewL,R};
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
