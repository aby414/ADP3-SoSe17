%% @author Agin
%% @doc @todo Add description to avltreeTest.


-module(avltreeTest).
-include_lib("eunit/include/eunit.hrl").

initBT_test() -> 
	BT = {},
	
	?assert(avltree:initBT() == BT).

isEmptyBT_test() -> 
	B1 = avltree:initBT(),
	B2 = {10,1,{},{}},
	?assert(avltree:isEmptyBT(B1) == true),
	?assert(avltree:isEmptyBT(B2) == false).

equalBT_test() ->
	B1 = {10,3,{8,2,{4,1,{},{}},{}},{13,1,{},{}}},
	B2 = {10,3,{8,2,{4,1,{},{}},{}},{13,1,{},{}}},
	B3 = {},
	B4 = {10,2,{8,1,{},{}},{}},
	
	?assert(avltree:equalBT(B1, B2) == true),
	?assert(avltree:equalBT(B3,{}) == true),
	?assert(avltree:equalBT(B1, B4) == false).

isBT_test() -> 
	Y = btree:insertBT({}, 10),
	X = btree:insertBT(Y, 8),
	C = btree:insertBT(X, 5),
	V = btree:insertBT(C, 4),
	B = btree:insertBT(V, 9),
	N = btree:insertBT(B, 15),
	M = btree:insertBT(N, 13),
	A = btree:insertBT(M, 16),
	
	S = btree:insertBT(A, 2),
	D = btree:insertBT(S, 1),
	
	B1 = {10,10,{8,3,{5,2,{4,1,{},{}},{}},{9,1,{},{}}},{15,1,{},{}}},
	B2 = {a,4,{8,3,{5,2,{6,1,{},{}},{}},{9,1,{},{}}},{15,1,{},{}}},
	B3 = {10,4,{8,3,{10,2,{4,1,{},{}},{}},{9,1,{},{}}},{15,2,{},{}}},
	B4 = {10,3,{8,2,{},{11,1,{},{}}},{}},
	
	A1= {10,3,{10,2,{},{11,1,{},{}}},{}},

	?assert(avltree:isBT(A) == true),
	?assert(avltree:isBT(B1) == false),
	?assert(avltree:isBT(B2) == false),
	?assert(avltree:isBT(B3) == false),
	?assert(avltree:isBT({}) == true),
	?assert(avltree:isBT(Y) == true),
	?assert(avltree:isBT(B4) == false),
	?assert(avltree:isBT(5) == false),
	?assert(avltree:isBT(S) == false),
	?assert(avltree:isBT(D) == false),
	?assert(avltree:isBT(A1) == false).

insertBT_test() ->
	RandList = util:randomliste(3000),
	Tree = runtime:buildTree(avltree:initBT(),RandList),
	avltree:printBT(Tree, 'tree.dot'),

	?assert(avltree:isBT(Tree) == true).

deleteBT_test() ->
	Y = {4,3,{2,2,{1,1,{},{}},{3,1,{},{}}},{5,1,{},{}}},
	YDel = {4,3,{3,2,{1,1,{},{}},{}},{5,1,{},{}}},
	
	RandList = util:randomliste(1000),
	Tree = runtime:buildTree(avltree:initBT(),RandList),
	%avltree:printBT(Tree, 'tree.dot'),
	DelList = util:randomliste(200),
	DelTree = runtime:deleteElements(Tree, DelList),
	%avltree:printBT(DelTree, 'deltree.dot'),
	
	?assert(avltree:isBT(DelTree) == true),
	?assert(avltree:deleteBT(Y, 2) == YDel).

	

	
	
	




