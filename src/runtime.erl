%% @author Agin
%% @doc @todo Add description to runtime.


-module(runtime).
-export([runRandNum/1]).

runRandNum(Number) ->
	RandList = util:randomliste(Number),
	Tree = buildTree(avltree:initBT(),RandList),
	avltree:printBT(Tree, 'avltree.dot').

buildTree(BT,[]) -> BT;
buildTree(BT,[H|T]) -> buildTree(avltree:insertBT(BT, H),T).
	
