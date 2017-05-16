%% @author Agin
%% @doc @todo Add description to runtime.


-module(runtime).
-export([runRandNum/4,runWithPrint/2,runWithDelete/4,run/4,buildTree/2,deleteElements/2]).

runRandNum(Start,End,Interval,Filename) ->
	util:logging(Filename,"NumberofNodes  "),
	util:logging(Filename,"Runtime "),
	util:logging(Filename,"\n"),
	runRandNum2(Start, End, Interval, Filename).

runRandNum2(Start,End,Interval,Filename) when Start =<End -> 
	randNum(Start,Filename),
	runRandNum2(Start+Interval, End, Interval,Filename);
runRandNum2(_,_,_,Filename) -> util:logging(Filename,"\n").

randNum(Number,Filename) ->
	RandList = util:sortliste(Number),
	Start1 = erlang:timestamp(),
	buildTree(avltree:initBT(),RandList),
	Time1 = timer:now_diff(erlang:timestamp(), Start1)/1000,
	util:logging(Filename,util:to_String(Number) ++ "    "),
	util:logging(Filename, util:to_String(Time1)),
	util:logging(Filename,"\n").

buildTree(BT,[]) -> BT;
buildTree(BT,[H|T]) -> buildTree(avltree:insertBT(BT, H),T).

deleteElements(BT,[]) -> BT;
deleteElements(BT,[H|T]) -> deleteElements(avltree:deleteBT(BT, H), T).

runWithPrint(Number,Filename) ->
	RandList = util:randomliste(Number),
	Tree = buildTree(avltree:initBT(),RandList),
	avltree:printBT(Tree, Filename).

runWithDelete(Number,DelNum,Filename,Filename2) ->
	RandList = util:randomliste(Number),
	Tree = buildTree(avltree:initBT(),RandList),
	avltree:printBT(Tree, Filename),
	DeleteList = lists:sublist(util:shuffle(RandList),DelNum),
	DeleteTree = deleteNum(Tree,DeleteList),
	avltree:printBT(DeleteTree, Filename2).

deleteNum(BT,[]) -> BT;
deleteNum(BT,[H|T]) -> deleteNum(avltree:deleteBT(BT, H),T).

run(Start,End,Interval,Filename) ->
		util:logging(Filename,"NumberofNodes  "),
	util:logging(Filename,"RuntimeBtree "),
	util:logging(Filename,"RuntimeAVl "),
	util:logging(Filename,"\n"),
	run2(Start, End, Interval, Filename).

run2(Start,End,Interval,Filename)  when Start =<End -> 
	run3(Start,Filename),
	run2(Start+Interval, End, Interval,Filename);
run2(_,_,_,Filename) -> util:logging(Filename,"\n").

run3(Number,Filename) ->
	RandList = util:sortliste(Number),
	
	Start = erlang:timestamp(),
	buildTree2(btree:initBT(),RandList),
	Time = timer:now_diff(erlang:timestamp(), Start)/1000,
	util:logging(Filename,util:to_String(Number) ++ "    "),
	util:logging(Filename, util:to_String(Time)++ "    "),
	
	Start1 = erlang:timestamp(),
	buildTree(avltree:initBT(),RandList),
	Time1 = timer:now_diff(erlang:timestamp(), Start1)/1000,
	util:logging(Filename, util:to_String(Time1)),
	
	util:logging(Filename,"\n").

buildTree2(BT,[]) -> BT;
buildTree2(BT,[H|T]) -> buildTree2(btree:insertBT(BT, H),T).



