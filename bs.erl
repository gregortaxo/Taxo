-module(bs).
-export([rpc/1,start/0,regUser/3,login/3,logout/3,searchTaxo/4,findTaxo/5,addTaxoBm/8,addTaxoTag/5,removeBookmark/4,getUserBookmarks/3,
		addContext/4, getContexts/3, removeContext/4, bmSearch/5, getTagsFromTaxoIndex/1,getIntersection/1,getBookmarkFile/4,addTaxoBmDataChunk/5,
		getBookmarkTree/3,getTable/1, getBookmarkTree/1,getFilePreview/4,getSimilarBms/4,addConcept/6,removeConcept/4]).
-include_lib("stdlib/include/qlc.hrl").

-record(users, {name, pass}).
-record(userswithconceptcounter, {name, pass, conceptcount}).
-record(testtable, {ime, vred}).
-record(usertaxobookmarks, {name,url,taxotags}).
-record(userbookmarkfiles, {name,filename,filedata}).
-record(taxoindex, {taxotag, bookmarks}).
-record(contexttable, {context, taxotags, queries}).
-record(userconcepttable, {id, name, desc, parent}).

%%-----------interface----------------------
start() -> spawn(fun()-> restarter() end),ok.

regUser(Name,Pass,Node) -> 
	rpc:call(nameToAtom(Node),bs,rpc,[{register,toString(Name),toString(Pass)}]).

login(Name,Pass,Node) ->
	rpc:call(nameToAtom(Node),bs,rpc,[{login,toString(Name),toString(Pass)}]).

logout(Name,Pass,Node) ->
	rpc:call(nameToAtom(Node),bs,rpc,[{logout,toString(Name),toString(Pass)}]).

searchTaxo(Name,Pass,Node,Word) ->  
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(Name),toString(Pass),{searchTaxo, Word}}]).

findTaxo(Name,Pass,Node,Word,ContextConcepts) ->  
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(Name),toString(Pass),{findTaxo,toString(Name), Word, ContextConcepts}}]).

addTaxoBm(User,Pass,Node,Name,Url,FileName,FileData,FilePreview) ->  
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{addTaxoBm,toString(User), Name, Url,FileName,FileData,FilePreview}}]).

addTaxoBmDataChunk(User,Pass,Node,Name,FileData) ->  
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{addTaxoBmDataChunk,toString(User), Name,FileData}}]).

removeBookmark(User,Pass,Node,Name) ->  
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{removeBookmark,toString(User), Name}}]).

addTaxoTag(User,Pass,Node,Name,Taxotags) ->  
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{addTaxoTag,toString(User), Name, Taxotags}}]).
	
getUserBookmarks(Name,Pass,Node) ->  
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(Name),toString(Pass),{getUserBookmarks,Name}}]).
	
addContext(User,Pass,Node,Context) ->  
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{addContext,toString(User), Context}}]).
	
getContexts(User,Pass,Node) ->  
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{getContexts,toString(User)}}]).

removeContext(User,Pass,Node,Context) ->  
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{removeContext,toString(User), Context}}]).

bmSearch(User,Pass,Node,Query,Children) -> 
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{bmSearch,toString(User),Query,Children}}]).

getBookmarkFile(User,Pass,Node,Bookmark) -> 
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{getBookmarkFile,toString(User),Bookmark}}]).

getFilePreview(User,Pass,Node,Bookmark) -> 
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{getFilePreview,toString(User),Bookmark}}]).

getBookmarkTree(User,Pass,Node) ->  
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{getBookmarkTree,User}}]).

getSimilarBms(User,Pass,Node,Bookmark) -> 
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{getSimilarBms,toString(User),Bookmark}}]).

addConcept(User,Pass,Node,ConceptName,ConceptDesc,Parent) -> 
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{addConcept,toString(User),ConceptName,ConceptDesc,Parent}}]).

removeConcept(User,Pass,Node,Id) -> 
	rpc:call(nameToAtom(Node),bs,rpc,[{userProc,toString(User),toString(Pass),{removeConcept,toString(User),Id}}]).

%%------------rpc------------------------
rpc(Q) ->
    bms ! {self(), Q},
    receive
	{bms, Reply} ->
	    Reply
	after 15000 ->
		{error,please_try_again}
    end.
	
%%-------------------user process loop-------------
loopProc(ProcessUser) ->
	case get(userConcepts) of
        undefined  -> loadUserConcepts(nameToAtom(toString(ProcessUser)++"concepts")),loopProc(ProcessUser);
        _ -> 
			receive
				{Id, {addTaxoBm, User, Name, Url,FileName,FileData,FilePreview}} ->
					bms ! {forward, Id, addBookmark(User, Name, Url, FileName, FileData,FilePreview)},
					loopProc(ProcessUser);
				{Id, {addTaxoBmDataChunk, User, Name,FileData}} ->
					bms ! {forward, Id, addTaxoBmDataChunk(User, Name, FileData)},
					loopProc(ProcessUser);
				{Id, {addTaxoTag, User, Name, Taxotags}} ->
					bms ! {forward, Id, addTaxoTagS(User, Name, Taxotags)},
					loopProc(ProcessUser);
				{Id, {removeBookmark, User, Name}} ->
					bms ! {forward, Id, removeBookmark(User, Name)},
					loopProc(ProcessUser);
				{Id, {getUserBookmarks,User}} ->
					bms ! {forward, Id, getUserBookmarks(User)},
					loopProc(ProcessUser);
				{Id, {searchTaxo, Word}} ->
					bms ! {forward, Id, getConceptWithChildrenAndParents(Word)},
					loopProc(ProcessUser);
				{Id, {findTaxo, User, Word, ContextConcepts}} ->
					bms ! {forward, Id, conceptSearchAndSortForUser(User, Word, ContextConcepts)},
					loopProc(ProcessUser);
				{Id, {addContext, User, Context}} ->
					bms ! {forward, Id, addContext(User, Context)},
					loopProc(ProcessUser);
				{Id, {getContexts, User}} ->
					bms ! {forward, Id, getContexts(User)},
					loopProc(ProcessUser);
				{Id, {removeContext, User, Context}} ->
					bms ! {forward, Id, removeContext(User, Context)},
					loopProc(ProcessUser);
				{Id, {bmSearch,User,Words,Children}} ->
					bms ! {forward, Id, bookmarkSearch(User,Words,Children)},
					loopProc(ProcessUser);
				{Id, {getBookmarkFile,User,Bookmark}} ->
					bms ! {forward, Id, getBookmarkFile(User,Bookmark)},
					loopProc(ProcessUser);
				{Id, {getFilePreview,User,Bookmark}} ->
					bms ! {forward, Id, getFilePreview(User,Bookmark)},
					loopProc(ProcessUser);
				{Id, {getBookmarkTree,User}} ->
					bms ! {forward, Id, getBookmarkTree(User)},
					loopProc(ProcessUser);
				{Id, {getSimilarBms,User,Bookmark}} ->
					bms ! {forward, Id, getSimilarBms(User,Bookmark)},
					loopProc(ProcessUser);
				{Id, {addConcept,User,ConceptName,ConceptDesc,Parent}} ->
					bms ! {forward, Id, addConcept(User,ConceptName,ConceptDesc,Parent)},
					loopProc(ProcessUser);
				{Id, {removeConcept,User,TaxoId}} ->
					bms ! {forward, Id, removeConceptWithCheck(User,TaxoId)},
					loopProc(ProcessUser);
				{end_proc} ->
					exit(normal);
				M -> 
					io:format("MessageloopProc=~w~n",[M]),
					loopProc(ProcessUser)
			end
    end.
    
%%-----------------Taxo func--------------------------------------
addBookmark(User,Name,Url,FileName,FileData,FilePreview) ->
	User2 = nameToAtom(toString(User)),
	User3 = nameToAtom(toString(User)++"files"),
	case get_bookmarkDB(User2, Name) of
		[] -> 
			add_TaxoBookmarkDB(User2, Name, Url, []), saveFileToDisk(toString(User),Name,FileData), saveFilePreviewToDisk(toString(User),Name,FilePreview),add_TaxoBookmarkDB(User3,Name,FileName,""), bookmark_added;
		_ ->
			bookmark_already_exists
	end.

saveFileToDisk(User,Name,FileData) ->
	directoryCheck(User),
	file:write_file("userFiles/" ++ User ++"/" ++ Name, [FileData], [append]).

saveFilePreviewToDisk(User,Name,FileData) ->
	directoryCheckPreviews(User),
	file:write_file("userImagePreviews/" ++ User ++"/" ++ Name, [FileData], [append]).

getBookmarkFile(User,Bookmark) ->
	directoryCheck(toString(User)),
	User2 = nameToAtom(toString(User)++"files"),
	[{_,_,FileName,_}] = get_bookmarkFilesDB(User2,Bookmark),
	{ok,FileText} = file:read_file("userFiles/" ++ toString(User) ++ "/" ++ Bookmark),
	[[toBinaryString(FileName), FileText]].

deleteFileFromDisk(User,Bookmark) -> 
	file:delete("userFiles/" ++ toString(User) ++ "/" ++ Bookmark).

deleteFilePreviewFromDisk(User,Bookmark) -> 
	file:delete("userImagePreviews/" ++ toString(User) ++ "/" ++ Bookmark).

directoryCheck(User) ->
	case file:make_dir("userFiles") of
		ok ->
			file:make_dir("userFiles/" ++ User), ok;
		{error,eexist} ->
			file:make_dir("userFiles/" ++ User), ok;
		_ ->
			ok
	end.

addTaxoBmDataChunk(User,Name,FileData) ->
	User2 = nameToAtom(toString(User)++"files"),
	File = get_bookmarkFilesDB(User2,Name),
	case File of
		[] -> 
			ok;
		_ ->
			saveFileToDisk(User, Name,FileData)
	end.

getFilePreview(User,Bookmark) ->
	directoryCheckPreviews(toString(User)),
	{ok,FileText} = file:read_file("userImagePreviews/" ++ toString(User) ++ "/" ++ Bookmark),
	FileText.

directoryCheckPreviews(User) ->
	case file:make_dir("userImagePreviews") of
		ok ->
			file:make_dir("userImagePreviews/" ++ User), ok;
		{error,eexist} ->
			file:make_dir("userImagePreviews/" ++ User), ok;
		_ ->
			ok
	end. 

removeBookmark(User, Name) -> 
	User2 = nameToAtom(toString(User)),
	User3 = nameToAtom(toString(User)++"files"),
	Bm = get_bookmarkDB(User2, Name),
	case Bm of
		[] -> 
			bookmark_removed;
		_ ->
			[H|_] = Bm,
			removeBmFromTaxoIndex(User2,H),
			remove_recordDB(User2, Name),
			remove_recordDB(User3, Name),
			deleteFileFromDisk(User,Name),
			deleteFilePreviewFromDisk(User,Name),	
			bookmark_removed
	end.

removeBmFromTaxoIndex(User,{_,BmName,_,[{Id,_}|T]}) -> removeBmFromTaxoIndexHelper(User,Id,BmName), removeBmFromTaxoIndex(User, {"",BmName,"",T});
removeBmFromTaxoIndex(_, _) -> ok.

removeBmFromTaxoIndexHelper(User, Id, Name) -> 
	User2 = nameToAtom(toString(User)++"index"),
	List = get_TaxoIndexDB(User2, Id),
	case checkIfTaxoIndexContains(List) of
		false ->
			ok;
		true ->
			[{_,_,Bms}] = List,
			NewBmList = lists:delete(Name,Bms),
			case NewBmList of
					[] ->
						remove_recordDB(User2,Id), ok;
					_ ->
						add_TaxoBookmarkDB2(User2, Id, NewBmList),ok
			end
	end.

addTaxoTagS(User, Name, Taxotags) -> 
	User2 = nameToAtom(toString(User)),
	[{_,_,U,T}] = get_bookmarkDB(User2, Name),
	case addTaxoTagS2(Taxotags,T) of
		false ->
			ok;
		true ->
			add_TaxoBookmarkDB(User2, Name, U, Taxotags++T),
			addBookmarkToTaxoIndex(User, Taxotags,{Name,U}),					
			ok
	end.

addTaxoTagS2([{Id,_}], [{Id,_}|_]) -> false;
addTaxoTagS2([{Id,Name}], [{_,_}|T]) -> addTaxoTagS2([{Id,Name}], T);
addTaxoTagS2([{_,_}], []) -> true.

getUserBookmarks(User) ->
	User2 = nameToAtom(toString(User)),
	Tmp = get_user_bookmarksDB3(User2),
	getUserBookmarksHelper(Tmp).
	
getUserBookmarksHelper([{Name,Url,TaxoTags}|T]) -> [[toBinaryString(Name),toBinaryString(Url),getUserBookmarksHelper2(TaxoTags)]] ++ getUserBookmarksHelper(T);
getUserBookmarksHelper([])  -> [].

getUserBookmarksHelper2([{Id,Name}|T]) -> [[toBinaryString(Name), toBinaryString(Id)]] ++ getUserBookmarksHelper2(T);
getUserBookmarksHelper2([])  -> [].
	
toBinaryString(String) ->
	list_to_binary(String).

addBookmarkToTaxoIndex(User, Taxotag, Bookmark) -> 
	if
		Taxotag == [] ->
			ok;
		true ->
			User2 = nameToAtom(toString(User)++"index"),
			Id = getIdFromTaxoTag(Taxotag),
			List = get_TaxoIndexDB(User2, Id),
			{BookmarkName,_} = Bookmark,
			case checkIfTaxoIndexContains(List) of
				false ->
					add_TaxoBookmarkDB2(User2, Id, [BookmarkName]),ok;
				true ->
					CurrentBookmarks = getBookmarksFromTaxoTag(List),
					case checkIfTaxoTagContainsBookmark(List, Bookmark) of
							false ->
								ok;
							true ->
								add_TaxoBookmarkDB2(User2, Id, CurrentBookmarks++[BookmarkName]),ok
					end
			end
	end.

checkIfTaxoIndexContains([]) -> false;
checkIfTaxoIndexContains(_) -> true.

checkIfTaxoTagContainsBookmark([{_,_,[Name|_]}],{Name,_}) -> false;
checkIfTaxoTagContainsBookmark([{U,Ta,[_|T]}],{Name,Url}) -> checkIfTaxoTagContainsBookmark([{U,Ta,T}],{Name,Url});
checkIfTaxoTagContainsBookmark([{_,_,[]}],{_,_}) -> true.

getIdFromTaxoTag([{Id,_}]) -> Id.

getBookmarksFromTaxoTag([{_,_,List}]) -> List;
getBookmarksFromTaxoTag([]) -> [].

jsonConvert([Bm|T]) -> [[toBinaryString(Bm)]] ++ jsonConvert(T);
jsonConvert([]) -> [].

searchTaxoBmsWithoutChildren(_,[]) -> [];
searchTaxoBmsWithoutChildren(User,Word) ->
	Concepts = findConceptsForString(lists:nth(1,Word)),
	Ids = getIdsFromMsg(Concepts),
	removeDuplicates(getResultsFromTaxoIndexHelper(nameToAtom(toString(User)++"index"),Ids)).

searchTaxoBmsWithChildren(_,[]) -> [];
searchTaxoBmsWithChildren(User,Word) ->
	Concepts = findConceptsForString(lists:nth(1,Word)),
	Ids = getIdsFromMsg(Concepts) ++ getOtroke2(Concepts),
	removeDuplicates(getResultsFromTaxoIndexHelper(nameToAtom(toString(User)++"index"),Ids)).

findConceptsForString(String) ->
	case ets:lookup(searchindex, string:to_lower(String)) of
        [{_,List}] -> getConceptsByIds(List) ++ findUserConceptsForString(string:to_lower(String));
		_ -> findUserConceptsForString(string:to_lower(String))
   	end.

getTaxoBmsWithoutChildrenForTaxoId(_,[]) -> [];
getTaxoBmsWithoutChildrenForTaxoId(User,Id) ->
	Ids = [Id],
	removeDuplicates(getResultsFromTaxoIndexHelper(nameToAtom(toString(User)++"index"),Ids)).

getTaxoBmsWithChildrenForTaxoId(_,[]) -> [];
getTaxoBmsWithChildrenForTaxoId(User,Id) ->
	Ids = [Id] ++ getOtroke2([[Id]]),
	removeDuplicates(getResultsFromTaxoIndexHelper(nameToAtom(toString(User)++"index"),Ids)).


getOtroke2([]) -> [];
getOtroke2([[Id|_]|T]) ->  getChildrenFromIndex(Id) ++ getOtroke2For(getChildrenFromIndex(Id)) ++ getOtroke2(T).

getOtroke2For([Taxo|Tail]) -> getChildrenFromIndex(Taxo) ++ getOtroke2For(getChildrenFromIndex(Taxo)) ++ getOtroke2For(Tail);
getOtroke2For([]) -> [].

getChildrenFromIndex(Id)-> 
	case ets:lookup(childrenindex, Id) of
        [{_,Res}] -> Res ++ getUserConceptsByParent(Id);
		_ -> getUserConceptsByParent(Id)
   	end.

getIdsFromMsg([[]|T2]) -> getIdsFromMsg(T2);
getIdsFromMsg([[Id|_]|T2]) -> [Id] ++ getIdsFromMsg(T2);
getIdsFromMsg([]) -> [].

getResultsFromTaxoIndex(User, [Id|T]) ->  getBookmarksFromTaxoTag(get_TaxoIndexDB(User, Id)) ++ getResultsFromTaxoIndex(User, T);
getResultsFromTaxoIndex(_, []) -> [].

getResultsFromTaxoIndexHelper(_, []) -> [];
getResultsFromTaxoIndexHelper(User, ListOfIds) ->  getResultsFromTaxoIndex(User, getIntersection([ListOfIds] ++ [getTagsFromTaxoIndex(User)])).

getIntersection([]) -> [];
getIntersection([GrOne|[]]) -> GrOne;
getIntersection([GrOne|Rest]) -> shortestListFirst([GrOne|Rest],[]).

shortestListFirst([H|T],[]) -> shortestListFirst(T,[H]);
shortestListFirst([H|T],[H2|T2]) -> 
	if
        length(H) < length(H2) ->
			shortestListFirst(T,[H] ++ [H2|T2]);
        true ->
            shortestListFirst(T,[H2|T2] ++ [H])
    end;
shortestListFirst([],[H|T]) -> search_taxoBmsSHelper3b(H,T).

search_taxoBmsSHelper3b(_,[]) -> [];
search_taxoBmsSHelper3b([Bm|T1], Rest) ->  search_taxoBmsSHelper3c(Bm,Rest) ++ search_taxoBmsSHelper3b(T1, Rest);
search_taxoBmsSHelper3b([],_) -> [].

search_taxoBmsSHelper3c(Bm,Rest) ->
	case search_taxoBmsSHelper3d(Bm,Rest) of
				false ->
					[];
				true ->
					[Bm]
	end.

search_taxoBmsSHelper3d(Bm,[H|T]) -> 
	case lists:member(Bm,H) of
		false ->
			false;
		true ->
			search_taxoBmsSHelper3d(Bm,T)
	end;
search_taxoBmsSHelper3d(_,[]) -> true.

getSimilarBms(User,Bookmark) ->
	User2 = nameToAtom(toString(User)),
	UserIndex = nameToAtom(toString(User)++"index"),
	[{_,_,_,T}] = get_bookmarkDB(User2, Bookmark),
	Res = formatSimBms(similarBmsForConcepts(getSimilarBmsForConcepts(getTaxoIds(T),UserIndex,3), [])),
	jsonConvert(removeThisBm(Res,Bookmark)).

formatSimBms([{_,X}|T]) -> [X] ++ formatSimBms(T);
formatSimBms([]) -> [].

similarBmsForConcepts([H|T],Res) ->  similarBmsForConcepts(T,similarBmsForConceptsHelp(H,Res));
similarBmsForConcepts([],Res) -> sortSimBmRes(Res,[]).

sortSimBmRes([H|T],Res) -> sortSimBmRes(T,simBmsAddToRes(H,Res));
sortSimBmRes([],Res) -> Res.

similarBmsForConceptsHelp([H|T],Res) -> 
	case memberSim2(H,Res) of
		false ->
			similarBmsForConceptsHelp(T,simBmsAddToRes(H,Res));
		true ->
			similarBmsForConceptsHelp(T,similarBmsForConceptsHelp2(H,Res))
	end;
similarBmsForConceptsHelp([],Res) -> Res.

memberSim2({_,X}, [{_,X}|_]) -> true;
memberSim2(X, [_|T]) -> memberSim2(X,T);
memberSim2(_,[]) -> false.

similarBmsForConceptsHelp2({X,Bm},[{N,Bm}|T]) ->[{X + N,Bm}] ++ T;
similarBmsForConceptsHelp2(F,[H|T]) -> [H] ++ similarBmsForConceptsHelp2(F,T).

getSimilarBmsForConcepts([Taxo|T],UserIndex,Limit) -> [getSimilarBmsForConcept(UserIndex,Taxo,Limit,[])] ++ getSimilarBmsForConcepts(T,UserIndex,Limit);
getSimilarBmsForConcepts([],_,_) -> [].

getSimilarBmsForConcepts2(_,_,Res,0) -> Res;
getSimilarBmsForConcepts2([Taxo|T],UserIndex,Res,Limit) -> getSimilarBmsForConcepts2(T,UserIndex,getSimilarBmsForConcept(UserIndex,Taxo,Limit,Res),Limit);
getSimilarBmsForConcepts2([],_,Res,_) -> Res.

getSimilarBmsForConcept(Index,TaxoId,Limit,ToReturn) -> 
	List = get_TaxoIndexDB(Index, TaxoId),
	Res = addToListIfNotAlready(getBookmarksFromTaxoTag(List),ToReturn),
	{_,{starsi,Starsi},{otroki,Otroki}} = getConcept(TaxoId),
	getSimilarBmsForConcepts2(getIdsFromMsg(Starsi ++ Otroki),Index,simBmsAddToResHelper(Res,ToReturn,Limit),Limit - 1).

addToListIfNotAlready([H|T], Res) ->
	case memberSim(H,Res) of
		false ->
			[H] ++ addToListIfNotAlready(T,Res);
		true ->
			addToListIfNotAlready(T,Res)
	end;
addToListIfNotAlready([],_) -> [].

memberSim(X, [{_,X}|_]) -> true;
memberSim(X, [_|T]) -> memberSim(X,T);
memberSim(_,[]) -> false.

simBmsAddToResHelper([H|T],ToReturn,Limit) -> simBmsAddToResHelper(T,simBmsAddToRes({Limit,H},ToReturn),Limit);
simBmsAddToResHelper([],ToReturn,_) -> ToReturn.

simBmsAddToRes({X,BmToAdd},[{N,BmInList}|T]) -> 
	if 
		X < N ->
			[{N,BmInList}] ++ simBmsAddToRes({X,BmToAdd},T);
		true -> 
			[{X,BmToAdd}] ++ [{N,BmInList}] ++ T
	end;
simBmsAddToRes({X,BmToAdd},[]) -> [{X,BmToAdd}].

removeThisBm([Bm|T],Bm) -> T;
removeThisBm([OtherBm|T],Bm) -> [OtherBm] ++ removeThisBm(T,Bm);
removeThisBm([],_) -> [].

getTaxoIds([{Taxo,_}|T]) -> [Taxo] ++ getTaxoIds(T);
getTaxoIds([]) -> [].
	
getChildren([Id,_,_,_]) -> getConceptsByIds(getUserConceptsByParent(Id));
getChildren([Id,_,_,_,Otroci]) -> getConceptsByIdsString(Otroci) ++ getConceptsByIds(getUserConceptsByParent(Id)).

getConceptsByIdsString(Ids) ->
	List = (string:tokens(Ids, ",")),
	getConceptsByIds(List).
	
getConceptsByIds([X|Y]) -> [getConceptById(X)] ++ getConceptsByIds(Y);
getConceptsByIds([]) -> [].

getConceptById(String) when String =/= "null" -> 
	case ets:lookup(dataindex, String) of
        [{_,Concept}] -> Concept;
		_ -> getUserConceptById(String)
   	end;
getConceptById(_) -> [].

getConcept(Id) ->
	case ets:lookup(dataindex, Id) of
        [{_,Concept}] -> 
			Children = getChildren(Concept),
			Parents = getConceptsByIdsString(lists:nth(3,Concept)),
			{Concept,{starsi,Parents},{otroki,Children}};
		_ -> getUserConcepts(Id)
   	end.

getConceptJsonFormated(String) ->
	case ets:lookup(dataindex, String) of
        [{_,Concept}] -> 
			Children = getChildren(Concept),
			Parents = getConceptsByIdsString(lists:nth(3,Concept)),
			[[<<"word">>,formatForJson(Concept)],[<<"parents">>,formatForJsonFor(Parents)],[<<"children">>,formatForJsonFor(Children)]];
		_ -> getUserConceptJson(String)
   	end.

getConceptWithChildrenAndParents(String) ->
	case ets:lookup(dataindex, String) of
        [{_,Concept}] -> 
			Children = getChildren(Concept),
			Parents = getConceptsByIdsString(lists:nth(3,Concept)),
			[[[<<"word">>,formatForJson(Concept)],[<<"parents">>,formatForJsonFor(Parents)],[<<"children">>,formatForJsonFor(Children)]]]
			++ getConceptJsonFormatedFor(Parents) ++ getConceptJsonFormatedFor(Children);
		_ -> getUserConceptJson2(String)
   	end.

getConceptJsonFormatedFor([[]|T]) -> getConceptJsonFormatedFor(T);
getConceptJsonFormatedFor([H|T]) -> [getConceptJsonFormated(lists:nth(1,H))] ++ getConceptJsonFormatedFor(T);
getConceptJsonFormatedFor([]) -> [].

%$--------------add concept-----------------------------------------
addConcept(User,ConceptName,ConceptDesc,Parent) ->
	User2 = nameToAtom(toString(User)++"concepts"),
	{UserNameDb,PassDb,Count} = lists:nth(1, getUserDB(nameToAtom(User))),
	writeDB({users,UserNameDb,PassDb,Count+1}),
	Id = "userconcept" ++ integer_to_list(Count),
	writeDB({User2,Id,ConceptName,ConceptDesc,Parent}),
	loadUserConcepts(User2), ok.

removeConceptWithCheck(User,Id) ->
	case canDeleteConcept(User,[Id]) of
		false ->
			things_connected_to_this_concept;
		true ->
			removeConcept(User,Id)
	end.

canDeleteConcept(User,[H|T]) ->
	TaxoIndexRes = get_TaxoIndexDB(nameToAtom(toString(User)++"index"), H),
	if
		TaxoIndexRes == [] ->
			case dict:find(H,get(userConceptChildren)) of
				error  -> canDeleteConcept(User,T);
				{ok,Concepts} -> 
					case canDeleteConcept(User,Concepts) of
						false -> false;
						_ -> canDeleteConcept(User,T)
					end
			end;
		true ->
			false
	end;
canDeleteConcept(_,[]) -> true.

removeConcept(User,Id) ->
	case dict:find(Id,get(userConcepts)) of
        error  -> 
			concept_cannot_be_deleted;
        {ok,_} -> 
			User2 = nameToAtom(toString(User)++"concepts"),
			case dict:find(Id,get(userConceptChildren)) of
				error  -> ok;
				{ok,Concepts} -> removeConceptFor(User,Concepts)
			end,
			remove_recordDB(User2, Id), 
			loadUserConcepts(User2),
			concept_removed
   	end.

removeConceptFor(User,[H|T]) -> removeConcept(User,H), removeConceptFor(User,T);
removeConceptFor(_,[]) -> ok.

getUserConceptJson(String) ->
	case dict:find(String,get(userConcepts)) of
        error  -> [];
        {ok,Concept} -> 
			Children = getChildren(Concept),
			Parents = getConceptsByIdsString(lists:nth(3,Concept)),
			[[<<"word">>,formatForJson(Concept)],[<<"parents">>,formatForJsonFor(Parents)],[<<"children">>,formatForJsonFor(Children)]]
   	end.

getUserConceptJson2(String) ->
	case dict:find(String,get(userConcepts)) of
        error  -> [];
        {ok,Concept} -> 
			Children = getChildren(Concept),
			Parents = getConceptsByIdsString(lists:nth(3,Concept)),
			[[[<<"word">>,formatForJson(Concept)],[<<"parents">>,formatForJsonFor(Parents)],[<<"children">>,formatForJsonFor(Children)]]]
   	end.

getUserConcepts(String) ->
	case dict:find(String,get(userConcepts)) of
        error  -> [];
        {ok,Concept} -> 
			Children = getChildren(Concept),
			Parents = getConceptsByIdsString(lists:nth(3,Concept)),
			{Concept,{starsi,Parents},{otroki,Children}}
   	end.

getUserConceptById(String) ->
	case dict:find(String,get(userConcepts)) of
        error  -> [];
        {ok,Concept} -> Concept
   	end.

getUserConceptsByParent(String) ->
	case dict:find(String,get(userConceptChildren)) of
        error  -> [];
        {ok,Concepts} -> Concepts
   	end.

findUserConceptsForString(String) ->
	case dict:find(String,get(userSearchIndex)) of
		error  -> [];
		{ok,Concepts} ->  getConceptsByIds(Concepts)
   	end.
	
loadUserConcepts(User) ->
	UserConcepts = getTable(User),
	Dict = dict:from_list(userConceptsFormatForDict(UserConcepts)),
	put(userConcepts, Dict),
	ParentsDict = dict:from_list(userConceptParentsFormatForDict(UserConcepts,[])),
	put(userConceptChildren, ParentsDict),
	SearchDict = dict:from_list(userConceptSearchIndexFormatForDict(UserConcepts,[])),
	put(userSearchIndex, SearchDict).

userConceptsFormatForDict([{_,Id,Name,Desc,Parent}|T]) -> [{Id,[Id,Name,Parent,Desc]}] ++ userConceptsFormatForDict(T);
userConceptsFormatForDict([]) -> [].

userConceptParentsFormatForDict([{_,Id,_,_,Parent}|T],Res) -> userConceptParentsFormatForDict(T,addUserConceptParentToRes(Id,Parent,Res));
userConceptParentsFormatForDict([],Res) -> Res.

addUserConceptParentToRes(Id,Parent,[{Parent,List}|T]) -> [{Parent,List ++ [Id]}|T];
addUserConceptParentToRes(Id,Parent,[{ParentOther,List}|T]) ->  [{ParentOther,List}] ++ addUserConceptParentToRes(Id,Parent,T);
addUserConceptParentToRes(Id,Parent,[]) -> [{Parent,[Id]}].

userConceptSearchIndexFormatForDict([{_,Id,Name,_,_}|T],Res) -> userConceptSearchIndexFormatForDict(T,addUserConceptParentToRes(Id,string:to_lower(Name),Res));
userConceptSearchIndexFormatForDict([],Res) -> Res.

%%---------------tree-------------------------------------
getBookmarkTree(User) ->
	User2 = nameToAtom(toString(User)++"index"),
	TaxoIndex = getTable(User2),
	Parents = getParentsFor(TaxoIndex),
	CommonParents = getCommonParents(Parents),
	TrimmedTree = getTrimmedTree(Parents, CommonParents),
	TrimmedList = convertForJson(getTrimmedList(TrimmedTree), User2),
	TrimmedList.

convertForJson([H|T], User) ->  [getConceptData(H,User)] ++ convertForJsonHelper(T,User);
convertForJson([],_) ->  [].

convertForJsonHelper([H|T],User) -> [convertForJson(H,User)] ++ convertForJsonHelper(T,User);
convertForJsonHelper([],_) -> [].

getConceptData(Id,User) ->
	Concept = getConceptJsonFormated(Id),
	Taxo = lists:nth(2,lists:nth(1, Concept)),
	TaxoIndexRes = get_TaxoIndexDB(User, Id),
	if
		TaxoIndexRes == [] ->
			[lists:nth(1, Taxo), lists:nth(2, Taxo),[]];
		true ->
			{_,_,Res} = lists:nth(1,TaxoIndexRes),
			[lists:nth(1, Taxo), lists:nth(2, Taxo), formatForJson(Res)]
	end.


getTrimmedList(TrimmedTree) -> getTrimmedList(TrimmedTree, []).

getTrimmedList([H|T],Res) -> getTrimmedList(T, addToTrimmedList(reverseList(H), 1,Res));
getTrimmedList([], Res) -> Res.

addToTrimmedList(Entries, N ,Res) ->
	if
		N > length(Entries) ->
			Res;
		N == 1 ->
			addToTrimmedList(Entries, N+1, insertToTrimmedListWithoutParent(lists:nth(N, Entries), Res));
		true ->
			addToTrimmedList(Entries, N+1, insertToTrimmedListWithParent(lists:nth(N, Entries),lists:nth(N-1, Entries), Res))
	end.

insertToTrimmedListWithoutParent(ToAdd, []) -> [ToAdd];
insertToTrimmedListWithoutParent(ToAdd, [ToAdd|T]) -> [ToAdd|T];
insertToTrimmedListWithoutParent(_,_) -> [].

insertToTrimmedListWithParent(ToAdd, Parent, [Parent|T]) -> [Parent|insertToTrimmedListWithParentHelper(ToAdd, T)];
insertToTrimmedListWithParent(ToAdd, Parent, [H|T]) -> [H] ++ insertToTrimmedListWithParentFor(ToAdd, Parent, T);
insertToTrimmedListWithParent(_, _, _) -> [].

insertToTrimmedListWithParentFor(ToAdd, Parent, [H|T]) -> [insertToTrimmedListWithParent(ToAdd, Parent, H)] ++ insertToTrimmedListWithParentFor(ToAdd, Parent, T);
insertToTrimmedListWithParentFor(_, _, []) -> [].

insertToTrimmedListWithParentHelper(ToAdd, [[ToAdd|T]|T2]) -> [[ToAdd|T]|T2];
insertToTrimmedListWithParentHelper(ToAdd, [[H|T]|T2]) -> [[H|T]] ++ insertToTrimmedListWithParentHelper(ToAdd, T2);
insertToTrimmedListWithParentHelper(ToAdd, []) -> [[ToAdd]].

reverseList([H|T]) -> reverseList(T) ++ [H];
reverseList([]) -> [].

getTrimmedTree(Parents, CommonParents) -> trimParents(Parents, getParentCounts(CommonParents,[])).

trimParents([H|T], ParentsToKeep) -> [trimParentsFor(H, ParentsToKeep)] ++ trimParents(T,ParentsToKeep);
trimParents([], _) -> [].

trimParentsFor([H|T],ParentsToKeep) -> 	
	case lists:member(H,ParentsToKeep) of
		false ->
			trimParentsFor(T,ParentsToKeep);
		true ->
			[H] ++ trimParentsFor(T,ParentsToKeep)
	end;
trimParentsFor([],_) -> [].


getParentCounts([H|T],Res) -> getParentCounts(T, Res ++ [lists:nth(1,H)] ++ H);
getParentCounts([], Res) -> getCounts(Res, []).

getCounts([H|T], Res) -> 
	case lists:member(H,T) of
		false ->
			getCounts(T, Res);
		true ->
			getCounts(T,Res ++ [H])
	end;
getCounts([],Res) -> Res.

getCommonParents(Parents) -> getCommonParents(Parents, [], []).

getCommonParents([H|T],[],[]) -> getCommonParents(T, [H], [H]);
getCommonParents([H|T], CommonParents, CP2) -> getCommonParents(T, CommonParents ++ [H],CP2 ++ [findCommonParents(H,CommonParents,[])]);
getCommonParents([], _,CP2) -> CP2.

findCommonParents(Parents, [H|T], []) -> findCommonParents(Parents, T, [findCommonLength(Parents, H, 0)]);
findCommonParents(Parents, [H|T], [L|_]) -> 
	NewLength = findCommonLength(Parents, H, 0),
	if
		NewLength > L ->
			findCommonParents(Parents, T, [NewLength]);
		true ->
			findCommonParents(Parents, T, [L])
	end;
findCommonParents(Parents,[], [L|_]) ->
	{NewParents,_}  = lists:split(length(Parents) - L + 1, Parents),
	NewParents.

findCommonLength([], _, CommonLength) -> CommonLength;
findCommonLength(_, [], CommonLength) -> CommonLength;
findCommonLength(L1, L2, CommonLength) -> 
	El1 = lists:nth(length(L1), L1),
	El2 = lists:nth(length(L2), L2),
	if 
		El1 == El2 ->
			{NewL1,_}  = lists:split(length(L1) - 1, L1),
			{NewL2,_}  = lists:split(length(L2) - 1, L2),
			findCommonLength(NewL1, NewL2, CommonLength + 1);
		true ->
			CommonLength
	end.

getParentsFor([{_,Taxo,_}|T]) -> [getParents(Taxo)] ++ getParentsFor(T);
getParentsFor([]) -> [].

getParents(Taxo) ->
	{_,{starsi,ListParents}, _} = getConcept(Taxo),
	[H|_] = ListParents,
	if 
		H == [] -> 
			[Taxo];
		true ->
			[Parent|_] = H,
			[Taxo] ++ getParents(Parent)
	end.
%%---------------bookmark search-------------------------------------

bookmarkSearch(User,Query,"True") ->
	Res = processQuery(User,true,bracketCountCheck(Query,0,Query),[],[]) ++ bmNamesearch(User,Query),
	jsonConvert(removeDuplicates(Res));
bookmarkSearch(User,Query,"False") ->
	Res = processQuery(User,false,bracketCountCheck(Query,0,Query),[],[]) ++ bmNamesearch(User,Query),
	jsonConvert(removeDuplicates(Res)).

bmNamesearch(User,Query) -> 
	filterBmsByName(get_user_bookmarksDB4(nameToAtom(toString(User))),Query).

filterBmsByName([H|T],Query) ->
	case string:str(H, Query) > 0 of
		false ->
			filterBmsByName(T,Query);
		true ->
			[H] ++ filterBmsByName(T,Query)
	end;
filterBmsByName([],_) -> [].

processQuery(User,Children,[[H|T]|T2],_,_)    -> processQuery(User,Children,T2,[],processQuery(User,Children,[H|T],[],[]));
processQuery(User,Children,[38|T], Buffer,[]) -> processQuery(User,Children,getTextFromNextToken(T),[],removeDuplicates(getIntersection([getBmsForWord(User,Buffer,Children)] ++ [getTextToNextToken(T,[],User,Children)])));
processQuery(User,Children,[38|T], _,CurrRes) -> processQuery(User,Children,getTextFromNextToken(T),[],removeDuplicates(getIntersection([CurrRes] ++ [getTextToNextToken(T,[],User,Children)])));
processQuery(User,Children,[124|T],Buffer,[]) -> processQuery(User,Children,getTextFromNextToken(T),[],removeDuplicates(getBmsForWord(User,Buffer,Children) ++ getTextToNextToken(T,[],User,Children)));
processQuery(User,Children,[124|T],_,CurrRes) -> processQuery(User,Children,getTextFromNextToken(T),[],removeDuplicates(CurrRes ++ getTextToNextToken(T,[],User,Children)));
processQuery(User,Children,[33|T],_,CurrRes)  -> processQuery(User,Children,getTextFromNextToken(T),[],removeDuplicates(CurrRes ++ getNotIncludedBms(getTextToNextToken(T,[],User,Children),get_user_bookmarksDB4(nameToAtom(toString(User))))));
processQuery(User,Children,[91|T],_,CurrRes)  -> processQuery(User,Children,getTextFromNextToken(T),[],removeDuplicates(CurrRes ++ getTextToNextToken(T,[],User,Children)));
processQuery(User,Children,[Text|T],Buffer,CurrRes) ->  processQuery(User,Children,T,Buffer ++ [Text],CurrRes);
processQuery(_,_,[],[],Res)                   -> Res;
processQuery(_,_,[],_,[H|T])   				  -> [H|T];
processQuery(User,Children,[],Buffer,[])      -> getBmsForWord(User,Buffer,Children).

getNotIncludedBms([H|T],Bms) -> getNotIncludedBms(T,lists:delete(H, Bms));
getNotIncludedBms([],Bms) -> Bms. 

getBmsForWord(User,Word,true) -> searchTaxoBmsWithChildren(User,[removeEmptyChars(Word)]);
getBmsForWord(User,Word,false) -> searchTaxoBmsWithoutChildren(User,[removeEmptyChars(Word)]).

getBmsForId(User,Id,Children) -> 
	IdList = string:tokens(Id,","),
	if
		length(IdList) == 1 ->
			A = removeEmptyChars2(lists:nth(1,IdList)),
			getBmsForId2(User,checkIfIdExists(A),A,Children);		
		length(IdList) == 2 ->
			A = removeEmptyChars2(lists:nth(2,IdList)),
			getBmsForId2(User,checkIfIdExists(A),A,Children);
		true ->
			[]	
    end.

removeEmptyChars2(Str) -> 
	A = string:tokens(Str, " "),
	if
		length(A) > 0 ->
			lists:nth(1,A);
		true ->
			[]
	end.


checkIfIdExists(Id) -> 
	Res = getConcept(Id),
	if 
		Res == [] -> 
			false;
		true ->
			true
	end.

getBmsForId2(_,false,_,_) -> [];
getBmsForId2(User,true,Id,true) -> getTaxoBmsWithChildrenForTaxoId(User,Id);
getBmsForId2(User,true,Id,false) -> getTaxoBmsWithoutChildrenForTaxoId(User,Id).

removeEmptyChars(Str) -> joinListWithEmptySpaces(string:tokens(Str, " ")).

joinListWithEmptySpaces([H|[]]) -> H; 
joinListWithEmptySpaces([H|T]) -> H ++ " " ++  joinListWithEmptySpaces(T);
joinListWithEmptySpaces([]) -> "".

getTextToNextToken([[H|T]|_],_,User,Children) -> processQuery(User,Children,[H|T],[],[]);
getTextToNextToken([38|_], Buffer,User,Children) -> getBmsForWord(User,Buffer,Children);
getTextToNextToken([124|_], Buffer,User,Children) -> getBmsForWord(User,Buffer,Children);
getTextToNextToken([33|T], _,User,Children) ->  getNotIncludedBms(getTextToNextToken(T,[],User,Children),get_user_bookmarksDB4(nameToAtom(toString(User))));
getTextToNextToken([93|_], Buffer,User,Children) -> getBmsForId(User,Buffer,Children);
getTextToNextToken([Text|T],Buffer,User,Children) -> getTextToNextToken(T,Buffer ++ [Text],User,Children);
getTextToNextToken([],Buffer,User,Children) -> getBmsForWord(User,Buffer,Children).

getTextFromNextToken([[_|_]|T2]) -> T2;
getTextFromNextToken([38|T]) -> [38|T];
getTextFromNextToken([124|T]) -> [124|T];
getTextFromNextToken([33|_]) -> [];
getTextFromNextToken([_|T]) -> getTextFromNextToken(T);
getTextFromNextToken([]) -> [].

bracketCountCheck([40|T],Brackets,Str) -> bracketCountCheck(T,Brackets + 1,Str);
bracketCountCheck([41|T],Brackets,Str) -> bracketCountCheck(T,Brackets - 1,Str);
bracketCountCheck([_|T],Brackets,Str) -> bracketCountCheck(T,Brackets,Str);
bracketCountCheck([],0,Str) -> bracketToList(Str);
bracketCountCheck([],_,_) -> [].

bracketToList([40|T]) -> [[bracketToList(T)]] ++ bracketToListHelper(T,0);
bracketToList([41|_]) -> [];
bracketToList([H|T]) -> [H] ++  bracketToList(T);
bracketToList(_) -> [].

bracketToListHelper([40|T],Count) -> bracketToListHelper(T, Count + 1);
bracketToListHelper([41|T],0) -> bracketToList(T);
bracketToListHelper([41|T],Count) -> bracketToListHelper(T, Count - 1);
bracketToListHelper([_|T],Count) -> bracketToListHelper(T, Count);
bracketToListHelper(_,_) -> [].

%----------------concept search-----------------------------

conceptSearchAndSortForUser(User, Word, ContextConcepts) ->
	Res = processConceptSearchQuery(Word),
	SortedByNumberOfBmsAttached = sortByNumberOfBmsAttached(Res, nameToAtom(toString(User)++"index"),[]),
	SortedByConceptsInContext = sortByConceptsInContext(SortedByNumberOfBmsAttached,string:tokens(ContextConcepts, ",")),
	formatForJsonFor(sortByPositionInHierarchyAndFormat(SortedByConceptsInContext)).

processConceptSearchQuery(Query) ->	processConceptQuery(replaceWordsWithConcepts(Query,[]),[]).

replaceWordsWithConcepts([38|T], Buffer) -> [findConceptsForString(removeEmptyChars(Buffer))] ++ [38] ++ replaceWordsWithConcepts(T,[]);
replaceWordsWithConcepts([124|T],Buffer) -> [findConceptsForString(removeEmptyChars(Buffer))] ++ [124] ++ replaceWordsWithConcepts(T,[]);
replaceWordsWithConcepts([Text|T],Buffer) ->  replaceWordsWithConcepts(T,Buffer ++ [Text]);
replaceWordsWithConcepts([],[]) -> [];
replaceWordsWithConcepts([],Buffer) -> [findConceptsForString(removeEmptyChars(Buffer))].

processConceptQuery([H|[38|[H2|T]]], []) -> processConceptQuery(T, removeDuplicates(getIntersection([H] ++ [H2])));
processConceptQuery([H|[124|[H2|T]]], []) -> processConceptQuery(T, removeDuplicates(H ++ H2));
processConceptQuery([38|[H|T]], Res) -> processConceptQuery(T, removeDuplicates(getIntersection([Res]++[H])));
processConceptQuery([124|[H|T]], Res) -> processConceptQuery(T, removeDuplicates(Res ++ H));
processConceptQuery([38|T], Res) -> processConceptQuery(T, Res);
processConceptQuery([124|T], Res) -> processConceptQuery(T, Res);
processConceptQuery([H|T], Res) -> processConceptQuery(T, removeDuplicates(Res ++ H));
processConceptQuery(_, Res) -> removeDuplicates(Res).

sortByPositionInHierarchyAndFormat([{_,H}|T]) -> 
	getSortedRes(sortByPositionInHierarchy(H, [])) ++ sortByPositionInHierarchyAndFormat(T);
sortByPositionInHierarchyAndFormat([]) -> [].

sortByNumberOfBmsAttached([H|T],User,Res) ->
	NumOfBms = getNumOfBmsAttached(get_TaxoIndexDB(User, lists:nth(1,H))),
	sortByNumberOfBmsAttached(T,User,addToRes(NumOfBms,H,Res));
sortByNumberOfBmsAttached([],_,Res) -> Res.

addToRes(NumOfBms,Taxo,[{N,Taxos}|T]) -> 
	if 
		NumOfBms < N ->
			[{N,Taxos}] ++ addToRes(NumOfBms,Taxo,T);
		NumOfBms == N ->
			[{N,Taxos ++ [Taxo]}] ++ T;
		true -> 
			[{NumOfBms,[Taxo]}] ++ [{N,Taxos}] ++ T
	end;
addToRes(NumOfBms,Taxo,[]) -> [{NumOfBms,[Taxo]}].

getNumOfBmsAttached([{_,_,Bms}|_]) -> length(Bms);
getNumOfBmsAttached(_) -> 0.
	
formatForJson([H|T]) -> [toBinaryString(H)] ++ formatForJson(T);
formatForJson([]) -> [].

formatForJsonFor([H|T]) -> [formatForJson(H)] ++ formatForJsonFor(T);
formatForJsonFor([])-> [].

sortByPositionInHierarchy([H|T], Res)-> 
	[H1|_] = H,
	Len = length(getParentsForTaxo(H1)),
	sortByPositionInHierarchy(T, sortHelper(Res,{Len,H}));
sortByPositionInHierarchy([],Res) -> Res.

sortHelper([], Taxo) -> [Taxo];
sortHelper([{Len1,Taxo1}|T], {Len2,Taxo2}) -> 
	if 
		Len2 < Len1 ->
			[{Len2,Taxo2},{Len1,Taxo1}|T];
		true -> 
			[{Len1,Taxo1}] ++ sortHelper(T,{Len2,Taxo2})
	end.

getParentsForTaxo(Taxo) ->
	case  getConcept(Taxo) of
		{_,{starsi,ListStarsi}, _} ->
			[H|_] = ListStarsi,
			if 
				H == [] -> 
					[Taxo];
				true ->
					[Parent|_] = H,
					[Taxo] ++ getParentsForTaxo(Parent)
			end;
		_ ->
			[]
	end.

getSortedRes([{_,H}|T]) -> [H] ++ getSortedRes(T);
getSortedRes([]) -> [].

sortByConceptsInContext(Res, []) -> Res;
sortByConceptsInContext([{_,H}|T], ContextConcepts) ->  makeNewGroupsSortedByContext(H,ContextConcepts) ++ sortByConceptsInContext(T, ContextConcepts);
sortByConceptsInContext([],_) -> [].

makeNewGroupsSortedByContext(H,ContextConcepts) -> 	makeNewGroupsSortedByContextHelper(sortByDistanceToConcepts(H,ContextConcepts),[]).

makeNewGroupsSortedByContextHelper([],Res) -> Res;
makeNewGroupsSortedByContextHelper([H|T],Res) -> makeNewGroupsSortedByContextHelper(T,sortByGroup(H,Res)).


sortByGroup({X,H},[{N,Taxos}|T]) -> 
	if 
		X < N ->
			[{N,Taxos}] ++ sortByGroup({X,H},T);
		X == N ->
			[{N,Taxos ++ [H]}] ++ T;
		true -> 
			[{X,[H]}] ++ [{N,Taxos}] ++ T
	end;
sortByGroup({X,H},[]) -> [{X,[H]}].

sortByDistanceToConcepts([H|T], ContextConcepts) -> sortByDistanceToConcept(H, ContextConcepts,0) ++ sortByDistanceToConcepts(T,ContextConcepts);
sortByDistanceToConcepts([], _) -> [].

sortByDistanceToConcept(Con, [H|T], Res) -> sortByDistanceToConcept(Con, T, Res + getDistanceToConcept(Con,[H],2));
sortByDistanceToConcept(Con,[],Res) -> [{Res,Con}].

getDistanceToConcept(_,_,0) -> 0;
getDistanceToConcept(Con,List,Limit) -> 
	case lists:member(lists:nth(1,Con),List) of
		false ->
			getDistanceToConceptHelperFor(Con,List,Limit,[]);
		true ->
			Limit
	end.

getDistanceToConceptHelperFor(Con,[H|T],Limit,Res) ->
	{_,{starsi,Starsi},{otroki,Otroki}} = getConcept(H),
	getDistanceToConceptHelperFor(Con,T,Limit,Res ++ getIdsFromMsg(Starsi ++ Otroki));
getDistanceToConceptHelperFor(Con,[],Limit,Res) -> getDistanceToConcept(Con,Res,Limit-1).

%%---------------context-------------------------------------
addContext(User, Context) ->
	User2 = nameToAtom(toString(User)++"context"),
	{ContextName,TagsString,QueriesString} = Context,
	Tags = string:tokens(TagsString, ","), 
	add_TaxoBookmarkDB(User2, ContextName,getTaxoForContext(Tags), QueriesString), ok.

getContexts(User) ->
	User2 = nameToAtom(toString(User)++"context"),
	getContextsFor(getTable(User2)).

getContextsFor([{_, Name, Taxos,Queries}|T]) -> [[[<<"contextName">>,toBinaryString(Name)],[<<"taxoTags">>,formatForJsonTuples(Taxos)],[<<"queries">>,toBinaryString(Queries)]]] ++ getContextsFor(T);
getContextsFor([]) -> [].

formatForJsonTuples([{A,B}|T])-> [[toBinaryString(A),toBinaryString(B)]] ++ formatForJsonTuples(T);
formatForJsonTuples([]) -> [].

getTaxoForContext([Taxo|Tail]) -> getTaxoForContext2(getConcept(Taxo)) ++ getTaxoForContext(Tail);
getTaxoForContext([]) -> [].

getTaxoForContext2({Beseda,_,_}) -> [{lists:nth(1,Beseda),lists:nth(2,Beseda)}];
getTaxoForContext2([]) -> [].

removeContext(User, Name) -> 
	User2 = nameToAtom(toString(User)++"context"),
	Con = getContextDB(User2, toString(Name)),
	case Con of
		[] -> 
			context_removed;
		_ ->
			remove_recordDB(User2, Name), context_removed
	end.
%%----------------help functions-------------------------			
nameToAtom(Name)->
	case is_list(Name) of
		false ->
			Name;
		true ->
			try list_to_existing_atom(Name) of
				Return -> Return
			catch
				_:_ -> list_to_atom(Name)
			end
	end.
	
toString(Word)->
	case io_lib:printable_unicode_list(Word) of
		true ->
			Word;
		false ->
			case is_atom(Word) of
				true ->
					atom_to_list(Word);
				false ->
					case is_integer(Word) of
						true ->
							integer_to_list(Word);
						false ->
							Word
					end
			end
	end.
	
removeDuplicates([])    -> [];
removeDuplicates([H|T]) -> [H | [X || X <- removeDuplicates(T), X /= H]].

%%------------------mnesia----------------------------------------
do_this_once() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(users,[{attributes, record_info(fields, userswithconceptcounter)},{disc_copies,[node()]}]),
	mnesia:create_table(testtable,[{attributes, record_info(fields, testtable)},{disc_copies,[node()]}]).

updateUsersTable() ->
	case length(mnesia:table_info(users,attributes)) of
		2 ->
			Transformer =
			fun(X)->
				#userswithconceptcounter{name = X#users.name,
					pass = X#users.pass,
					conceptcount = 1}
				end,
			mnesia:transform_table(users, Transformer, record_info(fields, userswithconceptcounter), userswithconceptcounter);
		_ ->
			ok
	end.

checkForConceptsTable(TableName) ->
	case table_exists(TableName) of
		false ->
				mnesia:create_table(TableName,[{attributes,record_info(fields,userconcepttable)},{disc_copies,[node()]}]);
		true ->
			ok
	end.

table_exists(TableName) ->
   Tables = mnesia:system_info(tables),
   lists:member(TableName,Tables).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
	
register_HelpDB() ->
	do(qlc:q([{X#userswithconceptcounter.name} || X <- mnesia:table(users)])).
	
login_HelpDB() ->
	do(qlc:q([{X#userswithconceptcounter.name, X#userswithconceptcounter.pass} || X <- mnesia:table(users)])).

add_userDB(Name,IndexName,ContextName,FileName,Pass,ConceptName) ->
	Row = {users,Name, Pass, 1},
	F = fun() ->
	mnesia:write(Row)
	end,
	mnesia:transaction(F),
	mnesia:create_table(Name,[{attributes, record_info(fields, usertaxobookmarks)},{disc_copies,[node()]}]),
	mnesia:create_table(IndexName,[{attributes, record_info(fields, taxoindex)},{disc_copies,[node()]}]),
	mnesia:create_table(ContextName,[{attributes, record_info(fields, contexttable)},{disc_copies,[node()]}]),
	mnesia:create_table(FileName,[{attributes, record_info(fields, userbookmarkfiles)},{disc_copies,[node()]}]),
	mnesia:create_table(ConceptName,[{attributes,record_info(fields,userconcepttable)},{disc_copies,[node()]}]).

	
getTable(TableName) ->
	do(qlc:q([X || X <- mnesia:table(TableName)])).
	
get_user_bookmarksDB3(User) ->
	do(qlc:q([{X#usertaxobookmarks.name, X#usertaxobookmarks.url, X#usertaxobookmarks.taxotags} || X <- mnesia:table(User)])).

get_user_bookmarksDB4(User) ->
    do(qlc:q([X#usertaxobookmarks.name || X <- mnesia:table(User)])).
	
get_bookmarkDB(User, Name) ->
	do(qlc:q([X || X <- mnesia:table(User), X#usertaxobookmarks.name == Name])).

get_bookmarkFilesDB(User, Name) ->
	do(qlc:q([X || X <- mnesia:table(User), X#userbookmarkfiles.name == Name])).

getContextDB(User, Name) ->
	do(qlc:q([X || X <- mnesia:table(User), X#contexttable.context == Name])).
	
get_TaxoIndexDB(User, Taxotag) ->
	do(qlc:q([X || X <- mnesia:table(User), X#taxoindex.taxotag == Taxotag])).

getTagsFromTaxoIndex(User) ->
	do(qlc:q([X#taxoindex.taxotag || X <- mnesia:table(User)])).

add_TaxoBookmarkDB(User, Name, Url, Taxotags) ->
	F = fun() ->
		mnesia:write({User, Name, Url, Taxotags})
	end,
	mnesia:transaction(F).

add_TaxoBookmarkDB2(User, Taxotag, Bookmarks) ->
	F = fun() ->
		mnesia:write({User, Taxotag, Bookmarks})
	end,
	mnesia:transaction(F).

remove_recordDB(User,Name) ->
	Oid = {User, Name},
	F = fun() ->
		mnesia:delete(Oid)
	end,
	mnesia:transaction(F).

writeDB(X) ->
	F = fun() ->
		mnesia:write(X)
	end,
	mnesia:transaction(F).

getUserDB(User) ->
	do(qlc:q([{X#userswithconceptcounter.name, X#userswithconceptcounter.pass, X#userswithconceptcounter.conceptcount} || X <- mnesia:table(users), X#userswithconceptcounter.name == User])).

%%------------main server process------------------------	
loop() ->
    receive
		{From, {register, Name, Pass}} ->
			loginproc ! {register,Name,Pass,From},
			loop();
		{From, {login, Name, Pass}} ->
			loginproc ! {login,Name,Pass,From},
			loop();
		{From, {logout, Name, Pass}} ->
			loginproc ! {logout,Name,Pass,From},
			loop();
		{From, {userProc,Name,Pass,Q}} ->
			loginproc ! {userProc,Name,Pass,Q,From},
			loop();
		{From, {getActiveUsers}} ->
			From ! {bms, get(users)},
			loop();
		{_, {putActiveUsers, Users}} ->
			put(users, Users),
			loop();
		{forward, Pid, Q} ->
			Pid ! {bms, Q},
			loop();
		M -> 
			io:format("Messageloop=~w~n",[M]),
			loop()
    end.
	
%%----------------setup & general functions-----------------------
restarter() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(fun()-> setup() end),
	register(bms, Pid),
	receive
		{'EXIT', Pid, normal} -> % not a crash
			ok;
		{'EXIT', Pid, _} ->
			restarter()
	end.
	
setup() ->
	spawn(fun()-> restarterDB() end),
	spawn(fun()-> restarterLogin() end),
	spawn(fun()-> restarterTaxo() end),
	spawn(fun()-> timer_for_inactive() end),
	loop().
	
restarterDB() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(fun()-> setupDB() end),
	register(dbs, Pid),
	receive
		{'EXIT', Pid, normal} -> % not a crash
			ok;
		{'EXIT', Pid, _} ->
			restarterDB()
	end.
	
setupDB() ->
	mnesia:start(),
	case mnesia:create_table(testtable,[{attributes, record_info(fields, testtable)},{disc_copies,[node()]}]) of
        {aborted,{already_exists, _}} ->
			mnesia:wait_for_tables([users], 2000),
			updateUsersTable();
        {aborted, _} ->
			mnesia:stop(), do_this_once(),mnesia:wait_for_tables([users], 2000)			
	end.

restarterTaxo() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(fun()-> loopTaxo() end),
	register(taxo, Pid),
	receive
		{'EXIT', Pid, normal} -> % not a crash
			ok;
		{'EXIT', Pid, _} ->
			restarterTaxo()
	end.
	
restarterLogin() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(fun()-> loopLogin() end),
	register(loginproc, Pid),
	U = rpc({getActiveUsers}),
	if
		U == undefined ->
			ok;
		true ->
			loginproc ! {updateUsers,U}
	end,
	receive
		{'EXIT', Pid, normal} -> % not a crash
			ok;
		{'EXIT', Pid, _} ->
			restarterLogin()
	end.	

%%-------------------login/active users process-----------------------	
loopLogin() ->
    receive
		{login, Name, Pass, From} ->
			bms ! {forward, From, loginS(Name, Pass, From)},
			loopLogin();
		{logout, Name, Pass, From} ->
			bms ! {forward, From, logoutS(Name, Pass)},
			loopLogin();
		{register, Name, Pass, From} ->
			bms ! {forward, From, registerS(Name, Pass)},
			loopLogin();
		{userProc, User, Pass, Q, From} ->
			case check_status_and_login(nameToAtom(User),Pass,From) of
				true -> 
					loginproc ! {userProcForward, User, Q, From};
				false ->
					bms ! {forward, From, you_are_not_logged_in}
			end,
			loopLogin();
		{userProcForward, User, Q, From} ->
			User2 = nameToAtom(User),
			{X,_,_} = get(User2),
			put(User2,{X,time_normal_format(erlang:timestamp()),From}),
			bms ! {self(), {putActiveUsers,get()}},
			X ! {From,Q},
			loopLogin();
		{error, User} ->
			User2 = nameToAtom(User),
			{_,_,_,From} = get(User2),
			bms ! {forward,From,{error, badarg}},
			loopLogin();
		{regproc, Name,Pid,From} ->
			registerProc(Name,Pid,From),
			loopLogin();
		{checkForInactive} ->
			checkForInactive(get()),
			loopLogin();
		{updateUsers,Users} ->
			updateActiveUsers(Users),
			loopLogin();
		M -> 
			io:format("MessageloopLogin=~w~n",[M]),
			loopLogin()
    end.

loginS(Name, Pass, From) ->
	Name2 = nameToAtom(Name),
	NameConcept = nameToAtom(toString(Name)++"concepts"),
	case member2(Name2, Pass, login_HelpDB()) of
		true -> 
			check_if_already_logged_in(Name2), checkForConceptsTable(NameConcept), spawn(fun()-> restarterForUserProc(Name2,From) end),success;
		false ->
			login_failed
	end.
	
member2(X,Y, [{X,Y}|_]) -> true;
member2(X,Y, [_|T]) -> member2(X,Y, T);
member2(_,_, []) -> false.

check_status_and_login(Name,Pass,From) ->
	case get(userList) of
		undefined ->
			put(userList, login_HelpDB());
		_ ->
			ok
	end,
	case member2(Name, Pass, get(userList)) of
		true -> 
			case get(Name) of
				undefined -> 
					Pid = spawn(fun()-> loopProc(Name) end),
					loginproc ! {regproc, Name, Pid, From},
					spawn(fun()-> restarterForUserProc(Name,From,Pid) end),
					true;
				_ ->
					true
			end;
		false ->
			false
	end.


registerProc(User,Pid,From) ->
	put(User,{Pid,time_normal_format(erlang:timestamp()),From}), bms ! {self(), {putActiveUsers,get()}},  ok.

restarterForUserProc(Name,From) ->
	process_flag(trap_exit, true),
	Pid = spawn_link(fun()-> loopProc(Name) end),
	loginproc ! {regproc, Name, Pid, From},
	receive
		{'EXIT', Pid, normal} -> % not a crash
			ok;
		{'EXIT', Pid, _} ->
			loginproc ! {error, Name},
			restarterForUserProc(Name,From)
	end.

restarterForUserProc(Name,From,Pid) ->
	process_flag(trap_exit, true),
	link(Pid),
	receive
		{'EXIT', Pid, normal} -> % not a crash
			ok;
		{'EXIT', Pid, _} ->
			loginproc ! {error, Name},
			restarterForUserProc(Name,From)
	end.

logoutS(User, Pass) ->
	User2 = nameToAtom(User),
	case member2(User2, Pass, get(userList)) of
		true -> 
			case get(User2) of
				undefined -> 
					successfully_logged_out;
				_ ->
					{X,_,_} = get(User2), X ! {end_proc},put(User2, undefined), bms ! {self(), {putActiveUsers,get()}}, successfully_logged_out
			end;
		false ->
			you_are_not_logged_in
	end.
	
logoutS(User) ->
	User2 = nameToAtom(User),
	case get(User2) of
		undefined -> 
			successfully_logged_out;
		_ ->
			{X,_,_} = get(User2), X ! {end_proc},put(User2, undefined), bms ! {self(), {putActiveUsers,get()}}, successfully_logged_out
	end.

registerS(Name, Pass) ->
	Name2 = nameToAtom(Name),
	IndexName = nameToAtom(toString(Name)++"index"),
	ContextName = nameToAtom(toString(Name)++"context"),
	FileName = nameToAtom(toString(Name)++"files"),
	ConceptName = nameToAtom(toString(Name)++"concepts"),
	filelib:ensure_dir("/files/"++ toString(Name) ++ "/"),
	case member(Name2, register_HelpDB() ) of
		true ->
			error_this_user_already_exists;
		false ->
			add_userDB(Name2,IndexName,ContextName,FileName,Pass,ConceptName),put(userList, login_HelpDB()),success
	end.
	
member(X, [{X}|_]) -> true;
member(X, [_|T]) -> member(X, T);
member(_, []) -> false.
	
check_if_already_logged_in(Name) ->
	case get(Name) of
		undefined -> 
			ok;
		{_,_,_} ->
			logoutS(Name),ok
	end.

checkForInactive([{N,{_,Time,_}}|T])-> 	
	Y = time_normal_format(erlang:timestamp()),
	Z = Y - Time,
	if
		Z >= 1200 ->
			logoutS(N); 
		true ->
			ok
	end, checkForInactive(T);
checkForInactive([_|T]) -> checkForInactive(T);
checkForInactive([]) -> ok.

time_normal_format({MegaSecs,Secs,_}) ->
	(MegaSecs*1000000 + Secs).
	
timer_for_inactive() ->
	loginproc ! {checkForInactive},
	timer:sleep(60000),
	timer_for_inactive().
	
updateActiveUsers([{N,C}|T]) -> put(N,C),updateActiveUsers(T);
updateActiveUsers([]) -> ok.

%%-------------------taxo process-----------------------	
loopTaxo() ->
	case get(dataLoaded) of
        undefined  -> taxoLoad(),loopTaxo();
        _ -> 
		receive
			_ -> 
				loopTaxo()
    	end
    end.

taxoLoad() -> 
	ets:new(dataindex, [set, named_table]),
	ets:new(childrenindex, [set, named_table]),
	ets:new(searchindex, [set, named_table]),
	io:format("Loading taxonomy ~n"),
	readDataIndex("data.txt"),
	readSearchIndex("searchIndex.txt"),
	io:format("Taxo has been loaded ~n"),
	put(dataLoaded, 1).


readDataIndex(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try getAllLinesForDataIndex(Device)
      after file:close(Device)
    end.

getAllLinesForDataIndex(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> List = string:tokens(Line, ";\n"), Id = lists:nth(1,List), ets:insert(dataindex,{Id,List}), addToChildrenIndex([{Id,List}]), getAllLinesForDataIndex(Device)
    end.

readSearchIndex(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try getAllLinesForSearchIndex(Device)
      after file:close(Device)
    end.

getAllLinesForSearchIndex(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> List = string:tokens(Line, ";\n"), ets:insert(searchindex,{string:to_lower(lists:nth(1,List)),lists:delete(lists:nth(1,List),List)}), getAllLinesForSearchIndex(Device)
    end.

addToChildrenIndex([{Id,[_,_,_,_,Otroci]}|T]) -> ets:insert(childrenindex,{Id,string:tokens(Otroci, ",")}), addToChildrenIndex(T);
addToChildrenIndex([{Id,[_,_,_,_]}|T]) -> ets:insert(childrenindex,{Id,[]}), addToChildrenIndex(T);
addToChildrenIndex([]) -> done.