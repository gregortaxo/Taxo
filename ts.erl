-module(ts). 
-export([go/0,service/3]). 

go() ->
   bs:start(), 	 
   inets:start(),
   inets:start(httpd, [ 
      {modules, [ 
         mod_alias, 
         mod_auth, 
         mod_esi, 
         mod_actions, 
         mod_cgi, 
         mod_dir,
         mod_get, 
         mod_head, 
         mod_log, 
         mod_disk_log 
      ]}, 
      
      {port,1236}, 
      {server_name,"ts"}, 
      {server_root,"/home/gregor/Documents"}, 
      {document_root,"/home/gregor/Documents"}, 
      {erl_script_alias, {"/erl", [ts]}}, 
      {error_log, "error.log"}, 
      {security_log, "security.log"}, 
      {transfer_log, "transfer.log"}, 
      
      {mime_types,[ 
         {"html","text/html"}, 
		 {"css","text/css"}, 
		 {"js","application/x-javascript"},  
		 {"json", "application/json"} ]} 
   ]). 
         
service(SessionID, _Env, _Input) -> 
	%%io:fwrite(_Input ++ "~n"),
	Res = handleRequest(_Input),
	JSONRes = jsx:encode(Res),
	mod_esi:deliver(SessionID, [JSONRes]).
	
handleRequest(Input) ->
	IsLegalJSON = jsx:is_json(list_to_binary(Input)),
	if 
		IsLegalJSON -> 	
			BinaryDecoded = jsx:decode(list_to_binary(Input)),
			handleRequestByType(BinaryDecoded,BinaryDecoded);
		true -> 
			{ok,RequestBin} = file:read_file("tsRequests/" ++ Input),
			file:delete("tsRequests/" ++ Input),
			IsLegalJSON2 = jsx:is_json(RequestBin),
			if 
				IsLegalJSON2 ->
					BinaryDecoded = jsx:decode(RequestBin),
					handleRequestByType(BinaryDecoded,BinaryDecoded);
				true -> 
					[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequest">>}]
			end
	end.
	
handleRequestByType([H|T],D) ->
	IsTuple = is_tuple(H),
	if
		IsTuple ->
			{Name,F} = H,
			if
				(Name == <<"fun">>) ->
					io:fwrite(binary_to_list(F) ++ "~n"),
					%%io:format("handleRequestByType: ~w~n~w~n", [erlang:process_info(self(),memory), erlang:process_info(self())]),
					Check = checkParams([<<"username">>,<<"password">>] ++ getRequestTypeParams(F),D),
					if
						Check ->
							U = getVal(<<"username">>,D),
							P = getVal(<<"password">>,D),
							N = atom_to_list(node()),
							if
								(F == <<"login">>) -> ret(list_to_binary(atom_to_list(bs:login(U,P,N))));
								(F == <<"regUser">>) -> ret(list_to_binary(atom_to_list(bs:regUser(U,P,N))));
								(F == <<"getUserBookmarks">>) -> ret(bs:getUserBookmarks(U,P,N));
								(F == <<"searchTaxo">>) -> ret(bs:searchTaxo(U,P,N,getVal(<<"word">>,D)));
								(F == <<"findTaxo">>) -> ret(bs:findTaxo(U,P,N,getVal(<<"word">>,D),getVal(<<"contextConcepts">>,D)));
								(F == <<"addTaxoBm">>) -> ret(bs:addTaxoBm(U,P,N,getVal(<<"name">>,D),getVal(<<"url">>,D),getVal(<<"filename">>,D),getVal(<<"filedata">>,D),getVal(<<"filepreview">>,D)));
								(F == <<"addTaxoBmDataChunk">>) -> ret(bs:addTaxoBmDataChunk(U,P,N,getVal(<<"name">>,D),getVal(<<"filedata">>,D)));
								(F == <<"addTaxoTag">>) -> ret(bs:addTaxoTag(U,P,N,getVal(<<"name">>,D),[{getVal(<<"taxoId">>,D),getVal(<<"taxoName">>,D)}]));
								(F == <<"removeBookmark">>) -> ret(bs:removeBookmark(U,P,N,getVal(<<"name">>,D)));
								(F == <<"addContext">>) -> ret(bs:addContext(U,P,N,{getVal(<<"contextName">>,D),getVal(<<"taxoTags">>,D),getVal(<<"queries">>,D)}));
								(F == <<"getContexts">>) -> ret(bs:getContexts(U,P,N));
								(F == <<"removeContext">>) -> ret(bs:removeContext(U,P,N,getVal(<<"contextName">>,D)));
								(F == <<"newSearch">>) -> ret(bs:bmSearch(U,P,N,getVal(<<"query">>,D),getVal(<<"children">>,D)));
								(F == <<"getBookmarkFile">>) -> ret(bs:getBookmarkFile(U,P,N,getVal(<<"bookmark">>,D)));
								(F == <<"getFilePreview">>) -> ret(bs:getFilePreview(U,P,N,getVal(<<"bookmark">>,D)));
								(F == <<"getBookmarkTree">>) ->	ret(bs:getBookmarkTree(U,P,N));
								(F == <<"getSimilarBms">>) -> ret(bs:getSimilarBms(U,P,N,getVal(<<"bookmark">>,D)));
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestType">>}]
							end;
						true -> 
							[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
					end;
				true -> 
					handleRequestByType(T,D)
			end;
		true -> 
			[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequest">>}]
    end;
handleRequestByType([],_) -> [{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequest">>}].

ret(Res) -> [{<<"error">>,<<"false">>},{<<"result">>,Res}].

getRequestTypeParams(F) ->
	if
		(F == <<"login">>) -> [];
		(F == <<"regUser">>) -> [];
		(F == <<"getUserBookmarks">>) -> [];
		(F == <<"searchTaxo">>) -> [<<"word">>];
		(F == <<"findTaxo">>) ->   [<<"word">>,<<"contextConcepts">>];
		(F == <<"addTaxoBm">>) ->  [<<"name">>,<<"url">>,<<"filename">>,<<"filedata">>,<<"filepreview">>];
		(F == <<"addTaxoBmDataChunk">>) -> [<<"name">>,<<"filedata">>];
		(F == <<"addTaxoTag">>) -> [<<"name">>,<<"taxoId">>,<<"taxoName">>];
		(F == <<"removeBookmark">>) -> [<<"name">>];
		(F == <<"addContext">>) -> [<<"contextName">>,<<"taxoTags">>,<<"queries">>];
		(F == <<"getContexts">>) -> [];
		(F == <<"removeContext">>) -> [<<"contextName">>];
		(F == <<"newSearch">>) -> [<<"query">>,<<"children">>];
		(F == <<"getBookmarkFile">>) -> [<<"bookmark">>];
		(F == <<"getFilePreview">>) -> [<<"bookmark">>];
		(F == <<"getBookmarkTree">>) -> [];
		(F == <<"getSimilarBms">>) -> [<<"bookmark">>];
		true -> []
	end.

checkParams([H|T],Data) -> 
	ParamExists = getVal(H,Data),
	if
		ParamExists == false ->
			false;
		true -> 
			checkParams(T,Data)
    end;
checkParams([],_) -> true.

getVal(Param, [H|T]) ->
	IsTuple = is_tuple(H),
	if
		IsTuple ->
			{Name,Value} = H,
			if
				(Name == Param) ->
					binary_to_list(Value);
				true -> 
					getVal(Param,T)
			end;
		true -> 
			getVal(Param,T)
    end;
getVal(_, []) ->	false.
