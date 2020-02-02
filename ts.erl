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
			[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequest">>}]
	end.
	
handleRequestByType([H|T],BinaryDecoded) ->
	IsTuple = is_tuple(H),
	if
		IsTuple ->
			{Name,Value} = H,
			if
				(Name == <<"fun">>) ->
					io:fwrite(binary_to_list(Value) ++ "~n"),
					%%io:format("handleRequestByType: ~w~n~w~n", [erlang:process_info(self(),memory), erlang:process_info(self())]),
					if
						(Value == <<"login">>) ->
							Check = checkParams([<<"username">>,<<"password">>],BinaryDecoded),
							if
								Check ->
									Res = atom_to_list(bs:login(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node()))),
									[{<<"error">>,<<"false">>},{<<"result">>,list_to_binary(Res)}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"regUser">>) ->
							Check = checkParams([<<"username">>,<<"password">>],BinaryDecoded),
							if
								Check ->
									Res = atom_to_list(bs:regUser(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node()))),
									[{<<"error">>,<<"false">>},{<<"result">>,list_to_binary(Res)}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"getUserBookmarks">>) ->
							Check = checkParams([<<"username">>,<<"password">>],BinaryDecoded),
							if
								Check ->
									Res = bs:getUserBookmarks(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node())),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"searchTaxo">>) ->
							Check = checkParams([<<"username">>,<<"password">>,<<"word">>],BinaryDecoded),
							if
								Check ->
									Res = bs:searchTaxo(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node()),binary_to_list(getParameter(<<"word">>,BinaryDecoded))),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"findTaxo">>) ->
							Check = checkParams([<<"username">>,<<"password">>,<<"word">>],BinaryDecoded),
							if
								Check ->
									Res = bs:findTaxo(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node()),binary_to_list(getParameter(<<"word">>,BinaryDecoded))),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"addTaxoBm">>) ->
							Check = checkParams([<<"username">>,<<"password">>,<<"name">>,<<"url">>,<<"filename">>,<<"filedata">>,<<"filepreview">>],BinaryDecoded),
							if
								Check ->
									Name2 = binary_to_list(getParameter(<<"name">>,BinaryDecoded)),
									Url = binary_to_list(getParameter(<<"url">>,BinaryDecoded)),
									FileName = binary_to_list(getParameter(<<"filename">>,BinaryDecoded)),
									FileData = binary_to_list(getParameter(<<"filedata">>,BinaryDecoded)),
									FilePreview = binary_to_list(getParameter(<<"filepreview">>,BinaryDecoded)),
									Res = bs:addTaxoBm(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node()),Name2,Url,FileName,FileData,FilePreview),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"addTaxoBmDataChunk">>) ->
							Check = checkParams([<<"username">>,<<"password">>,<<"name">>,<<"filedata">>],BinaryDecoded),
							if
								Check ->
									Name2 = binary_to_list(getParameter(<<"name">>,BinaryDecoded)),
									FileData = binary_to_list(getParameter(<<"filedata">>,BinaryDecoded)),
									Res = bs:addTaxoBmDataChunk(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node()),Name2,FileData),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"addTaxoTag">>) ->
							Check = checkParams([<<"username">>,<<"password">>,<<"name">>,<<"taxoId">>,<<"taxoName">>],BinaryDecoded),
							if
								Check ->
									Name2 = binary_to_list(getParameter(<<"name">>,BinaryDecoded)),
									TmpTuple = [{binary_to_list(getParameter(<<"taxoId">>,BinaryDecoded)),binary_to_list(getParameter(<<"taxoName">>,BinaryDecoded))}],
									Res = bs:addTaxoTag(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node()),Name2,TmpTuple),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"removeBookmark">>) ->
							Check = checkParams([<<"username">>,<<"password">>,<<"name">>],BinaryDecoded),
							if
								Check ->
									Res = bs:removeBookmark(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node()),binary_to_list(getParameter(<<"name">>,BinaryDecoded))),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"addContext">>) ->
							Check = checkParams([<<"username">>,<<"password">>,<<"contextName">>,<<"taxoTags">>,<<"queries">>],BinaryDecoded),
							if
								Check ->
									TmpTuple = {binary_to_list(getParameter(<<"contextName">>,BinaryDecoded)),binary_to_list(getParameter(<<"taxoTags">>,BinaryDecoded)),binary_to_list(getParameter(<<"queries">>,BinaryDecoded))},
									Res = bs:addContext(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node()),TmpTuple),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"getContexts">>) ->
							Check = checkParams([<<"username">>,<<"password">>],BinaryDecoded),
							if
								Check ->
									Res = bs:getContexts(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node())),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"removeContext">>) ->
							Check = checkParams([<<"username">>,<<"password">>,<<"contextName">>],BinaryDecoded),
							if
								Check ->
									Res = bs:removeContext(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node()),binary_to_list(getParameter(<<"contextName">>,BinaryDecoded))),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"newSearch">>) ->
							Check = checkParams([<<"username">>,<<"password">>,<<"query">>,<<"children">>],BinaryDecoded),
							if
								Check ->
									Res = bs:bmSearch(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node()),
									binary_to_list(getParameter(<<"query">>,BinaryDecoded)),binary_to_list(getParameter(<<"children">>,BinaryDecoded))),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"getBookmarkFile">>) ->
							Check = checkParams([<<"username">>,<<"password">>,<<"bookmark">>],BinaryDecoded),
							if
								Check ->
									Res = bs:getBookmarkFile(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node()),
									binary_to_list(getParameter(<<"bookmark">>,BinaryDecoded))),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"getImagePreview">>) ->
							Check = checkParams([<<"username">>,<<"password">>,<<"bookmark">>],BinaryDecoded),
							if
								Check ->
									Res = bs:getImagePreview(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node()),
									binary_to_list(getParameter(<<"bookmark">>,BinaryDecoded))),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						(Value == <<"getBookmarkTree">>) ->
							Check = checkParams([<<"username">>,<<"password">>],BinaryDecoded),
							if
								Check ->
									Res = bs:getBookmarkTree(binary_to_list(getParameter(<<"username">>,BinaryDecoded)),binary_to_list(getParameter(<<"password">>,BinaryDecoded)),atom_to_list(node())),
									[{<<"error">>,<<"false">>},{<<"result">>,Res}];
								true -> 
									[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestParams">>}]
							end;
						true -> 
							[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequestType">>}]
					end;
				true -> 
					handleRequestByType(T,BinaryDecoded)
			end;
		true -> 
			[{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequest">>}]
    end;
handleRequestByType([],_) -> [{<<"error">>,<<"true">>},{<<"errorInfo">>,<<"badRequest">>}].

checkParams([H|T],Data) -> 
	ParamExists = getParameter(H,Data),
	if
		ParamExists == false ->
			false;
		true -> 
			checkParams(T,Data)
    end;
checkParams([],_) -> true.

getParameter(Param, [H|T]) ->
	IsTuple = is_tuple(H),
	if
		IsTuple ->
			{Name,Value} = H,
			if
				(Name == Param) ->
					Value;
				true -> 
					getParameter(Param,T)
			end;
		true -> 
			getParameter(Param,T)
    end;
getParameter(_, []) ->	false.
