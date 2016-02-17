-module('box_api').

%% API exports
%%-export([]).
-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================

%% Upload file
upload(LFname,RFname,Client) ->
	upload(LFname,RFname,<<"0">>,Client).

upload(LFname,RFname,Parent,Client) when is_list(LFname) ->
	case file:read_file(LFname) of
		{ok,LBin} -> upload(LBin,RFname,Parent,Client);
		{error, Reason}	-> {error,0,Reason}
	end;	

upload(Body,RFname,Parent,Client) when is_binary(Body)
									  ,is_binary(RFname)
									  ,is_binary(Parent) ->
	Uri = <<"https://upload.box.com/api/2.0/files/content">>,
	Boundary = "delimiter",
	MetaData =  [
					{<<"name">>,RFname}
					,{<<"parent">>,[{<<"id">>,Parent}]}	
				],
	MultipartBody = iolist_to_binary(multipart_body(
			[
			{"application/json; charset=\"utf-8\"",jsx:encode(MetaData)}
			,{"application/octet-stream",RFname,Body,data}
			],Boundary)),
	io:format("MultipartBody:~n~p~n",[MultipartBody]),
	Headers = [{"Content-Length",integer_to_list(size(MultipartBody))}],
	Type = "multipart/form-data; boundary=\"" ++ Boundary ++ "\"",
	case oauth2c:request(post, Type, Uri, [201], Headers, MultipartBody, Client) of
		{{ok,201,RHeaders,Replay},Client2} -> 
			% io:format("Headers: ~p~n",[RHeaders]),
			{ok,Replay};
		{{error,Err,_,Replay},_} -> {error,Err,Replay};
		{error,Err,Replay} -> {error,Err,Replay};
		Req -> Req
	end.

%% Make dir

%% Make dir low level functions
make_dir(Folder,Parent,Client) when is_list(Folder) ->
	make_dir(list_to_binary(Folder),Parent,Client);	

make_dir(Folder,Parent,Client) when is_binary(Folder),
									is_binary(Parent) ->
	Uri = <<"https://api.box.com/2.0/folders">>,
	%% Pass parameters as request's body for POST
	Params =  [
				% {<<"mimeType">>,<<"application/vnd.google-apps.folder">>}
				{<<"name">>,Folder}
				,{<<"parent">>,[{<<"id">>,Parent}]}
				],
	case oauth2c:request(post, json, Uri, [201],[],Params,Client) of
		{{ok,201,RHeaders,Replay},Client2} -> {ok,Replay};
		{{error,Err,_,Replay},_} -> {error,Err,Replay};
		{error,Err,Replay} -> {error,Err,Replay};
		Req -> Req
	end.

% Account info

% account_info(Client) ->
% 	Uri = <<"https://www.googleapis.com/drive/v2/about">>,
% 	case oauth2c:request(get, Uri, [200], Client) of
% 		{{ok,200,RHeaders,Replay},Client2} -> {ok,Replay};
% 		{{error,Err,_,Replay},_} -> {error,Err,Replay}
% 	end.

% % Get list

% list(Query,Client) when is_list(Query)->
% 	Uri = "https://www.googleapis.com/drive/v2/files",
% 	Params =  [{"q",Query},{"spaces","drive"}],
% 	Url = restc:construct_url(Uri,Params),
% 	case oauth2c:request(get,list_to_binary(Url), [200], Client) of
% 		{{ok,200,RHeaders,Replay},Client2} -> {ok,Replay};
% 		{{error,Err,_,Replay},_} -> {error,Err,Replay};
% 		{error,Err,Replay} -> {error,Err,Replay};
% 		Req -> Req
% 	end.


% refresh_token(ClientId,ClientSecret,RefreshToken,Client) ->
% 	Uri = <<"https://www.googleapis.com/oauth2/v3/token">>,
% 	Params = [
% 			{"client_id",ClientId}
% 			,{"client_secret",ClientSecret}
% 			,{"grant_type","refresh_token"}
% 			,{"refresh_token",RefreshToken}
% 			],
% 	case oauth2c:request(post,percent,Uri,[200],[],Params,Client) of
% 		{{ok,200,RHeaders,Replay},Client2} -> {ok,Replay};
% 		{{error,Err,_,Replay},_} -> {error,Err,Replay};
% 		{error,Err,Replay} -> {error,Err,Replay}
% 	end.


%%====================================================================
%% Internal functions
%%====================================================================


multipart_body([], Boundary) ->
    ["--", Boundary, "--\r\n","\r\n"];
multipart_body([{Type,Body} | BodyList], Boundary) ->
    ["--", Boundary, "\r\n",
    "Content-Disposition: form-data; name=\"attributes\"","\r\n",
     % "Content-Type: ", Type, "\r\n",
     % "MIME-Version: 1.0","\r\n",
     "\r\n",
     Body,"\r\n"
     | multipart_body(BodyList, Boundary)];
multipart_body([{Type,RFname,Body,data} | BodyList], Boundary) ->
    ["--", Boundary, "\r\n",
     "Content-Disposition: form-data; name=\"file\"; filename=\"",RFname,"\"\r\n",
     "Content-Type: ", Type, "\r\n",
     "MIME-Version: 1.0","\r\n",
     "Content-Transfer-Encoding: binary","\r\n",
     "\r\n",
     Body,"\r\n"
     | multipart_body(BodyList, Boundary)].





