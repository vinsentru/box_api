-module('box_api').

%% API exports
%%-export([]).
-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================

%% Upload file
upload(LFname,RFname,Client) ->
	upload(LFname,RFname,<<"root">>,Client).

upload(LFname,RFname,Parent,Client) when is_list(LFname) ->
	case file:read_file(LFname) of
		{ok,LBin} -> upload(LBin,RFname,Parent,Client);
		{error, Reason}	-> {error,0,Reason}
	end;	

upload(Body,RFname,Parent,Client) when is_binary(Body) ->
	Uri = "https://www.googleapis.com/upload/drive/v2/files",
	Boundary = "delimiter",
	Params =  [{"uploadType","multipart"}],
	Url = restc:construct_url(Uri,Params),
	MetaData =  [
				{<<"title">>,RFname}
				,{<<"parents">>,[[{<<"id">>,Parent}]]}
				,{<<"mimeType">>,<<"audio/wav">>}
				],
	MultipartBody = iolist_to_binary(multipart_body(
			[
			{"application/json; charset=\"utf-8\"",jsx:encode(MetaData)}
			,{"application/octet-stream",Body,data}
			],Boundary)),
	Headers = [{"Content-Length",integer_to_list(size(MultipartBody))}],
	Type = "multipart/related; boundary=\"" ++ Boundary ++ "\"",
	case oauth2c:request(post, Type, list_to_binary(Url), [200], Headers, MultipartBody, Client) of
		{{ok,200,RHeaders,Replay},Client2} -> 
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

make_dir(Folder,Parent,Client) when is_binary(Folder)->
	Uri = <<"https://www.googleapis.com/drive/v2/files">>,
	%% Pass parameters as request's body for POST
	Params =  [
				{<<"mimeType">>,<<"application/vnd.google-apps.folder">>}
				,{<<"title">>,Folder}
				,{<<"parents">>,[[{<<"id">>,Parent}]]}
				],
	case oauth2c:request(post, json, Uri, [200],[],Params,Client) of
		{{ok,200,RHeaders,Replay},Client2} -> {ok,Replay};
		{{error,Err,_,Replay},_} -> {error,Err,Replay};
		{error,Err,Replay} -> {error,Err,Replay};
		Req -> Req
	end.

% Account info

account_info(Client) ->
	Uri = <<"https://www.googleapis.com/drive/v2/about">>,
	case oauth2c:request(get, Uri, [200], Client) of
		{{ok,200,RHeaders,Replay},Client2} -> {ok,Replay};
		{{error,Err,_,Replay},_} -> {error,Err,Replay}
	end.

% Get list

list(Query,Client) when is_list(Query)->
	Uri = "https://www.googleapis.com/drive/v2/files",
	Params =  [{"q",Query},{"spaces","drive"}],
	Url = restc:construct_url(Uri,Params),
	case oauth2c:request(get,list_to_binary(Url), [200], Client) of
		{{ok,200,RHeaders,Replay},Client2} -> {ok,Replay};
		{{error,Err,_,Replay},_} -> {error,Err,Replay};
		{error,Err,Replay} -> {error,Err,Replay};
		Req -> Req
	end.


refresh_token(ClientId,ClientSecret,RefreshToken,Client) ->
	Uri = <<"https://www.googleapis.com/oauth2/v3/token">>,
	Params = [
			{"client_id",ClientId}
			,{"client_secret",ClientSecret}
			,{"grant_type","refresh_token"}
			,{"refresh_token",RefreshToken}
			],
	case oauth2c:request(post,percent,Uri,[200],[],Params,Client) of
		{{ok,200,RHeaders,Replay},Client2} -> {ok,Replay};
		{{error,Err,_,Replay},_} -> {error,Err,Replay};
		{error,Err,Replay} -> {error,Err,Replay}
	end.


%%====================================================================
%% Internal functions
%%====================================================================


multipart_body([], Boundary) ->
    ["--", Boundary, "--\r\n","\r\n"];
multipart_body([{Type,Body} | BodyList], Boundary) ->
    ["--", Boundary, "\r\n",
     "Content-Type: ", Type, "\r\n",
     "MIME-Version: 1.0","\r\n",
     "\r\n",
     Body,"\r\n"
     | multipart_body(BodyList, Boundary)];
multipart_body([{Type,Body,data} | BodyList], Boundary) ->
    ["--", Boundary, "\r\n",
     "Content-Type: ", Type, "\r\n",
     "MIME-Version: 1.0","\r\n",
     "Content-Transfer-Encoding: binary","\r\n",
     "\r\n",
     Body,"\r\n"
     | multipart_body(BodyList, Boundary)].





