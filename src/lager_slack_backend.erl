-module(lager_slack_backend).

-behaviour(gen_event).

%% lager
-export([config_to_id/1]).

%% gen_event
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
        code_change/3]).

%%
-record(state, {
    level :: term(),
    uri :: string()
}).

%% lager
config_to_id(Config) ->
    case proplists:get_value(uri, Config) of
        undefined ->
            erlang:error(bad_uri);
        Uri ->
            {?MODULE, Uri}
    end.

%% gen_event
init(Config) ->
    Level = proplists:get_value(level, Config, critical),
    try {proplists:get_value(uri, Config), lager_util:config_to_mask(Level)} of
        {Uri, Levels} when is_list(Uri) -> 
            {ok, #state{uri=Uri, level=Levels}};
        {Uri, Levels} when is_binary(Uri) -> 
            {ok, #state{uri=binary_to_list(Uri), level=Levels}};
        _ -> 
            {error, {fatal, bad_uri}}
    catch
        _:_ -> 
            {error, {fatal, bad_log_level}}
    end.

handle_call(get_loglevel, S=#state{level=Level}) ->
    {ok, Level, S};
handle_call({set_loglevel, Level}, S) ->
    try lager_util:config_to_mask(Level) of
        Levels -> 
            {ok, ok, S#state{level=Levels}}
    catch
        _:_ -> 
            {ok, {error, bad_log_level}, S}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, Message}, S=#state{level=Level, uri=Uri}) ->
    case lager_util:is_loggable(Message, Level, ?MODULE) of
        true ->
            Msg = list_to_binary(lager_msg:message(Message)),
            {Mega, Sec, _Micro} = lager_msg:timestamp(Message),
            Json = #{
                attachments => [#{
                    fallback => Msg,
                    text => Msg,
                    title => lager_msg:severity(Message),
                    color => color(lager_msg:severity(Message)),
                    ts => Mega * 1000000 + Sec
                }]
            },
            {ok, _Req} = httpc:request(post, {Uri, [], "application/json", jsx:encode(Json)}, [], [
                {sync, false},
                {receiver, fun(Response) -> io:format("response: ~p", [Response]) end} % ignore response
            ]),
            {ok, S};
        false ->
            {ok, S}
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%
color(debug) -> <<"#DFF2BF">>;
color(info) -> <<"#BDE5F8">>;
color(notice) -> <<"#00529B">>;
color(warning) -> <<"#FEEFB3">>;
color(error) -> <<"#FFBABA">>;
color(critical) -> <<"#D8000C">>;
color(alert) -> <<"#D8000C">>;
color(emergency) -> <<"#D8000C">>.
