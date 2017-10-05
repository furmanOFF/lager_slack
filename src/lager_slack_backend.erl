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
    uri :: string(),
    sign=undefined :: string()
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
    config(Config, #state{
        level = lager_util:level_to_num(critical)
    }).

handle_call(get_loglevel, S=#state{level=Level}) ->
    {ok, Level, S};
handle_call({set_loglevel, Level}, S) ->
    try lager_util:config_to_mask(Level) of
        Mask -> 
            {ok, ok, S#state{level=Mask}}
    catch
        _:_ -> 
            {ok, {error, bad_log_level}, S}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, Message}, S=#state{level=Level, uri=Uri, sign=Sign}) ->
    case lager_util:is_loggable(Message, Level, ?MODULE) of
        true ->
            Msg = list_to_binary(lager_msg:message(Message)),
            Severity = lager_msg:severity(Message),
            {Mega, Sec, _Micro} = lager_msg:timestamp(Message),
            Json = #{
                attachments => [#{
                    fallback => Msg,
                    text => Msg,
                    title => Severity,
                    color => color(Severity),
                    footer => case Sign of undefined -> null; _ -> Sign end,
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
config([{uri, Uri} | T], S) when is_list(Uri) ->
    config(T, S#state{uri = Uri});
config([{uri, Uri} | T], S) when is_binary(Uri) ->
    config(T, S#state{uri = binary_to_list(Uri)});
config([{uri, _} | _], _) ->
    {error, bad_uri};
config([{level, Level} | T], S) ->
    try lager_util:config_to_mask(Level) of
        Mask -> 
            config(T, S#state{level=Mask})
    catch
        _:_ -> 
            {error, bad_log_level}
    end;
config([{sign, Sign} | T], S) when is_list(Sign) ->
    config(T, S#state{sign = list_to_binary(Sign)});
config([{sign, Sign} | T], S) when is_binary(Sign); is_atom(Sign) ->
    config(T, S#state{sign = Sign});
config([], State) ->
    State;
config(_, _) ->
    {error, bad_config}.

color(debug) -> null;
color(info) -> <<"good">>;
color(notice) -> <<"good">>;
color(warning) -> <<"warning">>;
color(error) -> <<"danger">>;
color(critical) -> <<"danger">>;
color(alert) -> <<"danger">>;
color(emergency) -> <<"danger">>.
