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
    sender :: pid(),
    sign=undefined :: string()
}).

-define(DEFAULT_CONFIG, #{
    level => critical,
    timeout => 5000,
    threshold => 20
}).

%% lager
config_to_id(Config) ->
    case proplists:get_value(uri, Config) of
        Uri when is_list(Uri) ->
            {?MODULE, list_to_binary(Uri)};
        Uri when is_binary(Uri) ->
            {?MODULE, Uri};
        Uri ->
            erlang:error({bad_uri, Uri})
    end.

%% gen_event
init(Config) ->
    case check_config(Config, ?DEFAULT_CONFIG) of
        {error, Reason} -> 
            {error, {fatal, Reason}};
        Map=#{uri:=Uri, level:=Level, timeout:=Timeout, threshold:=Thres} ->
            {ok, Pid} = lager_slack_sender:start_link(Uri, Timeout, Thres),
            {ok, #state{
                sender = Pid,
                level = Level,
                sign = maps:get(sign, Map, undefined)
            }}
    end.

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

handle_event({log, Message}, S=#state{sender=Pid, level=Level, sign=Sign}) ->
    case lager_util:is_loggable(Message, Level, ?MODULE) of
        true ->
            Msg = list_to_binary(lager_msg:message(Message)),
            Severity = lager_msg:severity(Message),
            {Mega, Sec, _Micro} = lager_msg:timestamp(Message),
            Json = #{
                fallback => Msg,
                text => Msg,
                title => Severity,
                color => color(Severity),
                footer => case Sign of undefined -> null; _ -> Sign end,
                ts => Mega * 1000000 + Sec
            },
            lager_slack_sender:send(Pid, Json),
            {ok, S};
        false ->
            {ok, S}
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_info({'EXIT', Pid, Reason}, #state{sender=Pid}) ->
    erlang:exit({sender_down, Reason});
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, #state{sender=Pid}) ->
    unlink(Pid),
    lager_slack_sender:stop(Pid).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%
check_config([{uri, Uri} | T], Map) when is_list(Uri) ->
    check_config(T, Map#{uri => Uri});
check_config([{uri, Uri} | T], Map) when is_binary(Uri) ->
    check_config(T, Map#{uri => binary_to_list(Uri)});
check_config([{uri, _} | _], _) ->
    {error, bad_uri};
check_config([{level, Level} | T], Map) ->
    try lager_util:config_to_mask(Level) of
        Mask -> 
            check_config(T, Map#{level => Mask})
    catch
        _:_ -> 
            {error, bad_log_level}
    end;
check_config([{sign, Sign} | T], Map) when is_list(Sign) ->
    check_config(T, Map#{sign => list_to_binary(Sign)});
check_config([{sign, Sign} | T], Map) when is_binary(Sign); is_atom(Sign) ->
    check_config(T, Map#{sign => Sign});
check_config([], Map=#{uri := _}) ->
    Map;
check_config(_, _) ->
    {error, bad_config}.

color(debug) -> null;
color(info) -> <<"good">>;
color(notice) -> <<"good">>;
color(warning) -> <<"warning">>;
color(error) -> <<"danger">>;
color(critical) -> <<"danger">>;
color(alert) -> <<"danger">>;
color(emergency) -> <<"danger">>.
