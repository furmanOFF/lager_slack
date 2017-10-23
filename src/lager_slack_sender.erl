-module(lager_slack_sender).

-behaviour(gen_server).

%% API
-export([start_link/3, stop/1, send/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%
-record(state, {
    uri :: string(),
    timeout :: integer(),
    threshold :: integer(),
    buffer = [] :: [jsx:json_term()],
    count=0 :: integer(),
    timer :: reference()
}).

%% API
start_link(Uri, Timeout, Threshold) when is_integer(Timeout), is_integer(Threshold) ->
    gen_server:start_link(?MODULE, {Uri, Timeout, Threshold}, []).

stop(Pid) ->
    gen_server:stop(Pid).

send(Pid, Msg) ->
    gen_server:cast(Pid, {send, Msg}).

%% gen_server
init({Uri, Timeout, Threshold}) ->
    {ok, #state{
        uri = Uri,
        timeout = Timeout,
        threshold = Threshold
    }}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({send, Msg}, S=#state{buffer=Buff, count=Cnt}) ->
    {noreply, handle_msg(S#state{
        buffer = [Msg | Buff],
        count = Cnt + 1
    })};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, S) ->
    {noreply, send(S#state{timer=undefined})};
handle_info({http, {_, {{_, 200, _Msg}, _, _}}}, State) ->
    {noreply, State};
handle_info({http, {_, {{_, Code, Msg}, _, _}}}, State) ->
    {stop, {request_error, {Code, Msg}}, State};
handle_info({http, {_, {error, Reason}}}, State) ->
    {stop, {error, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
handle_msg(S=#state{count=Cnt, timeout=Timeout, timer=undefined}) when Cnt > 0 ->
    S#state{
        timer = erlang:send_after(Timeout, self(), timeout)
    };
handle_msg(S=#state{count=Cnt, threshold=Thres, timer=Ref}) when Cnt >= Thres ->
    send(S#state{
        timer = erlang:cancel_timer(Ref, [{async, true}, {info, false}])
    });
handle_msg(State) -> State.

send(S=#state{count = 0}) -> S;
send(S=#state{uri=Uri, buffer=Buffer}) ->
    Json = jsx:encode(#{attachments => lists:reverse(Buffer)}),
    {ok, _Req} = httpc:request(post, {Uri, [], "application/json", Json}, [], [{sync, false}]),
    S#state{buffer = [], count = 0}.
