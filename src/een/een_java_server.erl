
-module(een_java_server).

-behaviour(gen_server).

-export([start/0, register_java_node/1, get_java_pid/1, wait_connection/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {java_nodes = orddict:new(),
                waits = orddict:new()}).

%% ----------------------------------------------------------------------------
%% Internal interface
%% ----------------------------------------------------------------------------

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

register_java_node(JavaPid) ->
    gen_server:call(?MODULE, {register_java_node, JavaPid}, infinity).

get_java_pid(JavaNode) ->
    gen_server:call(?MODULE, {get_java_pid, JavaNode}, infinity).

wait_connection(JavaNode) ->
    gen_server:call(?MODULE, {wait_connection, JavaNode}, infinity).

%% ----------------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({register_java_node, Pid}, _From,
            State = #state{java_nodes = JN, waits = Waits}) ->
    erlang:monitor_node(node(Pid), true),
    io:format("~p on ~p: Java node ~p:~p connected.~n",
              [?MODULE, node(), node(Pid), Pid]),
    State1 =
        case orddict:find(node(Pid), Waits) of
            {ok, WaitFrom} -> gen_server:reply(WaitFrom, ok),
                              State#state{waits = orddict:erase(node(Pid), Waits)};
            error          -> State
        end,
    {reply, ok, State1#state{java_nodes = orddict:store(node(Pid), Pid, JN)}};
handle_call({get_java_pid, Node}, _From, State = #state{java_nodes = JN}) ->
    {reply, orddict:find(Node, JN), State};
handle_call({wait_connection, Node}, From, State = #state{waits = Waits,
                                                          java_nodes = JN}) ->
    case orddict:find(Node, JN) of
        {ok, _} -> {reply, ok, State};
        error   -> NewWaits = orddict:store(Node, From, Waits),
                   {noreply, State#state{waits = NewWaits}}
    end.

handle_cast(_, _) ->
    unexpected.

handle_info({nodedown, Node}, State = #state{java_nodes = JN}) ->
    {noreply, State#state{java_nodes = orddict:erase(Node, JN)}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
