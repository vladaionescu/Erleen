
-module(sample01).

-behaviour(een_comp).

-include("erleen.hrl").



reinit(_OldModule, _OldState, _Args) ->
    {ok, #een_comp_params{allowed_in = [{call, some_call, 0}],
                          allowed_out = [{cast, some_cast, 0}]}, nostate}.

handle({call, some_call, {}, _From}, State) ->
    {reply, ok, State};
handle({some_cast, {}}, State) ->
    {noreply, State}.
