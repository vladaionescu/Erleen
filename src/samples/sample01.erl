
-module(sample01).

-behaviour(een_comp).



reinit(_OldModule, _OldState, _Params) ->
    {ok, #een_comp_params{allowed_in = [{call, some_call, 0}],
                          allowed_out = [{cast, some_cast, 0}]}, nostate}.

handle_call({some_call, {}, _From}, State) ->
    {reply, ok, State}.

handle_cast({some_cast, {}}, State) ->
    {noreply, State}.
