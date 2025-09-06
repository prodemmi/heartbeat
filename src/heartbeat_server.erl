-module(heartbeat_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/heal/:user_id", heal_handler, []},
                                 {"/checkup/:user_id", checkup_handler, []},
                                 {"/heart_rhythm/:user_id", heart_rhythm_handler, []}]}]),
    {ok, _} =
        cowboy:start_clear(http_listener, [{port, 3519}], #{env => #{dispatch => Dispatch}}),
    {ok, #{}}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.
