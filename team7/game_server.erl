-module(game_server).

-behaviour(gen_server).

-export([start_link/0, game_count/0]).

-define(GAME_MODULE, game).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {words, games = []}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

games() ->
    ["adalovelace", "hello", "world"].
init([]) ->
    {ok, #state{words = games(), games = games()}}.

handle_call(start_game, _From, State = #state{games = []}) ->
    {reply, empty, State};
handle_call(start_game, _From, State = #state{games = [G|Gs]}) ->
    Id = length(State#state.words) - length(State#state.games),
    Name = game_name(Id),
    game:game(Name, G),
    {reply, {ok, Name}, State#state{games = Gs}};
handle_call(game_left, _From, State) ->
    {reply, length(State#state.games), State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
game_name(Id) ->
    list_to_atom(lists:concat(["game_", Id])).
game_count() ->
    gen_server:call(?MODULE, game_left).
