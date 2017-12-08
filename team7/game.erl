-module(game).

-behaviour(gen_server).

-export([game/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {word::[{char(), boolean()}]}).

game(Name, Word) ->
    gen_server:start({global, Name}, ?MODULE, Word, []).

init(Word) ->
    ProcessedWord = lists:map(fun(Char) -> {Char, false} end, Word),
    {ok, #state{word = ProcessedWord}}.

handle_call({guess, Letter}, _From, State = #state{word = Word}) ->
    Result = lists:keyfind(Letter, 1, Word),

    case Result of
        false ->
            {reply, {guessedwrong, formatWord(Word)}, State};
        {Letter, true} ->
            {reply, {alreadyguessed, formatWord(Word)}, State};
        {Letter, false} ->
            NewWord = lists:map(
                        fun
                            ({Char, false}) when (Char =:= Letter) -> {Char, true};
                            (Anything) -> Anything
                        end, Word),

            GameEnd = lists:all(fun
                                    ({_Anything, Boolean}) -> Boolean
                                end, NewWord),

            case GameEnd of
                true ->
                    {stop, normal, {youwin, formatWord(NewWord)}, State#state{word = NewWord}};
                false ->
                    {reply, {guessedright, formatWord(NewWord)}, State#state{word = NewWord}}
            end
    end;
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

%% Internal functions
formatWord(Word) ->
    Encrypted = lists:map(
                  fun
                      ({Letter, true})   -> Letter;
                      ({_Letter, false}) -> $_
                  end, Word),
    Space = $\s,
    lists:join(Space, Encrypted).
