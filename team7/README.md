## Distributed Hangman using Erlang/OTP

`game.erl` implements the game logic using `gen_server`, accepting `{guess, <char>}`, replying with the progress of the word, until the whole word is
completed. `game_server.erl` is responsible for launching various games, returning a registered name so that players could interact with that game.

### Demo

Running server part on trygger:

```
username@trygger:~/erlang$ erlc game*.erl ; erl -setcookie magic -name server
Erlang/OTP 20 [erts-9.1] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:10] [kernel-poll:false]

Eshell V9.1  (abort with ^G)
(server@trygger.it.uu.se)1> game_server:start_link().
{ok,<0.71.0>}
(server@trygger.it.uu.se)2> gen_server:call({global, game_server}, start_game).
{ok,game_0}
(server@trygger.it.uu.se)3>
```

Running player part on hedenius:

```
username@hedenius:~/erlang$ erlc game*.erl ; erl -setcookie magic -name player -eval "net_adm:ping('server@trygger.it.uu.se')"
Erlang/OTP 20 [erts-9.1] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.1  (abort with ^G)
(player@hedenius.it.uu.se)1> nodes().
['server@trygger.it.uu.se']
(player@hedenius.it.uu.se)2> gen_server:call({global, game_0}, {guess, $a}).
{guessedright,"a _ a _ _ _ _ _ a _ _"}
(player@hedenius.it.uu.se)3> gen_server:call({global, game_0}, {guess, $d}).
{guessedright,"a d a _ _ _ _ _ a _ _"}
(player@hedenius.it.uu.se)4> gen_server:call({global, game_0}, {guess, $l}).
{guessedright,"a d a l _ _ _ l a _ _"}
(player@hedenius.it.uu.se)5> gen_server:call({global, game_0}, {guess, $o}).
{guessedright,"a d a l o _ _ l a _ _"}
(player@hedenius.it.uu.se)6> gen_server:call({global, game_0}, {guess, $v}).
{guessedright,"a d a l o v _ l a _ _"}
(player@hedenius.it.uu.se)7> gen_server:call({global, game_0}, {guess, $e}).
{guessedright,"a d a l o v e l a _ e"}
(player@hedenius.it.uu.se)8> gen_server:call({global, game_0}, {guess, $c}).
{youwin,"a d a l o v e l a c e"}
(player@hedenius.it.uu.se)9>
```
