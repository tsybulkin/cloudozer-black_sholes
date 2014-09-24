%
% implicit volatility
%

-module(iv_measure).
-export([test/0]).

-define(VOL_GUESS_MIN, 0.05).
-define(VOL_GUESS_MAX, 2.5).
-define(MAX_ITER, 100).
-define(VOL_TOL, 0.001).

-define(DVOL, 0.01).
-define(LOOPS, 100).



test() ->
	Vols = [ 0.05+J*?DVOL || J <- lists:seq(0,80) ],
	Opts = [ Opt || {Opt,_,_,_,_,_,_,_} <- [ black_sholes:test_bs(V) || V <- Vols ] ],
	io:format("Options: ~p~n",[Opts]),
	
	statistics(runtime),

	Results = loop(?LOOPS,Opts),
	
	{_,T} = statistics(runtime),
	io:format("Implicit vol:~p~n",[Results]),
	io:format("Test took ~p msec~n",[T/?LOOPS]).


loop(N,Opts) ->
	Res = lists:foldl(fun(Opt, Acc) ->
					IV = iv:iv(Opt),
					%io:format("Opt:~p, IV:~p~n",[Opt,IV]),

					[ IV|Acc]
				end, [], Opts),
	
	case N > 0 of
		true -> loop(N-1,Opts);
		_ -> Res
	end.

