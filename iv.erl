%
% implicit volatility
%

-module(iv).
-export([test/0]).

-define(VOL_GUESS_MIN, 0.05).
-define(VOL_GUESS_MAX, 2.5).
-define(MAX_ITER, 100).
-define(VOL_TOL, 0.001).

x() -> {0.5,100.0, 1.0, 1.0, 0.01, 0, 0, option_call,100.0}.

test() ->
	{Vol,Spot,Cttx,Vttx,Box,Borrow,Yield,CallPut,Strike} = x(),
	io:format("Correct volatility: ~p~n",[Vol]),
	{Opt,_,_,_,_,_,_,_} = black_sholes:blash(Vol,Spot,Cttx,Vttx,Box,Borrow,Yield,CallPut,Strike),
	IV = implicit_vol(Spot,Cttx,Vttx,Box,Borrow,Yield,CallPut,Strike,Opt),
	io:format("Implicit volatility: ~p~n",[IV]).


implicit_vol(Spot,Cttx,Vttx,Box,Borrow,Yield,CallPut,Strike,Option) ->
	Func = fun(V)->
		{Opt,_,_,_,_,_,_,_} = black_sholes:blash(V,Spot,Cttx,Vttx,Box,Borrow,Yield,CallPut,Strike),
		Opt - Option
	end,

	A = ?VOL_GUESS_MIN,
	B = ?VOL_GUESS_MAX,
	Fa = Func(A),
	Fb = Func(B),

	case {Fa < 0, Fb > 0} of
		{true, true} -> % start find volatility
			secant(1,A,B,Func);
		{false,false}-> % swap and start 
			secant(1,B,A,Func);
		_ -> no_roots
	end.

	
bisect(?MAX_ITER,A,B,_) -> (A+B)*0.5;
bisect(Iter, A,B, Func) ->
	C = (A+B)*0.5,
	Fc = Func(C),
	if
		Fc > ?VOL_TOL ->
			bisect(Iter+1, A, C, Func);
		Fc < -?VOL_TOL ->
			bisect(Iter+1, C, B, Func);
		true ->
			io:format("Found in ~p iterations~n",[Iter]),
			C  
	end.

secant(?MAX_ITER,A,B,Func) -> 
	Fa = Func(A),
	Fb = Func(B),
	(A*Fb-B*Fa)/(Fb-Fa);

secant(Iter, A,B, Func) ->
	Fa = Func(A),
	Fb = Func(B),
	C = (A*Fb-B*Fa)/(Fb-Fa),
	Fc = Func(C),
	if
		Fc > ?VOL_TOL ->
			secant(Iter+1, A, C, Func);
		Fc < -?VOL_TOL ->
			secant(Iter+1, C, B, Func);
		true ->
			io:format("Found in ~p iterations~n",[Iter]),
			C  
	end.




