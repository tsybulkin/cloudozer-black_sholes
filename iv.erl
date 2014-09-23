%
% implicit volatility
%

-module(iv).
-export([implicit_vol/1]).

-define(VOL_GUESS_MIN, 0.05).
-define(VOL_GUESS_MAX, 2.5).
-define(MAX_ITER, 100).
-define(VOL_TOL, 0.001).

x() -> {100.0, 1.0, 1.0, 0.01, 0, 0, option_call,100.0}.

tv(Vol) ->
	{Spot,Cttx,Vttx,Box,Borrow,Yield,CallPut,Strike} = x(),
	{TV,_,_,_,_,_,_,_} = black_sholes:blash(Vol,Spot,Cttx,Vttx,Box,Borrow,Yield,CallPut,Strike), 
	TV.

f(Vol,True_Vol) ->
	tv(Vol) - tv(True_Vol).

implicit_vol(Vol) ->
	{Spot,Cttx,Vttx,Box,Borrow,Yield,CallPut,Strike} = x(),
	TV = tv(0.5),
	io:format("Volatility:~p~n",[Vol]),

	A = ?VOL_GUESS_MIN,
	B = ?VOL_GUESS_MAX,
	case f(A,TV)*f(B,TV) > 0 of
		true -> no_roots;
		_ -> implicit_vol(1, A,B,fun(V)-> f(V,Vol) end)

	end.

	
implicit_vol(?MAX_ITER,A,B,_) -> (A+B)*0.5;
implicit_vol(Iter, A,B, Func) ->
	Fa = Func(A),
	C = (A+B)*0.5,
	Fc = Func(C),
	Fac = Fa*Fc,

	if
		Fac > ?VOL_TOL ->
			implicit_vol(Iter+1, B, C, Func);
		Fac < -?VOL_TOL ->
			implicit_vol(Iter+1, A, C, Func);
		true ->
			C  
	end.
	




