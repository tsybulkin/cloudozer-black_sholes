%
% Black Sholes formula
%

-module(black_sholes).
-export([blash/9,test_bs/1,measuring/1]).

-define(INV_SQRTPI, 1/math:sqrt(math:pi())).



test_bs(V) -> blash(V,100.0, 1.0, 1.0, 0.01, 0, 0, option_call,100.0).

measuring(N)->
    statistics(runtime),
    test_bs(0.5),
    measuring(N-1,N).

measuring(0,N) ->
    {_,T} = statistics(runtime),
    io:format("~w runs of Black-Sholes algo took: ~p sec~n",[N, T/1000]);
measuring(K,N) -> 
    test_bs(0.5),
    measuring(K-1,N).  


blash(Vol,_,_,_,_,_,_,_,Strike) when Vol=<0; Strike=<0 -> bad_input; 

blash(Vol,Spot,Cttx,Vttx,Box,Borrow,Yield,CallPut,Strike) ->
	LogSK = math:log(Spot / Strike),
    Roll = Box - Borrow,
    SqrtVttx = math:sqrt(Vttx),
    St = Vol * SqrtVttx,
    Bor_div_disc = math:exp((- Borrow - Yield) * Cttx),
    Box_disc = math:exp( -Box * Cttx),

    D1 = ((LogSK + (Roll - Yield) * Cttx) / St) + (0.5 * St),
    D2 = D1 - St,
    %io:format("D1:~p, D2:~p~n",[D1,D2]),
    Nd1 = ?INV_SQRTPI * math:exp(-0.5 * D1 * D1),
    %io:format("nd1:~p~n",[Nd1]),

    get_greeks(CallPut,D1,D2,Spot,Bor_div_disc,Box_disc,Nd1,St,SqrtVttx,Cttx,Strike,Vol).

get_greeks(option_call,D1,D2,Spot,Bor_div_disc,Box_disc,Nd1,St,SqrtVttx,Cttx,Strike,Vol) ->
	N1 = gaussian:gaussianCDF(D1),
    N2 = gaussian:gaussianCDF(D2),
    %io:format("N1:~p, N2:~p~n~n",[N1,N2]),
    TV = Spot * Bor_div_disc * N1 - Strike * Box_disc * N2,
    Delta = Bor_div_disc * N1,
    Gamma = Nd1 * Bor_div_disc / (Spot * St),
    Vega = Spot * Bor_div_disc * Nd1 * SqrtVttx,
    BoxRho = Cttx * Strike * Box_disc * N2,
    BorrowRho = - Cttx * Spot * Delta,
    Vegavega = Vega * D1 * D2 / Vol,
    Deltavega = - Bor_div_disc * D2 * Nd1 / Vol,
    %io:format("TV:~p~nDelta:~p~nGamma:~p~nVega:~p~nBoxRho:~p~nBorrowRho:~p~nVegaVega:~p~nDeltaVega:~p~n",
    %	[TV,Delta,Gamma,Vega,BoxRho,BorrowRho,Vegavega,Deltavega]),
    {TV,Delta,Gamma,Vega,BoxRho,BorrowRho,Vegavega,Deltavega};

get_greeks(option_put,D1,D2,Spot,Bor_div_disc,Box_disc,Nd1,St,SqrtVttx,Cttx,Strike,Vol) ->
	N1 = gaussian:gaussianCDF(-D1),
    N2 = gaussian:gaussianCDF(-D2),
    %io:format("N1:~p, N2:~p~n~n",[N1,N2]),
    TV = - Spot * Bor_div_disc * N1 + Strike * Box_disc * N2,
    Delta = - Bor_div_disc * N1,
    Gamma = Nd1 * Bor_div_disc / (Spot * St),
    Vega = Spot * Bor_div_disc * Nd1 * SqrtVttx,
    BoxRho = - Cttx * Strike * Box_disc * N2,
    BorrowRho = - Cttx * Spot * Delta,
    Vegavega = Vega * D1 * D2 / Vol,
    Deltavega = - Bor_div_disc * D2 * Nd1 / Vol,
    %io:format("TV:~p~nDelta:~p~nGamma:~p~nVega:~p~nBoxRho:~p~nBorrowRho:~p~nVegaVega:~p~nDeltaVega:~p~n",
    %	[TV,Delta,Gamma,Vega,BoxRho,BorrowRho,Vegavega,Deltavega]),
    {TV,Delta,Gamma,Vega,BoxRho,BorrowRho,Vegavega,Deltavega};

get_greeks(_,_,_,_,_,_,_,_,_,_,_,_) -> bad_input.


	