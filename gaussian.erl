%
% Gaussian CDF
%

-module(gaussian).
-export([gaussianCDF/1]).

-define( a1, 0.398942280444E+00).
-define( a2, 0.399903438504E+00).
-define( a3, 5.75885480458E+00).
-define( a4, 29.8213557808E+00).
-define( a5, 2.62433121679E+00).
-define( a6, 48.6959930692E+00).
-define( a7, 5.92885724438E+00).
-define( b0, 0.398942280385E+00).
-define( b1, 3.8052E-08).
-define( b2, 1.00000615302E+00).
-define( b3, 3.98064794E-04).
-define( b4, 1.98615381364E+00).
-define( b5, 0.151679116635E+00).
-define( b6, 5.29330324926E+00).
-define( b7, 4.8385912808E+00).
-define( b8, 15.1508972451E+00).
-define( b9, 0.742380924027E+00).
-define( b10, 30.789933034E+00).
-define( b11, 3.99019417011E+00).

gaussianCDF(X) ->
	ABSx = abs(X),
	if
		%ABSx < 0.000000001 -> 0.5; % no need for calc: x=0 gives cdf=0.5
		ABSx < 0.01 -> 0.5 - ?a1 * X; % alternative implementation

		ABSx =< 1.28 ->
			Y = X*X/2,
			0.5 + X * ( ?a1 - ?a2 * Y / ( Y + ?a3 
            - ?a4 / ( Y + ?a5
            + ?a6 / ( Y + ?a7 ) ) ) );

        ABSx =< 2.7, X<0 ->   %  1.28 < |X| <= 12.7
        	Y = X*X/2,
        	math:exp ( - Y )
            * ?b0  / ( ABSx - ?b1
            + ?b2  / ( ABSx + ?b3
            + ?b4  / ( ABSx - ?b5
            + ?b6  / ( ABSx + ?b7
            - ?b8  / ( ABSx + ?b9
            + ?b10 / ( ABSx + ?b11 ) ) ) ) ) );

        X >0 ->   %  1.28 < |X| <= 12.7
        	Y = X*X/2,
        	1 - math:exp ( - Y )
            * ?b0  / ( ABSx - ?b1
            + ?b2  / ( ABSx + ?b3
            + ?b4  / ( ABSx - ?b5
            + ?b6  / ( ABSx + ?b7
            - ?b8  / ( ABSx + ?b9
            + ?b10 / ( ABSx + ?b11 ) ) ) ) ) );

        true -> 0.0
	end.


