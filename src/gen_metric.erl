%% @doc gen_metric defines a behavior module for types which some notion of
%% distance between two concrete instantation of those types.  It requires
%% a function, distance/2, which maps two things of type X onto the real number
%% line.  In addition, there must be some concept of addition and subtraction
%% over the set which the objects belong to.  See euclidian_vector.erl for an 
%% example.
%% @author Jared Kofron <jared.kofron.at.work@gmail.com>
-module(gen_metric).

%% @doc A module which implements gen_metric needs to be constructible.  The 
%% arguments to the constructor are provided as a simple proplist.  It is 
%% allowed for the callback module to have an error associated with 
%% construction.
-callback new(list({term(), term()})) ->
    {ok, any()} | {error, term()}.

%% @doc distance/2 simply maps any two objects onto the real number line.  For
%% example, consider a simple euclidean metric defined over some vector space V.
%% For any two elements P and Q in that space, the distance can be defined 
%% simply as the square root of the sum over the squared differences between the
%% elements of P and Q.
-callback distance(X,X) ->
    float().

%% @doc add/2 adds the two objects together and returns the result.
-callback add(X,X) ->
    X.

%% @doc subtract/2 subtracts the two objects A and B and returns C 
%% s.t. C = A - B.
-callback subtract(X,X) ->
    X.

%% @doc scale/2 scales an object V by a scalar value S.
-callback scale(X, float()) ->
    X.
