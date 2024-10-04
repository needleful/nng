:- module(library, [
	src_file_exists/1,
	lt/2, 
	leq/2,
	gt/2, 
	geq/2,
	eq/2, 
	neq/2,
	say_month/2
	]).

:- use_module(library(filesex)).
:- use_module(common).

% For type annotations
:- op(1090, xfx, '::').
:- discontiguous('::'/2).

% Other formula predicates
src_file_exists(text) :: boolean.
src_file_exists(RelPath) :- atom(RelPath),
	b_getval(current_source, SourceFile),
	relative_file_name(RealPath, SourceFile, RelPath),
	exists_file(RealPath).

say_month(date) :: text.
say_month(_/1/_, 'January').
say_month(_/2/_, 'February').
say_month(_/3/_, 'March').
say_month(_/4/_, 'April').
say_month(_/5/_, 'May').
say_month(_/6/_, 'June').
say_month(_/7/_, 'July').
say_month(_/8/_, 'August').
say_month(_/9/_, 'September').
say_month(_/10/_, 'October').
say_month(_/11/_, 'November').
say_month(_/12/_, 'December').

% Replacements for Comparison operators
:-op(700, xfx, lt).
:-op(700, xfx, gt).
:-op(700, xfx, leq).
:-op(700, xfx, geq).
:-op(700, xfx, eq).
:-op(700, xfx, neq).

X :: boolean :- X=..[Op,A,B], 
	member(Op, [lt,leq,gt,geq,eq,neq]),
	numeric_type(A),
	numeric_type(B).

A lt B :- A<B.
A leq B :- A =< B. 
A gt B :- A>B.
A geq B :- A >= B.
A eq B :- A == B.
A neq B :- A \= B.