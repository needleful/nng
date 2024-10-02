:- module(library, [
	src_file_exists/1
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

% Replacements for Comparison operators
% Maybe someday I can get them working with read_term_from_atom
:-op(700, xfx, lt).
:-op(700, xfx, gt).
:-op(700, xfx, leq).
:-op(700, xfx, geq).
:-op(700, xfx, eq).
:-op(700, xfx, neq).

X :: boolean :- X=..[Op,A,B],
	numeric_type(A),
	numeric_type(B),
	member(Op, [lt,leq,gt,geq,eq,neq]).

A lt B :- A<B.
A leq B :- A =< B. 
A gt B :- A>B.
A geq B :- A >= B.
A eq B :- A == B.
A neq B :- A \= B.