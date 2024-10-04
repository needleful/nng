:- module(common,[
	type/1,
	any/1,
	atomic_type/1,
	xml_type/1,
	numeric_type/1,
	convert_text/3,
	to_atom/3,
	exp_type/2,
	common_type/2,
	err/2,
	expect/2
	]).

%%% Type information
type(text).
type(xml).
type(boolean).
type(number).
type(integer).
type(date).
% list(ElementName,ElementType)
type(list(_,_)).
% struct(Assoc)
% Assoc is Name-Type, recursively defined
type(struct(_)).

any(_).

convertible(T, T).
convertible(number, text).
convertible(integer, number).
convertible(boolean, text).
convertible(A, C) :- convertible(A, B), convertible(B, C).

atomic_type(T) :- T=text;T=boolean;T=number;T=integer;T=date.
xml_type(xml).
numeric_type(T) :- T=number;T=integer.

convert_text(A, text, A) :- atom(A).
convert_text(A, boolean, A) :- A=true;A=false.
convert_text(A, number, N) :- atom(A), atom_number(A, N).
convert_text(A, integer, I) :- atom(A), atom_number(A, I), integer(I).
convert_text(A, date, Year/Month/Day) :-
	read_term_from_atom(A, Year/Month/Day, []),
	integer(Year),
	integer(Month),
	integer(Day).
convert_text(X, T, X) :- T=xml;T=markdown.

common_type([A|T], C) :-
	common_type(A, T, C).
common_type(A, [], A).
common_type(A, [B|T], C) :-
	convertible(A, TC),
	convertible(B, TC),
	common_type(TC, T, C).

to_atom(A, T, A) :- T=text;T=boolean.
to_atom(N, T, A) :- (T=number;T=integer), atom_number(A, N).

exp_type(Op, math) :- member(Op, 
	[	+,-,/,*,mod,rem,//,div,rdiv,gcd,lcm,abs,**,^,
		sign,cmpr,copysign,roundtoward,max,maxr,
		min, minr,random, random_float,round,integer,float,rational,rationalize,
		numerator,denominator,float_fractional_part,float_integer_part,
		truncate,floor,ceiling,ceil,>>,<<,/\,\/,sqrt,sin,cos,tan,
		asin,acos,atan,atan2,sinh,cosh,tanh,asinh,acosh,atanh,log,log10,exp,
		powm,lgamma,erf,erfc,pi,e,epsilon,inf,nan,cputime]).

exp_type(Op, logic) :- member(Op, [';',',','->','\\+']).
exp_type(Op, compare) :- member(Op, ['<', '=<', '==', '\\=', '>=', '>']).

err(Term, Message) :- 
	throw(nng_error(Message, Term)).

expect(Term, Message) :-
	(	call(Term)
	->	true
	;	writeln(Message), fail).
