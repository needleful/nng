:- module(common,[
	type/1,
	any/1,
	atomic_type/1,
	xml_type/1,
	numeric_type/1,
	convert_arg/3,
	to_atom/3,
	exp_type/2,
	common_type/2,
	err/2,
	expect/2
	]).

%%% Type information
type(text).
type(date).
type(file).
type(boolean).
type(number).
type(integer).
% list(ElementName,ElementType)
type(list(_,_)).
% struct(Assoc)
% Assoc is Name-Type, recursively defined
type(struct(_)).
type(markdown).
type(xml).

any(_).

convertible(T, T).
convertible(number, text).
convertible(integer, number).
convertible(date, text).
convertible(file, text).
convertible(boolean, text).
convertible(A, C) :- convertible(A, B), convertible(B, C).

atomic_type(T) :- T=text;T=date;T=file;T=boolean;T=number;T=integer.
xml_type(T) :- T=xml;T=markdown.
numeric_type(T) :- T=number;T=integer.

% TODO: more validation for file and date
convert_arg(A, text, A) :- atom(A).
convert_arg(A, date, A) :- atom(A).
convert_arg(A, file, A) :- atom(A).
convert_arg(A, boolean, A) :- A=true;A=false.
convert_arg(A, number, N) :- atom(A), atom_number(A, N).
convert_arg(A, integer, I) :- atom(A), atom_number(A, I), integer(I).
convert_arg(El, list(Name, SubType), R) :-
	maplist(list_item_convert(Name, SubType), El, R).
convert_arg(El, struct(Assoc), R) :-
	validate_inputs(Assoc, El, R).
convert_arg(X, T, X) :- T=xml;T=markdown.

common_type([A|T], C) :-
	common_type(A, T, C).
common_type(A, [], A).
common_type(A, [B|T], C) :-
	convertible(A, TC),
	convertible(B, TC),
	common_type(TC, T, C).

to_atom(A, T, A) :- T=text;T=date;T=file;T=boolean.
to_atom(N, T, A) :- (T=number;T=integer), atom_number(A, N).

list_item_convert(Name, SubType, element(Name, _, Content), Result) :-
	convert_arg(Content, SubType, Result).

exp_type(Op, math) :- member(Op, 
	[	+,-,/,*,mod,rem,//,div,rdiv,gcd,lcm,abs,**,^,
		sign,cmpr,copysign,roundtoward,max,maxr,
		min, minr,random, random_float,round,integer,float,rational,rationalize,
		numerator,denominator,float_fractional_part,float_integer_part,
		truncate,floor,ceiling,ceil,>>,<<,/\,\/,sqrt,sin,cos,tan,
		asin,acos,atan,atan2,sinh,cosh,tanh,asinh,acosh,atanh,log,log10,exp,
		powm,lgamma,erf,erfc,pi,e,epsilon,inf,nan,cputime]).

exp_type(Op, logic) :- member(Op, [';',',','->','\\+']).

err(Term, Message) :- 
	throw(error(Term, Message)).

expect(Term, Message) :-
	(	call(Term)
	->	true
	;	writeln(Message), fail).
