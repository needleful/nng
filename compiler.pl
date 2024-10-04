:- module(compiler, [
	compile_file/1,
	compile_template/1,
	numeric_type/1
	]).

:- use_module(library(sgml)).
:- use_module(common).
:- use_module(library).
:- use_module(templates).

compile_file(File) :-
	load_xml(File, [XML], [space(sgml)]),
	compile_template(XML), !.

compile_template(element(templates, _, Templates)) :-
	maplist(compile_template, Templates).

compile_template(element(template, [name=Name], Content)) :-
	empty_assoc(Empty),
	expect(compiler:compile_params(Content, (Empty, InputDef), ContentNodes),
		'Failed to compile parameters for':Name),
	expect(compiler:compile_xml(InputDef, ContentNodes, Code),
		'Failed to compile content for':Name),
	retractall(template_defined(Name, _, _)),
	assert(template_defined(Name, InputDef, Code)), !.

%%% Parameter compilation

compile_params([], (InputDef, InputDef), _).
compile_params([element(param, Attribs, Nodes)|Tail], (InAssoc, InputDef), ContentNodes) :-
	param_info(InAssoc, Attribs, Nodes, PName-Value),
	put_assoc(PName, InAssoc, Value, InAssoc2),
	compile_params(Tail, (InAssoc2,InputDef), ContentNodes).
compile_params([element(content, _, Content)|Tail], Input, ContentNodes) :-
	ContentNodes=Content,
	compile_params(Tail, Input, ContentNodes).

param_info(InAssoc, Attribs, Nodes, PName-(Type,Required,Default)) :-
	param_attrib(Attribs, Nodes, PName-(Type,Required,DefaultText)),
	expect(nonvar(PName),
		'No name given to parameter'),
	expect(\+get_assoc(PName, InAssoc, _),
		'Duplicate parameter':PName),
	expect(type(Type),
		'Invalid parameter type':{PName, Type}), !,
	(	Required=true
	->	expect(Default=[], 
			'Defining "default" on required parameter is redundant':PName)
	;	Required=false,
		(	nonvar(DefaultText)
		->	expect(convert_text(DefaultText, Type, Default),
				'Invalid conversion for default value':default(PName)={DefaultText}->Type)
		;	type_default(Type, Default))).

param_attrib([], _, _).
param_attrib([name=N|X], Xml, N-Val) :-
	param_attrib(X, Xml, N-Val).
param_attrib([type=T|X], Xml, N-(Type,R,D)) :-
	(	(T=list;T=struct)
	->	get_full_type(T, Xml, Type)
	;	Type=T,
		Xml=[]),
	param_attrib(X, Xml, N-(T,R,D)).
param_attrib([required=R|X], Xml, N-(T,R,D)) :-
	param_attrib(X, Xml, N-(T,R,D)).
param_attrib([default=D|X], Xml, N-(T,R,D)) :-
	param_attrib(X, Xml, N-(T,R,D)).

get_full_type(list, [element(param, Attribs, Content)], list(EName, SubType)) :-
	empty_assoc(Empty),
	param_info(Empty, Attribs, Content, EName-(SubType,_,_)).
get_full_type(struct, Elements, struct(Assoc)) :-
	empty_assoc(Empty),
	compile_params(Elements, (Empty, Assoc), []).

%%% Code compilation

compile_xml(Input, Body, Code) :-
	compile_xml(Input, Body, [], Code).
compile_xml(_,[],Code,Code).
compile_xml(Input,[Node|Body],Working,Code) :-
	is_list(Working),
	(	compile_code(Input, Node, NodeResult)
	;	err(Node, 'Failed to compile node')),
	(	NodeResult = []
	->	Working2=Working
	;	(	NodeResult = [_|_]
		->	append(Working, NodeResult, Working2)
		;	append(Working, [NodeResult], Working2)
		)
	),
	compile_xml(Input, Body, Working2, Code).

compile_code(Input, A, Code) :- atom(A),
	compile_text(Input, A, Code, any).

compile_code(Input, element(E, Attribs, Xml), Code) :-
	(	processor(E)
	->	compile_processor(Input, E, Attribs, Xml, Processor),
		Code=proc(Processor)
	;	expect(compiler:compile_attribs(Input, Attribs, [], CAttr),
			'Bad attributes':Attribs),
		expect(compiler:compile_xml(Input, Xml, SubCode),
			'Bad XML':Xml),
		Code=element(E,CAttr,SubCode)).

processor(foreach).
processor(match).

compile_processor(Input, foreach, Attribs, Xml, foreach(ListName, ElName, Index, SubCode)) :-
	get_foreach_attrib(Input, ('',it_, unknown), Attribs, (ListName, Index, ElName:ElType)),
	put_assoc(ElName, Input, (ElType,true,[]), InputF),
	put_assoc(Index, InputF, (integer,true,[]), ListInput), !,
	compile_xml(ListInput, Xml, SubCode).
compile_processor(Input, match, [val=Formula], Xml, match(Fn, SubCode)) :-
	compile_formula(Input, Formula, (Type, Fn)),
	expect(\+numeric_type(text), 'match does not work on numbers':{Formula}),
	expect(atomic_type(text), 'match only supports matching on text':Type:{Formula}),
	compile_xml(Input, Xml, SubCode).

get_foreach_attrib(_, Compiled, [], Compiled).
get_foreach_attrib(Input, ('', Index, _), [list=Formula|Tail], Result) :-
	expect(
		compiler:typecheck(Input, Formula, list(ElName, ElType), ListF),
		'Formula was not a list':Formula), !,
	get_foreach_attrib(Input, (ListF, Index, ElName:ElType), Tail, Result).
get_foreach_attrib(Input, (List, _, Element), [it=Name|Tail], Result) :-
	atom(Name), !,
	get_foreach_attrib(Input, (List, Name, Element), Tail, Result).
get_foreach_attrib(_, _, [A|_], _) :-
	writeln('Unexpected `foreach` attribute':{A}),
	fail.

compile_attribs(_,[],C,C).
compile_attribs(Input, [N=V|Tail], Defined, Code) :-
	compile_text(Input, N, NC, atomic_type),
	compile_text(Input, V, VC, atomic_type),
	append(Defined, [NC=VC], Defined2),
	compile_attribs(Input, Tail, Defined2, Code).

compile_text(Input, A, Code, Allowed) :- atom(A),
	(	extract(A, Before, Ftext, After)
	->	compile_formula(Input, Ftext, (Type, Formula)),
		expect(call(Allowed, Type),
			'Incompatible types':(Allowed, Type)),
		(	atomic_type(Type)
		->	ThisCode=insert_text(Type, Formula)
		;	xml_type(Type),
			ThisCode=insert_xml(Formula)),
		compile_text(Input, After, AfterCode, Allowed),
		(	AfterCode=[_|_]
		->	append([Before, ThisCode], AfterCode, NewCode)
		;	NewCode=[Before, ThisCode, AfterCode]),
		exclude(=(''), NewCode, Code)
	;	Code=A).

compile_formula(Input, Text, (Type, CheckedFormula)) :-
	read_term_from_atom(Text, Formula, [double_quoted(string), module(library)]),
	expect(compiler:typecheck(Input, Formula, Type, CheckedFormula),
		'Bad formula':{Formula}).

%% The meat of compilation: typechecking
constant(true, boolean).
constant(false, boolean).
typecheck(_, I, integer, I) :- integer(I).
typecheck(_, N, number, N) :- number(N).
typecheck(_, S, text, quote(A)) :- string(S), string_to_atom(S, A).
typecheck(Input, A, Type, A) :- atom(A),
	typecheck_get(root, Input, A, Type, A).
typecheck(Input, (A|T), text, text(Formulas)) :- !,
	typecheck_text_insert(Input, (A|T), [], Formulas).
typecheck(Input, Base:Field, Type, get(Getter)) :-
	typecheck_access(root, Input, Base:Field, Type, [], Getter).
typecheck(Input, A->B;C, Type, cond(Ca, Cb, Cc)) :-
	typecheck(Input, A, boolean, Ca),
	typecheck(Input, B, TypeB, Cb),
	typecheck(Input, C, TypeC, Cc),
	common_type([TypeB, TypeC], Type).
typecheck(Input, Fn, Type, CheckedFn) :- Fn=..[Op, A1|Args], Op \= ':',
	maplist(typecheck(Input), [A1|Args], ArgTypes, CheckedArgs),
	typecheck_fn(Op, OpType),
	expect( compiler:typecheck_call(OpType, Op, ArgTypes, Type, FunName),
		'Bad expression':{Fn}),
	% This maps to
	% m(Op, Args) for math
	% l(Op, Args) for logic
	% p(Op, Args) for predicates
	% f(Op, Args) for functions
	CheckedFn=..[FunName, Op, CheckedArgs].

typecheck_access(IType, Input, Base:Field, Type, Current, Getter) :-
	typecheck_get(IType, Input, Base, BaseType, Inner),
	append(Current, [Inner], C2),
	typecheck_access(BaseType, Input, Field, Type, C2, Getter).
typecheck_access(IType, Input, Field, Type, Current, Getter) :- Field \= _:_,
	typecheck_get(IType, Input, Field, Type, Inner),
	append(Current, [Inner], Getter).

typecheck_get(root, Input, Name, Type, Name) :- atom(Name), 
	(	constant(Name, Type), !
	;	expect(get_assoc(Name, Input, (Type,_,_)),
			'No variable found':Name)).
typecheck_get(struct(Types), _, Name, Type, sget(Name)) :-
	expect(atom(Name), 'Invalid struct field name':Name),
	expect(get_assoc(Name, Types, (Type,_,_)),
		'Field not found':Name).
typecheck_get(list(_, EType), Input, Formula, EType, lget(C)) :- !,
	typecheck(Input, Formula, T, C),
	expect(T=integer, 'List expected an integer index, got':(T, Formula)).
typecheck_get(date, _, Name, integer, dget(Name)) :-
	(Name=day; Name=month; Name=year).
typecheck_get(Type,_,F,_,_) :- atom(Type), type(Type),
	format('Tried to get field {~w} on a variable of type {~w}~n', [F, Type]),
	fail.

typecheck_text_insert(Input, (A | Tail), Current, Result) :-
	check_text_(Input, A, F),
	append(Current, [F], C2),
	typecheck_text_insert(Input, Tail, C2, Result).
typecheck_text_insert(Input, A, Current, Result) :- 
	check_text_(Input, A, F),
	append(Current, [F], Result).

typecheck_call(math, Op, ArgTypes, Type, m) :-
	expect(maplist(numeric_type, ArgTypes),
		'Math operators can only be called with numbers':{Op}),
	common_type(ArgTypes, Type).
typecheck_call(logic, _, ArgTypes, boolean, l) :-
	maplist(=(boolean), ArgTypes).
typecheck_call(func, Op, ArgTypes, Type, FunCall) :-
	FnSig=..[Op|ArgTypes],
	expect( library:'::'(FnSig, Type),
		'No such function defined':{FnSig}),
	% Boolean and non-boolean functions are different
	% Something like `f(X) :: integer` would describe a predicate f(X, Result)
	% so we use 'p' for predicates and 'f' for functions.
	(	Type=boolean
	->	FunCall=p
	;	FunCall=f
	).
typecheck_call(compare, Op, [A,B], boolean, p) :-
	expect((numeric_type(A), numeric_type(B)),
		'Comparison is only valid between numbers':{Op, A, B}).
	

typecheck_fn(Op, Type) :- exp_type(Op, Type), !.
typecheck_fn(_, func).

check_text_(Input, A, Result) :-
	typecheck(Input, A, Type, Checked),
	compile_conversion(Type, Checked, text, Result).

compile_conversion(T, F, T, F). 
compile_conversion(boolean, F, text, F). 
compile_conversion(T, N, text, ntoa(N)) :- numeric_type(T).

type_default(T, '') :- T=text.
type_default(T, false) :- T=boolean.
type_default(T, 0) :- T=number;T=integer.
type_default(T,[]) :- T=list(_,_);T=xml;T=markdown.
type_default(T,Empty) :- T=struct(_), empty_assoc(Empty).

%%% Text processing
extract(Text, Before, Match, After) :- atom(Text),
	split(Text, '[[', Before, MatchPlus),
	split(MatchPlus, ']]', Match, After).

split(Text, Key, Before, After) :- atom(Text), atom(Key),
	sub_atom(Text, A, _, B, Key),
	!,
	sub_atom(Text, 0, A, _, Before),
	sub_atom(Text, _, B, 0, After).