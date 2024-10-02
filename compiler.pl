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
	load_xml(File, [XML], [space(remove)]),
	compile_template(XML), !.

compile_template(element(templates, _, Templates)) :-
	maplist(compile_template, Templates).

compile_template(element(template, [name=Name], Content)) :-
	empty_assoc(Empty),
	compile_params(Content, (Empty, InputDef), ContentNodes),
	compile_xml(InputDef, ContentNodes, Code),
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
	type(Type), !,
	(	Required=true
	->	expect(Default=[], 
			'Defining "default" on required parameter is redundant':PName)
	;	Required=false,
		(	nonvar(DefaultText)
		->	expect(convert_arg(DefaultText, Type, Default),
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
	compile_code(Input, Node, NodeResult),
	(	NodeResult = [_|_]
	->	append(Working, NodeResult, Working2)
	;	append(Working, [NodeResult], Working2)),
	compile_xml(Input, Body, Working2, Code).

compile_code(Input, A, Code) :- atom(A),
	compile_text(Input, A, Code, any).

compile_code(Input, element(E, Attribs, Xml), Code) :-
	compile_attribs(Input, Attribs, [], CAttr),
	(	processor(E)
	->	compile_processor(Input, E, CAttr, Xml, Processor),
		Code=processor(Processor)
	;	compile_xml(Input, Xml, SubCode),
		Code=element(E,CAttr,SubCode)).

compile_attribs(_,[],C,C).
compile_attribs(Input, [N=V|Tail], Defined, Code) :-
	compile_text(Input, N, NC, numeric_type),
	compile_text(Input, V, VC, numeric_type),
	append(Defined, [NC=VC], Defined2),
	compile_attribs(Input, Tail, Defined2, Code).

compile_text(Input, A, Code, Allowed) :- atom(A), atom(Allowed),
	(	extract(A, Before, Ftext, After)
	->	compile_formula(Input, Ftext, (Type, Formula)),
		call(Allowed, Type),
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

compile_formula(Input, Text, (Type, Formula)) :-
	read_term_from_atom(Text, Formula, [double_quoted(string)]),
	expect(compiler:typecheck(Input, Formula, Type),
		'Bad formula':{Formula}).

constant(true, boolean).
constant(false, boolean).
typecheck(_, I, integer) :- integer(I).
typecheck(_, N, integer) :- number(N).
typecheck(_, S, text) :- string(S).
typecheck(Input, A, Type) :- atom(A),
	typecheck_get(root, Input, A, Type).
typecheck(Input, Base:Field, Type) :-
	typecheck_access(root, Input, Base:Field, Type).
typecheck(Input, A->B;C, Type) :-
	typecheck(Input, A, boolean),
	typecheck(Input, B, TypeB),
	typecheck(Input, C, TypeC),
	common_type([TypeB, TypeC], Type).
typecheck(Input, Fn, Type) :- Fn=..[Op, A1|Args], Op \= ':',
	maplist(typecheck(Input), [A1|Args], ArgTypes),
	typecheck_fn(Op, OpType),
	typecheck_call(OpType, Op, ArgTypes, Type).

typecheck_access(IType, Input, Base:Field, Type) :-
	typecheck_get(IType, Input, Base, BaseType),
	typecheck_access(BaseType, Input, Field, Type).
typecheck_access(IType, Input, Field, Type) :- Field \= _:_,
	typecheck_get(IType, Input, Field, Type).

typecheck_get(root, Input, Name, Type) :- atom(Name), 
	(	constant(Name, Type), !
	;	expect(get_assoc(Name, Input, (Type,_,_)),
			'No variable found':Name)).
typecheck_get(struct(Types), _, Name, Type) :-
	expect(atom(Name), 'Invalid struct field name':Name),
	expect(get_assoc(Name, Types, (Type,_,_)),
		'Field not found':Name).
typecheck_get(list(_, EType), Input, Formula, EType) :- !,
	typecheck(Input, Formula, T),
	expect(T=integer, 'List expected an integer index, got':(T, Formula)).
typecheck_get(Type,_,F,_) :- atom(Type), type(Type),
	format('Tried to get field {~w} on a variable of type {~w}~n', [F, Type]),
	fail.

typecheck_call(math, _, ArgTypes, Type) :- maplist(numeric_type, ArgTypes),
	common_type(ArgTypes, Type).
typecheck_call(logic, _, ArgTypes, boolean) :- maplist(=(boolean), ArgTypes).
typecheck_call(func, Op, ArgTypes, Type) :-
	FnSig=..[Op|ArgTypes],
	library:'::'(FnSig, Type).

typecheck_fn(Op, Type) :- exp_type(Op, Type), !.
typecheck_fn(_, func).

processor(foreach).
processor(match).

type_default(T, '') :- T=text;T=date;T=file.
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