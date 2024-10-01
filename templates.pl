:- module(templates, [
	template/3,
	template_defined/3,
	generate_page/2
	]).

:- use_module(library(assoc)).
:- use_module(common).
:- use_module(library).

:- dynamic(template_defined/3).

template(Name, InXML, OutXML) :-
	template_defined(Name, InputDef, Code),
	validate_inputs(InputDef, InXML, Vars),
	apply_template(Vars, Code, [], OutXML).

generate_page(InXML, OutXML) :-
	empty_assoc(Empty),
	apply_node(Empty, InXML, OutXML).

validate_inputs(InputDef, XML, InAssoc) :-
	empty_assoc(Defined),
	validate_in(InputDef, XML, Defined, Filled),
	assoc_to_list(InputDef, InputList),
	apply_defaults(InputList, Filled, InAssoc).

validate_in(_, [], Defined, Defined).
validate_in(InputDef, [element(Name, _, Content)|Tail], Defined, Filled) :-
	expect(\+ get_assoc(Name, Defined, _),
		'Duplicate argument':Name),
	expect(get_assoc(Name, InputDef, (Type, _, _)), 
		'Unexpected argument':Name),
	expect(convert_arg(Content, Type, Value), 
		'Type conversion failed':Content->Type),
	put_assoc(Name, Defined, Value, Defined2),
	validate_in(InputDef, Tail, Defined2, Filled).

apply_defaults([], Defined, Defined).
apply_defaults([Name-(_, Required, Default)|Tail], Defined, InAssoc) :-
	get_assoc(Name, Defined, _)
	->	apply_defaults(Tail, Defined, InAssoc)
	;	expect(Required=false, 'Missing required parameter':Name),
		put_assoc(Name, Defined, Default, Defined2),
		apply_defaults(Tail, Defined2, InAssoc).

apply_template(_, [], Result, Result).
apply_template(Vars, [A|Tail], Xml, Result) :-
	apply_node(Vars, A, NodeResult)
	->	append(Xml, NodeXml, Xml2),
		apply_template(Vars, Tail, Xml2, Result).

apply_node(Vars, A, A) :- atom(A).

apply_node(Vars, element(Name, Attrib, Content), NodeXml) :-
	apply_template(Vars, Content, [], SubContent),
	apply_attribs(Vars, Attrib, Attrib2),
	(	template_defined(Name, _, _)
		->	template(Name, SubContent, NodeXml)
		;	NodeXml=[element(Name, Attrib2, SubContent)]).

apply_node(Vars, processor(Code), NodeXml) :-
	process(Code, NodeXml).

apply_node(Vars, insert_text(Type, Formula), [Result]) :-
	fdo(Vars, Formula, Data),
	to_atom(Data, Type, Result).

apply_node(Vars, insert_xml(Formula), Result) :-
	fdo(Vars, Formula, Result).

process(foreach(ListName, Key, Index, Content), Vars, NodeXml) :-
	fdo(Vars, ListName, List),
	maplist(process_foreach(Vars, Content, Key, Index), List, Indeces, NestedResult),
	flatten(NestedResult, NodeXml).

process(match(Formula, Content), Vars, NodeXml) :-
	fdo(Vars, Formula, Match),
	process_match(Vars, Match, Content, [], NodeXml).
process(Other, _) :- err(Other, 'Unknown processor').

process_foreach(Vars, Content, Key, Index, Item, Index, Result) :-
	put_assoc(Key, Vars, Item, Vars2),
	put_assoc(Key, Vars2, Index, Vars3),
	apply_template(Vars3, Item, [], Result).

process_match(_,_,[],Result,Result).
process_match(Vars, Match,[E|Tail],Xml,Result) :-
	(	E = element(Match, _, Content)
	->	apply_template(Vars, Content, [], SubResult),
		append(Xml, SubResult, Xml2)
	;	Xml2=Xml),
	process_match(Vars, Match, Tail, Xml2, Result).

fdo(_, S, A) :- string(S), string_to_atom(S, A).
fdo(Vars, F, Value) :- atom(F), !,
	get_assoc(F, Vars, Value).
fdo(Vars, Struct:Field, Val) :- !,
	fdo(Vars, Struct, S),
	fdo(S, Field, Val).
% Logic evaluation
fdo(Vars, (A,B), Val) :-
	fdo(Vars, A, LA),
	(	LA=true
	->	fdo(Vars, B, Val)
	;	Val=false).
fdo(Vars, (A;B), Val) :-
	fdo(Vars, A, LA)
	(	LA=true
	->	Val=true
	;	fdo(Vars, B, Val)).
fdo(Vars, (Cond->Then;Else), Val) :-
	fdo(Vars, Cond, R),
	(	R=true
	->	fdo(Vars, Then, Val)
	;	fdo(Vars, Else, Val)).
fdo(Vars, \+Cond, Val) :-
	fdo(Cond),
	(	Cond=true
	->	Val=false
	;	Val=true).
% Catchall
fdo(Vars, Fn, Val) :- Fn =.. [F|Args],
	(	maplist(fdo(Vars), Args, Values),
		Exp =.. [F|Values],
		(	exp_type(F, math)
		->	Val is Exp
		;	(	call(Exp)
			->	Val = true
			;	Val = false)).
fdo(_, F, _) :- err(F, 'Bad formula').
