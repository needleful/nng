:- module(templates, [
	template/3,
	template_defined/3,
	generate_page/2
	]).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(md/md_parse)).
:- use_module(library(pprint)).
:- use_module(common).
:- use_module(library).

:- dynamic(template_defined/3).

template(Name, InXML, OutXML) :-
	template_defined(Name, InputDef, Code),
	validate_inputs(InputDef, InXML, Vars),
	apply_template(Vars, Code, [], OutXML), !.

generate_page([InXML], OutXML) :-
	empty_assoc(Empty),
	apply_node(Empty, InXML, OutXML), !.

validate_inputs(InputDef, XML, InAssoc) :-
	empty_assoc(Defined),
	validate_in(InputDef, XML, Defined, Filled),
	assoc_to_list(InputDef, InputList),
	apply_defaults(InputList, Filled, InAssoc).

validate_in(_, [], Defined, Defined).
validate_in(InputDef, [element(Name, _, Content)|Tail], Defined, Filled) :-
	expect(get_assoc(Name, InputDef, (Type, _, _)), 
		'Unexpected argument':Name),
	expect(\+ get_assoc(Name, Defined, _),
		'Duplicate argument':Name),
	expect(templates:convert_arg(Content, Type, Value), 
		'Type conversion failed':Name->Type:{Content}),
	put_assoc(Name, Defined, Value, Defined2),
	validate_in(InputDef, Tail, Defined2, Filled).

convert_arg(Content, xml, Content).
convert_arg(Content, markdown, MdContent) :-
	convert_markdown(Content, [], MdContent).
convert_arg([In], Type, Val) :- atomic_type(Type),
	convert_text(Type, In, Val).
convert_arg(List, text, Val) :- is_list(List),
	maplist(convert_text(text), List, Converted),
	atomic_list_concat(Converted, Val).
convert_arg(El, list(Name, SubType), R) :-
	maplist(convert_list_item(Name, SubType), El, R).
convert_arg(El, struct(Assoc), R) :-
	validate_inputs(Assoc, El, R).
convert_arg(Val, Type, _) :-
	writeln('Bad conversion':Val->Type),
	fail.
convert_list_item(Name, SubType, element(Name, _, Content), Result) :-
	convert_arg(Content, SubType, Result).

convert_markdown([], W, W).
convert_markdown([E|Tail], Working, Result) :-
	md_convert_element(E, A),
	(	is_list(A)
	->	append(Working, A, Working2)
	;	append(Working, [A], Working2)
	),
	convert_markdown(Tail, Working2, Result).

md_convert_element(A, Xml) :- atom(A),
	atom_string(A, Str),
	md_parse_string(Str, Html),
	maplist(html_to_xml, Html, Xml).
md_convert_element(element(Name, Attribs, Content), element(Name, Attribs, Content2)) :-
	convert_markdown(Content, [], Content2).

html_to_xml(S, A) :- string(S), atom_string(A, S).
html_to_xml(\List, A) :- is_list(List),
	maplist(html_atom_convert, List, Atoms),
	atomic_list_concat(Atoms, A).
html_to_xml(E, element(Name, [], Content)) :- E=..[Name, HtmlContent],
	convert_html_elements(HtmlContent, Content).
html_to_xml(E, element(Name, Attribs, Content)) :- E=..[Name, HAttribs, HtmlContent],
	convert_html_attribs(HAttribs, Attribs),
	convert_html_elements(HtmlContent, Content).

html_atom_convert(A, A) :- atom(A).
html_atom_convert(S, A) :- string(S), atom_string(A, S).
html_atom_convert(S, _) :-
	writeln('Unexpected text-like'),
	write_canonical(S), !,
	fail.

convert_html_elements(List, XList) :- is_list(List),
	maplist(html_to_xml, List, XList).
convert_html_elements(E, [X]) :-
	html_to_xml(E, X).

convert_html_attribs([], []).
convert_html_attribs(A=B, [A=B]).
convert_html_attribs([A=B|T], [A=B|XT]) :-
	convert_html_attribs(T, XT).

apply_defaults([], Defined, Defined).
apply_defaults([Name-(_, Required, Default)|Tail], Defined, InAssoc) :-
	get_assoc(Name, Defined, _)
	->	apply_defaults(Tail, Defined, InAssoc)
	;	expect(Required=false, 'Missing required parameter':Name),
		put_assoc(Name, Defined, Default, Defined2),
		apply_defaults(Tail, Defined2, InAssoc).

apply_template(_, [], Result, Result).
apply_template(Vars, [A|Tail], Xml, Result) :-
	apply_node(Vars, A, NodeXml)
	->	append(Xml, NodeXml, Xml2),
		apply_template(Vars, Tail, Xml2, Result).

apply_node(_, A, [A]) :- atom(A).

apply_node(Vars, element(Name, Attrib, Content), NodeXml) :-
	apply_template(Vars, Content, [], SubResult),
	apply_attribs(Vars, Attrib, Attrib2),
	(	template_defined(Name, _, _)
	->	template(Name, SubResult, NodeXml)
	;	NodeXml=[element(Name, Attrib2, SubResult)]
	).

apply_node(Vars, proc(Code), NodeXml) :-
	process(Code, Vars, NodeXml).

apply_node(Vars, insert_text(Type, Formula), [Result]) :-
	expect(templates:evaln(Vars, Formula, Data),
		'Bad formula':Formula),
	to_atom(Data, Type, Result).

apply_node(Vars, insert_xml(Formula), Result) :-
	evaln(Vars, Formula, Result).

apply_node(_,_,_) :-
	%writeln('Failed to apply node'),
	%writeln(Node),
	%print_term(Node, []),
	% nl,
	fail.

apply_attribs(Vars, A, A2) :-
	maplist(apply_attr(Vars), A, A2).

apply_attr(Vars, Key=Value, Key2=Value2) :-
	apply_text_field(Vars, Key, Key2),
	apply_text_field(Vars, Value, Value2).
apply_text_field(Vars, [A|Tail], Result) :-
	maplist(apply_node(Vars), [A|Tail], List),
	flatten(List, Flat),
	atomic_list_concat(Flat, Result).
apply_text_field(Vars, A, Result) :-
	apply_node(Vars, A, [Result]).

process(foreach(ListName, Key, Index, Content), Vars, NodeXml) :-
	evaln(Vars, ListName, List),
	indeces(List, Indeces),
	maplist(process_foreach(Vars, (Key, Index), Content), List, Indeces, NestedResult),
	flatten(NestedResult, NodeXml).

process(match(Formula, Content), Vars, NodeXml) :-
	evaln(Vars, Formula, Match),
	process_match(Vars, Match, Content, [], NodeXml).
process(Other, _, _) :- err(Other, 'Bad processor').

process_foreach(Vars, (Key, IndexName), Content, Item, Index, Result) :-
	put_assoc(Key, Vars, Item, Vars2),
	put_assoc(IndexName, Vars2, Index, Vars3),
	apply_template(Vars3, Content, [], Result).

process_match(_,_,[],Result,Result).
process_match(Vars, Match,[E|Tail],Xml,Result) :-
	(	E = element(Match, _, Content)
	->	apply_template(Vars, Content, [], SubResult),
		append(Xml, SubResult, Xml2)
	;	Xml2=Xml),
	process_match(Vars, Match, Tail, Xml2, Result).

evaln(_, quote(A), A).
evaln(_, N, N) :- number(N).
evaln(Vars, F, Value) :- atom(F), !,
	get_assoc(F, Vars, Value).
evaln(Vars, text(List), Result) :- 
	maplist(evaln(Vars), List, Atoms),
	atomic_list_concat(Atoms, Result).
evaln(Vars, ntoa(F), A) :-
	evaln(Vars, F, N),
	atom_number(A, N).
evaln(Vars, m(Op, Args), R) :-
	maplist(evaln(Vars), Args, In),
	Fn=..[Op|In],
	R is Fn.
evaln(Vars, p(Op, Args), R) :-
	maplist(evaln(Vars), Args, In),
	Fn=..[Op|In],
	(	call(Fn)
	->	R=true
	;	R=false).
evaln(Vars, f(Op, Args), R) :-
	maplist(evaln(Vars), Args, In),
	Fn=..[Op|In],
	call(Fn, R).
evaln(Vars, l(Op, [A,B]), R) :-
	Fn=..[Op|[A,B]],
	eval_logic(Vars, Fn, R).
evaln(Vars, l(\+ , [A]), R) :-
	evaln(Vars, A, N),
	negate(N, R). 
evaln(Vars, cond(A,B,C), R) :-
	evaln(Vars, A, F),
	(	F=true
	->	evaln(Vars, B, R)
	;	evaln(Vars, C, R)).
evaln(Vars, get(List), Value) :-
	eval_get(root, Vars, List, Value).

eval_logic(Vars, (A,B), R) :-
	(	evaln(Vars, A, false)
	->	R=false
	;	evaln(Vars, B, R)).
eval_logic(Vars, (A;B), R) :-
	(	evaln(Vars, A, true)
	->	R=true
	;	evaln(Vars, B, R)).
negate(true, false).
negate(false, true).

eval_get(Value, _, [], Value).
eval_get(Ctx, Vars, [A|Tail], Value) :-
	get_one(Ctx, Vars, A, V1),
	eval_get(V1, Vars, Tail, Value).
get_one(_, Vars, A, V) :- atom(A),
	get_assoc(A, Vars, V).
get_one(Struct,_,sget(F),V) :-
	get_assoc(F, Struct, V).
get_one(List,Vars,lget(F),V) :-
	evaln(Vars, F, I),
	nth0(I, List, V).
get_one(Year/_/_,  _,  dget(year),  Year).
get_one(_/Month/_, _,  dget(month), Month).
get_one(_/_/Day,   _,  dget(day),   Day).

indeces(L, I) :-
	indeces_(L, 0, I).
indeces_([], _, []).
indeces_([_|Tail], C, [C|ITail]) :- 
	Cp1 is C+1,
	indeces_(Tail, Cp1, ITail).