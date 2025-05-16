:- module(templates, [
	template/4,
	template_defined/3,
	snippet_defined/3,
	generate_page/5,
	get_snippets/3
	]).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(md/md_parse)).
:- use_module(library(pprint)).
:- use_module(common).
:- use_module(library).

:- dynamic(template_defined/3).
:- dynamic(snippet_defined/3).
:- dynamic(file_info/2).

template(Name, InXML, OutXML, UseRef) :-
	template_defined(Name, ParamAssoc, Code),
	validate_inputs(ParamAssoc, InXML, Vars),
	apply_template(Vars, Code, [], OutXML, (false, UseRef)), !.

generate_page(SFile, OFile, [InXML], OutXML, UseRef) :-
	retractall(file_info(_,_)),
	assert(file_info(SFile, OFile)),
	empty_assoc(Empty),
	apply_node(Empty, InXML, XML1, UseRef),
	maplist(find_snippets(OFile), XML1, OutXML).

get_snippets(Path, InXML, OutXML) :- 
	maplist(apply_referal(Path), InXML, OutXML).

validate_inputs(ParamAssoc, XML, InAssoc) :-
	empty_assoc(Defined),
	assoc_to_list(ParamAssoc, ParamList),
	with((Defined, InAssoc), (
		(	templates:validate_only(ParamList, XML)
			;
			templates:validate_each(ParamAssoc, XML)
		),
		templates:apply_defaults(ParamList)
	)).

validate_only([(Name-PInfo)], XML, (Defined, Filled)) :-
	pinfo_only(PInfo, true),
	pinfo_type(PInfo, Type),
	expect(templates:convert_arg(XML, Type, Value),
		'Template parameter invalid':Name->Type:{Value}),
	put_assoc(Name, Defined, Value, Filled).

validate_each(_, [], (D,D)).
validate_each(ParamAssoc, [element(Name, _, Content)|Tail], (Defined, Filled)) :-
	expect(get_assoc(Name, ParamAssoc, PInfo), 
		'Unexpected argument':Name),
	pinfo_type(PInfo, Type),
	pinfo_only(PInfo, Only),
	expect(Only=false,
		"Invalid use of the 'only-param' attribute on template parameter (it's only valid for templates with one parameter)":(Name-PInfo)),
	expect(\+ get_assoc(Name, Defined, _),
		'Duplicate argument':Name),
	expect(templates:convert_arg(Content, Type, Value), 
		'Type conversion failed':Name->Type:{Content}),
	put_assoc(Name, Defined, Value, Defined2),
	validate_each(ParamAssoc, Tail, (Defined2, Filled)).

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
	expect(templates:md_parse_string(Str, Html),
		'Malformed markdown'),
	maplist(html_to_xml_h, Html, Xml), !.
md_convert_element(element(Name, Attribs, Content), element(Name, Attribs, Content2)) :-
	(	md_block_element(Name)
	->	convert_markdown(Content, [], Content2)
	;	Content = Content2
	).

% Add elements here if they should have children converted as Markdown
% Otherwise, the element and its children are added as verbatim HTML
md_block_element(details).
md_block_element(div).
md_block_element('nng:refer').
md_block_element('nng:snippet').

html_to_xml_h(Html, Xml) :-
	expect(templates:html_to_xml(Html, Xml),
		'Bad HTML'), !.

html_to_xml(A, A) :- atom(A).
html_to_xml(S, A) :- string(S), atom_string(A, S).
html_to_xml(\List, A) :- is_list(List),
	maplist(html_atom_convert, List, Atoms),
	atomic_list_concat(Atoms, A).
html_to_xml(img(Attribs), element(img, AttrXML, [])) :-
	convert_html_attribs(Attribs, AttrXML).
html_to_xml(E, element(Name, [], Content)) :- E=..[Name, HtmlContent],
	convert_html_elements(HtmlContent, Content).
html_to_xml(E, element(Name, Attribs, Content)) :- E=..[Name, HAttribs, HtmlContent],
	convert_html_attribs(HAttribs, Attribs),
	convert_html_elements(HtmlContent, Content).
html_to_xml(E, E) :-
	bad_html(E).

bad_html(E):-
	writeln('Unexpected HTML'),
	writeln(E),
	fail.

html_atom_convert(A, A) :- atom(A), !.
html_atom_convert(S, A) :- string(S), atom_string(A, S), !.
html_atom_convert(S, _) :-
	bad_markdown(S).

bad_markdown(S) :-
	writeln('Unexpected text-like'),
	(	S=..[A|_]
	->	writeln(func(A, '...'))
	;	'...'),
	fail.

convert_html_elements(List, XList) :- is_list(List),
	maplist(html_to_xml, List, XList).
convert_html_elements(E, [X]) :-
	html_to_xml(E, X).

convert_html_attribs([], []).
convert_html_attribs(A=B, [A=B]).
convert_html_attribs([A=B|T], [A=B|XT]) :-
	convert_html_attribs(T, XT).

apply_defaults([], (D, D)).
apply_defaults([Name-PInfo|Tail], (Defined, InAssoc)) :-
	pinfo_default(PInfo, Default),
	pinfo_required(PInfo, Required),
	(	get_assoc(Name, Defined, _)
		->	apply_defaults(Tail, (Defined, InAssoc))
		;	expect(Required=false, 'Missing required parameter':Name),
			put_assoc(Name, Defined, Default, Defined2),
			apply_defaults(Tail, (Defined2, InAssoc))
	).

apply_template(_, [], Result, Result, (U, U)).
apply_template(Vars, [A|Tail], Xml, Result, (U, UseRef)) :-
	apply_node(Vars, A, NodeXml, NodeUseRef)
	->	append(Xml, NodeXml, Xml2),
		useref_or(NodeUseRef, (U, U2)),
		apply_template(Vars, Tail, Xml2, Result, (U2, UseRef)).

apply_node(_, A, [A], false) :- atom(A).

apply_node(Vars, element('nng:refer', Attrib, _), NodeXml, UseRef) :-
	apply_attribs(Vars, Attrib, Attrib2),
	expect(templates:attrib_get(name, Attrib2, Name),
		'Expected name in nng:refer attributes':Attrib2),
	(	snippet_defined(Name, Path, Content)
	->	file_info(_, OFile),
		snippet_referal(OFile, (Name, Path, Content), Ref),
		NodeXml=[Ref],
		UseRef=false
	;	NodeXml=[element('nng:refer', [name=Name], [])],
		UseRef=true
	).

apply_node(Vars, element(Name, Attrib, Content), NodeXml, UseRef) :-
	apply_template(Vars, Content, [], SubResult, (false, SUseRef)),
	apply_attribs(Vars, Attrib, Attrib2),
	(	template_defined(Name, _, _)
	->	template(Name, SubResult, NodeXml, TUseRef),
		useref_or(SUseRef, (TUseRef, UseRef))
	;	NodeXml=[element(Name, Attrib2, SubResult)],
		UseRef=SUseRef
	).

apply_node(Vars, proc(Code), NodeXml, UseRef) :-
	process(Code, Vars, NodeXml, UseRef).

apply_node(Vars, insert_text(Type, Formula), [Result], false) :-
	expect(templates:evaln(Vars, Formula, Data),
		'Bad formula':Formula),
	to_atom(Data, Type, Result).

apply_node(Vars, insert_xml(Formula), Result, false) :-
	evaln(Vars, Formula, Result).

apply_node(_,_,_,false) :-
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
	maplist(apply_node(Vars), [A|Tail], List, _),
	flatten(List, Flat),
	atomic_list_concat(Flat, Result).
apply_text_field(Vars, A, Result) :-
	apply_node(Vars, A, [Result], _).

attrib_get(Key, [Key=Value|_], Value) :- !.
attrib_get(Key, [_|Other], Value) :- attrib_get(Key, Other, Value).

process(foreach(ListName, Key, Index, Content), Vars, NodeXml, UseRef) :-
	evaln(Vars, ListName, List),
	indeces(List, Indeces),
	maplist(process_foreach(Vars, (Key, Index), Content), List, Indeces, NestedResult, UseRefs),
	useref_flatten(UseRefs, UseRef),
	flatten(NestedResult, NodeXml).

process(match(Formula, Content), Vars, NodeXml, UseRef) :-
	evaln(Vars, Formula, Match),
	process_match(Vars, Match, Content, [], NodeXml, UseRef).
process(when(Formula, Content), Vars, NodeXml, UseRef) :-
	evaln(Vars, Formula, R),
	(	R=true
	->	apply_template(Vars, Content, [], NodeXml, (false, UseRef))
	;	NodeXml = [], UseRef=false).
process(Other, _, _, false) :- err(Other, 'Bad processor').

process_foreach(Vars, (Key, IndexName), Content, Item, Index, Result, UseRef) :-
	put_assoc(Key, Vars, Item, Vars2),
	put_assoc(IndexName, Vars2, Index, Vars3),
	apply_template(Vars3, Content, [], Result, (false, UseRef)).

process_match(_,_,[],Result,Result, false).
process_match(Vars, Match,[E|Tail],Xml,Result, UseRef) :-
	(	E = element(Match, _, Content)
	->	apply_template(Vars, Content, [], SubResult, (false, UseRef)),
		append(Xml, SubResult, Xml2)
	;	Xml2=Xml),
	process_match(Vars, Match, Tail, Xml2, Result, UseRef).

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

useref_or(true, (_, true)).
useref_or(false, (Old, Old)).
useref_flatten(List, UseRef) :-
	(	maplist('='(false), List)
	->	UseRef=false
	;	UseRef=true).

find_snippets(OutPath, element('nng:snippet', Attrib, Content), NodeXml) :-
	maplist(find_snippets(OutPath), Content, SubResult),
	expect(templates:attrib_get(name, Attrib, Name),
		'Expected name in nng:snippet attributes':Attrib),
	expect(\+ templates:snippet_defined(Name, _, _),
		'Duplicate snippet defined':Name),
	assertz(snippet_defined(Name, OutPath, SubResult)),
	writeln(Name=SubResult),
	NodeXml=element(div, [id=Name, class='block-snippet'], SubResult).
find_snippets(OutPath, element(N, A, C), element(N, A, C2)) :-
	maplist(find_snippets(OutPath), C, C2).
find_snippets(_, Other, Other).


apply_referal(OutPath, element('nng:refer', [name=Name], _), SubResult) :-
	(	snippet_defined(Name, Path, Content)
	->	snippet_referal(OutPath, (Name, Path, Content), SubResult)
	;	format(atom(Message), 'ERROR: Undefined snippet: ~w', [Name]),
		writeln(Message),
		SubResult=element(dif, [class='block-snippet referer'], [Message])
	).

apply_referal(OutPath, element(N, A, C), element(N, A, C2)) :-
	maplist(apply_referal(OutPath), C, C2).

apply_referal(_, Other, Other).

snippet_referal(OutPath, (Name, Path, Content), NodeXml) :-
	relative_file_name(Path, OutPath, RelPath),
	format(atom(Link), '~w#~w', [RelPath, Name]),
	append(Content, 
		[element(br, [], []), element(a, [href=Link], ['Quoted Snippet'])],
		SubResult),
	NodeXml=element(div, [class='block-snippet referer'], SubResult).