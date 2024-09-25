:- module(templates, [
	template/3,
	load_templates/1,
	process_file/2
	]).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(pprint)).
:- use_module(library(sgml)).

% template(Name, Parameters, Content).
% Parameters defined as an association between PName and (PType, Required)
% PType can be text, integer, float, file, xml, or list(Param), which is recursive
:- dynamic(template/3).

load_templates(SourceDir) :-
	working_directory(CWD, SourceDir),
	expand_file_name('*.template.xml', TFiles),
	process_templates(TFiles),
	working_directory(_, CWD).

process_templates([]).
process_templates([A|T]) :- 
	(	load_xml(A, XML, [space(remove)]),
		read_template_xml(XML)
	;	writeln('Failed to parse file':A)),
	process_templates(T).

read_template_xml([element(templates, _, Content)]) :- 
	maplist(read_template_xml, Content).
read_template_xml([element(template, [name=TMPName], Content)]) :-
	\+ processor(TMPName),
	empty_assoc(Params),
	h_read_template(Content, template(TMPName, Params, []), Template),
	assert(Template).

h_read_template([], T, T).
h_read_template([element(param, Attributes, PC)|Tail], template(TName, Params, Content), Template) :-
	process_param(Attributes, PC, (PKey, PValue)),
	put_assoc(PKey, Params, PValue, Params2),
	h_read_template(Tail, template(TName, Params2, Content), Template).
h_read_template([element(content, _, Content)|Tail], template(TName, P, []), Template) :-
	h_read_template(Tail, template(TName, P, Content), Template).

process_param(Attributes, Content, Param) :-
	h_process_param_attr(Attributes, ('', ('', true)), P),
	valid_param(P),
	(Name, (Type, Req)) = P,
	(	(	Type = list,
			Content = [element(param, Atr2, C2)],
			process_param(Atr2, C2, P2),
			Type2 = list(P2),
			Param = (Name, (Type2, Req)))
		;	(	Content = [],
				Param = P)).

h_process_param_attr([], P, P).
h_process_param_attr([name=N|List], ('', (T, R)), Param) :- 
	h_process_param_attr(List, (N, (T, R)), Param).
h_process_param_attr([type=T|List], (N, ('', R)), Param) :-
	h_process_param_attr(List, (N, (T, R)), Param).
h_process_param_attr([required=R|List], (N, (T, true)), Param) :-
	h_process_param_attr(List, (N, (T, R)), Param).

valid_param((_, (PT, PR))) :-
	member(PT, [integer, float, text, xml, file, list, markdown]),
	member(PR, [true, false]).

:- dynamic(text_formula/3).

processor(match).
processor(foreach).

process_file(SourceXml, OutHtml) :-
	empty_assoc(Context),
	process(Context, SourceXml, [], OutHtml).

process(_, [], HTML, HTML).
process(Context, [element(Name, Attribs, Content)|XML], HTML, Result) :-
	attrib_process(Context, Attribs, Attrib2),
	(	(	processor(Name), !,
		process(Context, processor(Name, Attrib2, Content), [], NodeResult))
	
	;	(	template(Name, _, _), !,
		process(Context, template(Name, Attrib2, Content), [], NodeResult))
	
	;	(	process(Context, Content, [], SubResult),
		NodeResult = element(Name, Attrib2, SubResult))),
	!,
	append(HTML, [NodeResult], HTML2),
	process(Context, XML, HTML2, Result).

process(Context, [Atom|XML], HTML, Result) :- atom(Atom),
	text_process(Context, Atom, A2),
	(	A2=[_|_], ANext = A2
	;	Result=[A2]),
	append(HTML, ANext, HTML2),
	!,
	process(Context, XML, HTML2, Result).

process(Context, template(Template, _, Args), HTML, Result) :-
	template(Template, Params, TemplateContent),
	!,
	fill_params(Args, Params, Vars),
	assoc_to_list(Vars, VarList),
	def_vars(Context, VarList, LocalContext),
	process(LocalContext, TemplateContent, HTML, Result).
process(_, processor(Other, _, _), element(unknown_processor, [name=Other], [])).

def_vars(C, [], C).
def_vars(Context, [Key-Value|Tail], NewContext) :-
	put_assoc(Key, Context, Value, Context2),
	def_vars(Context2, Tail, NewContext).

fill_params(Args, Params, Vars) :-
	empty_assoc(EmptyVars),
	fill_param_h(Args, Params, EmptyVars, Vars).

fill_param_h([], Params, Vars1, Vars1) :-
	assoc_to_list(Params, ParamList),
	optional_params(ParamList).
fill_param_h([element(Name, _, Value)|Tail], Params, Vars1, VarsN) :-
	del_assoc(Name, Params, (PType, _), Params2),
	parse_arg(PType, Value, ParsedValue),
	put_assoc(Name, Vars1, (PType, ParsedValue), Vars2),
	fill_param_h(Tail, Params2, Vars2, VarsN).

parse_arg(xml, X, X).
parse_arg(markdown, M, M).
parse_arg(integer, [I], Int) :- atom_number(I, Int), integer(Int).
parse_arg(float, [F], Float) :- atom_number(F, Float).
parse_arg(file, [F], F) :- atom(F).
parse_arg(text, [T], T) :- atom(T).
% TODO: parse_arg for lists
parse_arg(_, _, _).

optional_params([]).
optional_params([_-(_, false)|Tail]) :- optional_params(Tail).

split(Text, Key, Before, After) :- atom(Text), atom(Key),
	sub_atom(Text, A, _, B, Key),
	!,
	sub_atom(Text, 0, A, _, Before),
	sub_atom(Text, _, B, 0, After).

xml_merge([], []).
xml_merge([''], []).
xml_merge([A], A).
xml_merge([''|Tail], Result) :- xml_merge(Tail, Result).
xml_merge([A,B|Tail], Result) :- atom(A), atom(B),
	atom_concat(A, B, AB), xml_merge([AB|Tail], Result).
xml_merge([A|Tail], Result) :-
	xml_merge(Tail, Merged),
	(	Merged=[], Result=A
	;	Merged=[_|_], Result=[A|Merged]
	;	Result=[A,Merged]).

text_process(Context, Text, Result) :-
	(	split(Text, '[[', BeforeMatch, MatchPlus),
		split(MatchPlus, ']]', F, AfterMatch),
		%writeln(formula(F->Text)),
		!,
		do_formula(Context, F, Replaced),
		text_process(Context, AfterMatch, Processed),
		xml_merge([BeforeMatch, Replaced, Processed], Result))
	;	Result = Text.

do_formula(_, N, N) :- number(N).
do_formula(Context, F, Result) :- atom(F),
	get_assoc(F, Context, (_, Result)).

attrib_process(Context, Attrib, Result) :-
	h_attrib_process(Context, Attrib, [], Result).
h_attrib_process(_, [], R, R).
h_attrib_process(C, [A=V|Tail], Tail2, Result) :-
	text_process(C, A, A2),
	text_process(C, V, V2),
	(	(	atom(A2), ANew = A2)
		;	atomic_list_concat(A2, '', ANew)),
	(	(	atom(V2), VNew = V2)
		;	atomic_list_concat(V2, '', VNew)),
	h_attrib_process(C, Tail, [ANew=VNew|Tail2], Result).