:- module(templates, [
	template/3,
	load_templates/1,
	process/4,
	text_process/3
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
	(   load_xml(A, XML, [space(remove)]),
	    read_template_xml(XML)
	;   writeln('Failed to parse file':A)),
	process_templates(T).

read_template_xml([element(templates, _, Content)]) :- 
	maplist(read_template_xml, Content).
read_template_xml([element(template, [name=TMPName], Content)]) :-
	empty_assoc(Params),
	h_read_template(Content, template(TMPName, Params, []), Template),
	print_term(Template, [indent_arguments(3)]),
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
	(   (	Type = list,
		Content = [element(param, Atr2, C2)],
		process_param(Atr2, C2, P2),
		Type2 = list(P2),
		Param = (Name, (Type2, Req)))
	;   (	Content = [],
		Param = P)).

h_process_param_attr([], P, P).
h_process_param_attr([name=N|List], ('', (T, R)), Param) :- 
	h_process_param_attr(List, (N, (T, R)), Param).
h_process_param_attr([type=T|List], (N, ('', R)), Param) :-
	h_process_param_attr(List, (N, (T, R)), Param).
h_process_param_attr([required=R|List], (N, (T, true)), Param) :-
	h_process_param_attr(List, (N, (T, R)), Param).

valid_param((PN, (PT, PR))) :-
	(   member(PT, [text, xml, integer, file, float, list, markdown]),
		member(PR, [true, false]))
	;    (	 writeln('Invalid parameter'(PN, (PT, PR))),
		 fail).

:- dynamic(text_formula/3).

processor(match).
processor(foreach).

process(_, [], HTML, HTML).
process(Context, [element(Name, Attribs, Content)|XML], HTML, Result) :-
	text_process(Context, Name, Name2),
	attrib_process(Context, Attribs, Attrib2),

	(   (	processor(Name2),
		process(Context, processor(Name2, Attrib2, Content), NodeResult))
	
	;   (	template(Name2, _, _),
		process(Context, template(Name2, Attrib2, Content), NodeResult))
	
	;   (	process(Context, Content, [], SubResult),
		NodeResult = element(Name, Attribs, SubResult))),
	append(HTML, [NodeResult], HTML2),
	process(Context, XML, HTML2, Result).

process(Context, [Atom|XML], HTML, Result) :-
	text_process(Context, Atom, A2),
	append(HTML, [A2], HTML2),
	process(Context, XML, HTML2, Result).

process(_, processor(_, _, _), element('processed', [], [])).

text_process(Context, Atom, Result) :-
	(   sub_atom(Atom, Start, 2, _, '[['),
	    sub_atom(Atom, End, 2, _, ']]'),
	    Length is End - Start,
	    sub_atom(Atom, Start+2, Length, _, F),
	    writeln(formula(F)),
	    (	text_formula(F, Formula)
	    ;	create_formula(F, Formula)),
	    do_formula(Context, Formula, Result));
	writeln('no replacement':Atom),
	Result = Atom.

attrib_process(Context, Attrib, Result) :-
	h_attrib_process(Context, Attrib, [], Result).
h_attrib_process(_, [], R, R).
h_attrib_process(C, [A=V|Tail], [Tail2], R) :-
	text_process(C, A, A2),
	text_process(C, V, V2),
	h_attrib_process(C, [Tail], [A2=V2|Tail2], R).`