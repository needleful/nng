:- module(templates_old_, [
	load_templates/1,
	process_file/2,
	process_xml/2
	]).

:- use_module(library(assoc)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(prolog_stack)).
:- use_module(library(pprint)).
:- use_module(library(sgml)).

% template(Name, Context, Input, Result).
:- dynamic(template/4).

% Processors are basically built-in templates
processor(foreach).
processor(match).
processor(let).
processor(when).

err(Term, Message) :- throw(error(Term, context(_, Message))).

default_var(text, '').
default_var(date, 0).
default_var(file, '.').
default_var(Number, 0) :- Number=float; Number=integer.
default_var(List, []) :- List=list(_,_);List=markdown;List=xml.
default_var(struct(_), Empty) :- empty_assoc(Empty). 
default_var(Type, _) :- err(Type,'Unknown type').

load_templates(SourceDir) :-
	retractall(template(_, _, _)),
	atom_concat(SourceDir, '/*.template.xml', TSource),
	expand_file_name(TSource, TFiles),
	maplist(load_template, TFiles).

load_templates_h(File) :-
	load_xml(File, [XML], [space(remove)]),
	(	compile_file(XML)
	;	err(File:XML, 'Failed to compile template')).

compile_file(element(templates, _, Templates)) :-
	maplist(compile_template, Templates).
compile_file(XML) :-
	complie_template(XML).

compile_template(element(template, [name=TName], Elements)) :-
	\+ processor(TName),
	\+ clause(template(TName, _, _, _), _),
	empty_assoc(Params),
	compile_elements(Elements, (Context, Input, Result), Params, Vars, ContentWriter),
	Program = (	
		(	read_args(Params, Input, Vars),
		;	err(TName, 'Failed to read input')),
		(	ContentWriter
		;	err(TName, 'Failed to write content'))
	),
	assert(template(TName, Context, Input, Result) :- Program).

compile_elements(E, Args, Params, VarsWriter) :-
	empty_assoc(Empty),
	read_info(E, Empty, Params, Content),
	build_writer(Params, Vars, Content, Writer).

read_info([], P, P, C) :- nonvar(C).
read_info([element(param, Attr, PC)|Tail], InParams, Params, Content) :-
	check_param(Attr, PC, (Key, Value)),
	(	\+ get_assoc(Key, InParams, _)
	;	err(Key, 'Duplicate parameter name')),
	put_assoc(Key, InParams, Value, Params2),
	read_info(Tail, Params2, Params, Content).
read_info([element(content, Attr, C)|Tail], InParams, Params, Content) :-
	(	Content=content(Attr, C)
	;	err(Content\=content(Attr, C), 'Multiple content elements')),
	read_info(Tail, InParams, Params, Content).

build_writer(_,_,_, true).

read_template_xml(element(templates, _, Content)) :- !,
	maplist(read_template_xml, Content).
read_template_xml(element(template, [name=TempName], Content)) :- !,
	\+ processor(TempName),
	empty_assoc(Params),
	(	read_params(Content, template(TempName, Params, []), Template)
	->	writeln(template:TempName),
		assert(Template)
	;	writeln('Failed to parse template':TempName), fail).
read_template_xml(Other) :- err(Other, 'Invalid Template XML').

read_params([], T, T).
read_params([element(param, Attributes, PC)|Tail], template(TName, Params, Content), Template) :-
	check_param(Attributes, PC, (PKey, PValue)),
	put_assoc(PKey, Params, PValue, Params2),
	read_params(Tail, template(TName, Params2, Content), Template).
read_params([element(content, _, Content)|Tail], template(TName, P, []), Template) :-
	read_params(Tail, template(TName, P, Content), Template).

check_param(Attributes, Content, Param) :-
	h_check_param_attr(Attributes, ('', '', true), P),
	(	valid_param(P)
	->	check_param_type(P, Content, Param)
	;	err(P, 'Invalid parameter')).

h_check_param_attr([], P, P).
h_check_param_attr([name=Name|List], ('', T, R), Param) :- !, 
	(	read_term_from_atom(Name, Var, [double_quotes(string)]),
		atom(Var)
	->	h_check_param_attr(List, (Name, T, R), Param)
	;	err(Name, 'Parameter names should be simple Prolog atoms')).
h_check_param_attr([type=Type|List], (N, '', R), Param) :- !,
	h_check_param_attr(List, (N, Type, R), Param).
h_check_param_attr([required=Required|List], (N, T, true), Param) :- !,
	h_check_param_attr(List, (N, T, Required), Param).
h_check_param_attr([Unknown|_]) :-
	writeln('Unknown parameter':Unknown), fail.

valid_param((_, Type, Required)) :-
	member(Type, [text, date, file, float, integer, xml, list, markdown, struct]),
	member(Required, [false, true]).

check_param_type((N, list, R), [element(param, SubAttr, SubContent)], (N, list(EName, EType), R)) :- !,
	check_param(SubAttr, SubContent, (EName, EType, _)).
check_param_type((N, struct, R), FieldList, (N, struct(Fields), R)) :- !,
	maplist(check_param_struct_field, FieldList, FieldParams),
	list_to_assoc(FieldParams, Fields).
check_param_type(P, _, P).

check_param_struct_field(element(param, Attributes, Content), Name-(Type,Required)) :-
	check_param(Attributes, Content, (Name, Type, Required)).

process_file(SFile, OFile) :-
	(	writeln(in:SFile->out:OFile),
		b_setval(current_source, SFile),
		exists_file(SFile),
		access_file(OFile, write),
		load_xml(SFile, SourceXml, [space(remove)]),
		process_xml(SourceXml, OutHtml)
	;	err(SFile, 'Failed to process file')),
	!,
	(	writeln('Writing to':OFile),
		open(OFile, write, Stream, []),
		writeln(Stream, '<!DOCTYPE html>'),
		html_write(Stream, OutHtml, [
			header(false), layout(false)]),
		!,
		(	close(Stream)
		;	writeln('The file didn\'t close?'))
	;	print_term('Failed to write':OutHtml, [quoted(true)])).

process_xml(SourceXml, OutHtml) :-
	empty_assoc(Context),
	process(Context, SourceXml, [], OutHtml).

process(_, [], Result, Result).
process(Context, [element(Name, Attribs, Content)|XML], WorkingSet, Result) :-
	attrib_process(Context, Attribs, Attrib2),
	(	processor(Name), !,
		Process =.. [Name, Context, Attrib2, Content],
		pdo(Process, NodeResult)
	;	template(Name, _, _), !,
		tdo(Name, Context, Content, NodeResult)
	;	(	process(Context, Content, [], SubResult),
			NodeResult = [element(Name, Attrib2, SubResult)]
		;	err(element(Name, Attrib2, Content), 'Processing element failed'))),
	append(WorkingSet, NodeResult, NewWorkingSet),
	process(Context, XML, NewWorkingSet, Result).
process(Context, [Atom|XML], WorkingSet, Result) :- !, atom(Atom),
	(	text_process(Context, Atom, Replaced), !
	;	err(Atom, 'Text replacement failed')),
	(	Replaced=[_|_], NodeResult=Replaced, !
	;	NodeResult=[Replaced]),
	append(WorkingSet, NodeResult, NewWorkingSet),
	process(Context, XML, NewWorkingSet, Result).

tdo(Template, Context, Args, Result) :-
	template(Template, Params, TemplateContent),
	!,
	fill_params(Args, Params, Vars),
	assoc_to_list(Vars, VarList),
	(	def_vars(Context, VarList, LocalContext), !
	;	throw('Problem defining variables')),
	(	process(LocalContext, TemplateContent, [], Result), !
	;	err(Template, 'Processing template failed')).

pdo(foreach(Context, [list=ListName], Content), Result) :- !,
	pdo(foreach(Context, [list=ListName,i=idx_], Content), Result).
pdo(foreach(Context, [list=ListName,i=IndexName], Content), Result) :- !,
	do_formula(Context, ListName, (Type, List)),
	(	Type=list(KeyName, SubType), !
	;	err(ListName=Type, 'Expected a list')),
	indeces(List, Indeces),
	maplist(process_list_item(KeyName, Context, Content, SubType, IndexName), List, Indeces, NestedResult),
	flatten(NestedResult, Result).
pdo(foreach(C, [i=I, list=L], C), R) :-
	pdo(foreach(C, [list=L, i=I], C), R).

pdo(match(Context, [val=VText], Content), Result) :- !,
	read_term_from_atom(VText, Formula, [double_quotes(string)]),
	do_formula(Context, Formula, (text, Match)),
	match_(Context, Match, Content, [], Result).
pdo(Other, _) :- !, err(Other, 'Unknown processor').

indeces(L, I) :-
	indeces_(L, 1, I).
indeces_([], _, []).
indeces_([_|Tail], C, [C|ITail]) :- 
	Cp1 is C+1,
	indeces_(Tail, Cp1, ITail).

match_(_,_,[], WorkingSet, WorkingSet).
match_(Context, Match, [element(Match, _, Content)|Tail], WorkingSet, Result) :-
	process(Context, Content, [], NodeResult),
	append(WorkingSet, NodeResult, NewSet),
	match_(Context, Match, Tail, NewSet, Result).
match_(C,M,[_|T],S,R) :- match_(C,M,T,S,R).

process_list_item(Key, Context, Content, Type, IndexName, Item, Index, Result) :-
	put_assoc(Key, Context, (Type, Item), ItemContext),
	put_assoc(IndexName, ItemContext, (integer, Index), FullContext),
	(	process(FullContext, Content, [], Result)
	;	err(Item, 'Bad list item')).

fill_params(Args, Params, Vars) :-
	empty_assoc(EmptyVars),
	fill_param_h(Args, Params, EmptyVars, DefinedVars),
	assoc_to_list(Params, ParamList),
	add_defaults(ParamList, DefinedVars, Vars).

fill_param_h([], _, Vars, Vars).
fill_param_h([element(Name, _, Value)|Tail], Params, Vars1, VarsN) :-
	del_assoc(Name, Params, (PType, _), Params2),
	parse_arg(PType, Value, ParsedValue), !,
	put_assoc(Name, Vars1, (PType, ParsedValue), Vars2),
	fill_param_h(Tail, Params2, Vars2, VarsN).

add_defaults([], Defined, Defined).
add_defaults([Name-(Type, false)|Tail], Defined, Vars) :-
	(	get_assoc(Name, Defined, _),
		Defined2=Defined
	;	default_var(Type, Default),
		put_assoc(Name, Defined, (Type, Default), Defined2)),
	add_defaults(Tail, Defined2, Vars).
add_defaults([Name-(Type, true)|Tail], Defined, Vars) :-
	get_assoc(Name, Defined, _), !,
	add_defaults(Tail, Defined, Vars)
	;	err((Name, Type), 'Missing required argument').

parse_arg(xml, X, X).
parse_arg(markdown, M, M).
parse_arg(integer, [I], Int).
parse_arg(float, [F], Float).
parse_arg(file, [F], F).
parse_arg(text, [T], T).
parse_arg(list(_, EType), Args, Values) :- !, maplist(parse_list_arg(EType), Args, Values).
parse_arg(struct(Fields), Args, Values) :- !,
	empty_assoc(Defined),
	parse_struct_args(Fields, Defined, Args, Values).
parse_arg(Type, Value, _) :- err(Type=Value, 'Could not process var').

parse_list_arg(Type, element(_, _, A), V) :- parse_arg(Type, A, V).

parse_struct_args(Fields, Defined, [], Defined) :-
	assoc_to_list(Defined, DefList),
	remove_keys(Fields, DefList, Undefined),
	(	empty_assoc(Undefined)
	;	assoc_to_list(Undefined, UndefList),
		(	maplist(=(_-(_, _, false)), UndefList), !
		;	err(UndefList, 'Missing fields in struct'))).
parse_struct_args(Fields, Defined, [element(Name, _, Value)|Tail], Values) :-
	(	\+get_assoc(Name, Defined, _), !
	;	err('Duplicate struct field':Name), fail),
	get_assoc(Name, Fields, (Type, _)),
	parse_arg(Type, Value, Parsed),
	put_assoc(Name, Defined, (Type, Parsed), Defined2),
	parse_struct_args(Fields, Defined2, Tail, Values).

remove_keys(Assoc, [], Assoc).
remove_keys(Assoc, [Key-_|Tail], Result) :-
	del_assoc(Key, Assoc, _, Assoc2),
	remove_keys(Assoc2, Tail, Result).

def_vars(C, [], C).
def_vars(Context, [Key-Value|Tail], NewContext) :-
	(Type, Data) = Value,
	(	def_var_h(Context, Type, Data, ParsedData),
		put_assoc(Key, Context, (Type, ParsedData), Context2), !
	;	writeln('Problem defining variable':(Key:Type)=Data), fail),
	!, def_vars(Context2, Tail, NewContext).

def_var_h(Context, _, Data, ParsedData) :- atom(Data),
	text_process(Context, Data, ParsedData).
def_var_h(_, _, Data, Data) :- atomic(Data).
def_var_h(Context, list(_, SubType), Data, ParsedData) :-
	maplist(def_var_h(Context, SubType), Data, ParsedData).
def_var_h(Context, Type, Data, ParsedData) :- (Type=xml;Type=markdown),
	process(Context, Data, [], ParsedData).
def_var_h(Context, struct(_), Data, ParsedData) :-
	assoc_to_list(Data, DataList),
	maplist(def_struct_item(Context), DataList, ParseList),
	list_to_assoc(ParseList, ParsedData).
def_var_h(_, Type, Data, _) :- err((Type, Data), 'Could not define args').

def_struct_item(Context, Name-(Type, Data), Name-(Type, Parsed)) :-
	def_var_h(Context, Type, Data, Parsed)
	;	err((Name, Type, Data), 'Failed to define struct field').

attrib_process(Context, Attrib, Result) :-
	maplist(h_attrib_process(Context), Attrib, Result).
h_attrib_process(C, A=V, A2=V2) :-
	text_process(C, A, A2), atom(A2),
	text_process(C, V, V2), atom(V2).

text_process(_, '', '').
text_process(Context, Text, Result) :-
	(	extract(Text, BeforeMatch, '[[', F, ']]', AfterMatch)
	->	(	read_term_from_atom(F, Exp, [double_quotes(string)]),
			do_formula(Context, Exp, (_, Replaced)),
			(	atomic(Replaced)
			->	make_atom(Replaced, RText)
			;	RText=Replaced),
			text_process(Context, AfterMatch, Processed),
			xml_merge([BeforeMatch, RText, Processed], Result))
	;	Result = Text).

% Replacements for Comparison operators
% Maybe someday I can get them working with read_term_from_atom
:-op(700, xfx, lt).
:-op(700, xfx, gt).
:-op(700, xfx, leq).
:-op(700, xfx, geq).
:-op(700, xfx, eq).
:-op(700, xfx, neq).

A lt B :- A<B.
A leq B :- A =< B. 
A gt B :- A>B.
A geq B :- A >= B.
A eq B :- A == B.
A neq B :- A \= B.

% Other formula predicates
src_file_exists(RelPath) :- atom(RelPath),
	b_getval(current_source, SourceFile),
	relative_file_name(RealPath, SourceFile, RelPath),
	exists_file(RealPath).

do_formula(_, true, (text, true)).
do_formula(_, false, (text, false)).
do_formula(_, N, (integer, N)) :- integer(N).
do_formula(_, N, (float, N)) :- float(N).
do_formula(_, S, (text, A)) :- string(S), string_to_atom(S, A).
do_formula(Context, F, Value) :- atom(F), !,
	(	get_assoc(F, Context, Value), !
		; 	err(F=>{Context}, 'Missing variable')).
do_formula(Context, Struct:Field, Val) :- !,
	do_formula(Context, Struct, (_, S)),
	(	do_formula(S, Field, Val), !
	;	throw('No such field':{Struct:Field})).

do_formula(Context, Math, (Type, Val)) :- Math =.. [Op, A, B],
	member(Op, [+,-,/,*]), !,
	do_formula(Context, A, (_, NA)), do_formula(Context, B, (_, NB)),
	Fn =.. [Op, NA, NB],
	(	number(NA), number(NB)
	;	err(Fn, 'Numbers expected')),
	Val is Fn,
	(	integer(Val)
	->	Type=integer
	;	Type=float).

do_formula(Context, Logic, (text, Val)) :- Logic =.. [Op, A, B],
	member(Op, [',',';']), !,
	(	do_formula(Context, A, (text, LA)),
		do_formula(Context, B, (text, LB)),
		Exp =..[Op, LA, LB],
		simple_logic(Exp, Val)
		;	err(Logic, 'Bad logic function')).

do_formula(Context, Fn, (text, Val)) :- Fn =.. [Op|Args], !,
	(	maplist(do_formula(Context), Args, ArgTuples),
		maplist(var_val, ArgTuples, Values),
		Exp =.. [Op|Values],
		(	call(Exp)
		->	Val = true
		;	Val = false)).
do_formula(_, F, _) :- err(F, 'Bad formula').

var_val((_, Val), Val).

simple_logic((false;false), false).
simple_logic((_;_), true).
simple_logic((true,true), true).
simple_logic((_,_), false).

simple_logic(Other, _) :- err(Other, 'Unknown logic expression').

make_atom(A, A) :- atom(A).
make_atom(N, A) :- number(N), atom_number(A, N).

split(Text, Key, Before, After) :- atom(Text), atom(Key),
	sub_atom(Text, A, _, B, Key),
	!,
	sub_atom(Text, 0, A, _, Before),
	sub_atom(Text, _, B, 0, After).

extract(Text, Before, Open, Match, Close, After) :- atom(Open), atom(Close),
	split(Text, Open, Before, MatchPlus),
	split(MatchPlus, Close, Match, After).

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