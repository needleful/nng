#!/usr/bin/env swipl
:- module(nng, [
	generate/2]).

:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(pprint)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).

:- use_module(common).
:- use_module(compiler).
:- use_module(templates).

:- initialization(main, main).

main([]) :-
	generate('test/src', 'test/www').

main([InFolder, OutFolder]) :-
	generate(InFolder, OutFolder).

generate(Source, Out) :-
	gen_dir(Source, Out, ([], Referals)),
	finish_referals(Referals), !.

load_templates(SourceDir) :-
	atom_concat(SourceDir, '/*.template.xml', TSource),
	expand_file_name(TSource, TFiles),
	maplist(compile_file, TFiles).

gen_dir(SourceDir, OutDir, Referals) :-
	writeln(dir:SourceDir->OutDir),
	exists_directory(SourceDir),
	load_templates(SourceDir),
	(	exists_directory(OutDir)
	;	make_directory(OutDir)),
				  !,
	directory_files(SourceDir, ['.', '..'|SFiles]),
	h_gen_files(SourceDir, OutDir, SFiles, Referals).

h_gen_files(_, _, [], (R, R)).
h_gen_files(S, O, ['.'|F], Ref) :- h_gen_files(S,O,F,Ref).
h_gen_files(S, O, ['..'|F], Ref) :- h_gen_files(S,O,F,Ref).
h_gen_files(SourceDir, OutDir, [S|Files],(R, Refs)) :-
	expect(\+ var(R),
		'Unexpected variable':(SourceDir, S)),
	directory_file_path(SourceDir, S, SPath),
	(	exists_directory(SPath),
		directory_file_path(OutDir, S, OPath),
		gen_dir(SPath, OPath, (R, R2))
	;	atom_concat(_, '.template.xml', S),
		R2=R
	;	atom_concat(Name, '.page.xml', S),
		atom_concat(Name, '.html', OFile),
		!,
		directory_file_path(OutDir, OFile, OPath),
		catch_with_backtrace(
			gen_file(SPath, OPath, Referal),
			Error,
			(	print_message(error, Error),
				Referal=[])
			),

		(	Referal = []
		->	R2 = R
		;	R2 = [Referal|R]
		)
	),
	h_gen_files(SourceDir, OutDir, Files, (R2, Refs)).

gen_file(SFile, OFile, Ref) :-
	(	b_setval(current_source, SFile),
		expect(exists_file(SFile),
			'File does not exist':SFile),
		access_file(OFile, write),
		load_xml(SFile, SourceXml, [space(sgml)]),
		generate_page(SourceXml, OutHtml, HasRef)
	;	err(SFile, 'Failed to process file'),
		HasRef=false
	), !,

	(	HasRef=false
	->	html_out(OutHtml, OFile),
		Ref = []
	;	Ref = (OutHtml, OFile)
	), !.

html_out(OutHtml, OFile) :-
	(	writeln('Writing to':OFile),
		open(OFile, write, Stream, []),
		writeln(Stream, '<!DOCTYPE html>'),
		html_write(Stream, OutHtml, [
			header(false), layout(false)]),
		!,
		(	close(Stream)
		;	writeln('The file didn\'t close?')
		)
	;	print_term('Failed to write':OutHtml, [quoted(true)])
	).

finish_referals([]).
finish_referals([Ref|Others]) :-
	apply_referal(Ref),
	finish_referals(Others).

apply_referal((XML, Output)) :-
	get_snippets(XML, OutHtml),
	html_out(OutHtml, Output).
