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
	gen_dir(Source, Out), !.

load_templates(SourceDir) :-
	atom_concat(SourceDir, '/*.template.xml', TSource),
	expand_file_name(TSource, TFiles),
	maplist(compile_file, TFiles).

gen_dir(SourceDir, OutDir) :-
	writeln(dir:SourceDir->OutDir),
	exists_directory(SourceDir),
	load_templates(SourceDir),
	(	exists_directory(OutDir)
	;	make_directory(OutDir)),
	!,
	directory_files(SourceDir, ['.', '..'|SFiles]),
	h_gen_files(SourceDir, OutDir, SFiles).

h_gen_files(_, _, []).
h_gen_files(S, O, ['.'|F]) :- h_gen_files(S,O,F).
h_gen_files(S, O, ['..'|F]) :- h_gen_files(S,O,F).
h_gen_files(SourceDir, OutDir, [S|Files]) :-
	directory_file_path(SourceDir, S, SPath),
	(	exists_directory(SPath),
		directory_file_path(OutDir, S, OPath),
		gen_dir(SPath, OPath)
	;	atom_concat(_, '.template.xml', S)
	;	atom_concat(Name, '.page.xml', S),
		atom_concat(Name, '.html', OFile),
		!,
		directory_file_path(OutDir, OFile, OPath),
		catch_with_backtrace(
			gen_file(SPath, OPath),
			Error,
			print_message(error, Error))),
	h_gen_files(SourceDir, OutDir, Files).

gen_file(SFile, OFile) :-
	(	b_setval(current_source, SFile),
		expect(exists_file(SFile),
			'File does not exist':SFile),
		access_file(OFile, write),
		load_xml(SFile, SourceXml, [space(sgml)]),
		generate_page(SourceXml, OutHtml)
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
	;	print_term('Failed to write':OutHtml, [quoted(true)])),
	!.