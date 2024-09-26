module(nng, [
	process_files/2]).

:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(pprint)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).

:- use_module(templates).

generate(Source, Out) :-
	load_templates(Source),
	!,
	writeln('%-----TEMPLATES GENERATED-----%'),
	gen_dir(Source, Out),
	!.

gen_dir(SourceDir, OutDir) :-
	writeln(dir:SourceDir->OutDir),
	exists_directory(SourceDir),
	(	exists_directory(OutDir)
	;	make_directory(OutDir)),
	!,
	directory_files(SourceDir, ['.', '..'|SFiles]),
	writeln(SourceDir:SFiles),
	h_gen_files(SourceDir, OutDir, SFiles).

gen_file(SFile, OFile) :-
	(	writeln(in:SFile->out:OFile),
		exists_file(SFile),
		access_file(OFile, write),
		load_xml(SFile, SourceXml, [space(remove)]),
		process_file(SourceXml, OutHtml)
	;	writeln('Failed to process file':SFile), fail),
	open(OFile, write, FdOut, []),
	% print_term(OutHtml, [indent_arguments(3)]),
	html_write(FdOut, OutHtml, [doctype(html)]),
	close(FdOut).

h_gen_files(_, _, []).
h_gen_files(S, O, ['.'|F]) :- h_gen_files(S,O,F).
h_gen_files(S, O, ['..'|F]) :- h_gen_files(S,O,F).
h_gen_files(SourceDir, OutDir, [S|Files]) :-
	directory_file_path(SourceDir, S, SPath),
	(	exists_directory(SPath),
		directory_file_path(OutDir, S, OPath),
		gen_dir(SPath, OPath)
	;	atom_concat(Name, '.page.xml', S),
		atom_concat(Name, '.html', OFile),
		!,
		directory_file_path(OutDir, OFile, OPath),
		gen_file(SPath, OPath)),
	h_gen_files(SourceDir, OutDir, Files).