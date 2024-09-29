#!/usr/bin/env swipl

:- use_module(nng).
:- initialization(main, main).

main([]) :-
	generate('test/src', 'test/www').

main([InFolder, OutFolder]) :-
	generate(InFolder, OutFolder).