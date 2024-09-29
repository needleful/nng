#!/usr/bin/env swipl

:- use_module(nng).
:- initialization(main, main).

main([InFolder, OutFolder]) :-
	gen_dir(InFolder, OutFolder).