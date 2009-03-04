% code for Java interface communication

:- use_module(library(jasper)).
:- use_module(library(charsio)).
:- use_module(library(system)).

open_gui_window(LCParserGUI, JVM) :-
    get_jvm(JVM),
    jasper_new_object(JVM,'LCParserGUI',init,init,LCParserGUI),
    call_foreign_meta(JVM,set_visible(LCParserGUI,true)).

get_jvm(JVM) :-
	jasper_initialize([classpath('./bin')],JVM).

report_state_to_gui([LCParserGUI,JVM], State) :-
    write_to_chars(State, Chars),
    call_foreign_meta(JVM,report_parse_step(LCParserGUI,Chars)).

pressed_button([LCParserGUI,JVM], Button) :-
    call_foreign_meta(JVM,get_pressed_button(LCParserGUI, Button)).
    
terminate_gui_execution([LCParserGUI,JVM]) :-
    call_foreign_meta(JVM,terminate_execution(LCParserGUI)).
    
notify_success([LCParserGUI,JVM], Result) :-
    report_description([LCParserGUI,JVM], 'Parsing successful!'),
    report_state_to_gui([LCParserGUI,JVM], Result),
    call_foreign_meta(JVM,parsing_success(LCParserGUI)).
    
notify_failure([LCParserGUI,JVM], Result) :-
    report_description([LCParserGUI,JVM], 'Parsing failed!'),
    report_state_to_gui([LCParserGUI,JVM], Result),
    call_foreign_meta(JVM,parsing_failure(LCParserGUI)).

call_foreign_meta(JVM, Goal) :-
   functor(Goal, Name, Arity),  % extract predicate name
   functor(ArgDesc, Name, Arity), % build template
   foreign(Method, java, ArgDesc), % look it up
   !,
   jasper_call(JVM, Method, ArgDesc, Goal).

foreign(method('LCParserGUI','setVisible',[instance]),java,set_visible(+object('LCParserGUI'),+boolean)).
foreign(method('LCParserGUI','addParseStepToHistory',[instance]),java,report_parse_step(+object('LCParserGUI'),+chars)).
foreign(method('LCParserGUI','addDescriptionForNextParseStep',[instance]),java,report_step_description(+object('LCParserGUI'),+string)).
foreign(method('LCParserGUI','getPressedButton',[instance]),java,get_pressed_button(+object('LCParserGUI'),[-string])).
foreign(method('LCParserGUI','terminateExecution',[instance]),java,terminate_execution(+object('LCParserGUI'))).
foreign(method('LCParserGUI','parsingSuccess',[instance]),java,parsing_success(+object('LCParserGUI'))).
foreign(method('LCParserGUI','parsingFailure',[instance]),java,parsing_failure(+object('LCParserGUI'))).

% core of the parser

parse(Category,Sentence) :-
    open_gui_window(LCParserGUI, JVM),
	PdctStack = [[[Category,Y],[[Category,Y]]]],
	((
		parse_word(Category,Sentence,[],PdctStack,NewPdctStack, [LCParserGUI, JVM]),
		!,
		NewPdctStack = [[Result,_]],
		notify_success([LCParserGUI, JVM], Result),
		print('Successful parse:\n'),
		print(Result)
	);
	(
		notify_failure([LCParserGUI, JVM],[Category,Y]),
		print('Parsing not successful!\n')
	)).

parse_word(C,[Word|S2],S,PdctStack,NewPdctStack, JavaEnv) :-
    word(W,Word),
	NewTopVar = [W,[Word]],
	NextPdctStack = [[NewTopVar, _]|PdctStack],
	build_scan_description(Word, W,Description),
    stack_to_hypothetical_tree(NextPdctStack,Tree),
    control_output(Tree, Description),
    report_description(JavaEnv, Description),
    await_gui_guidance(JavaEnv,Tree, Signal),
    ((
        Signal == 'abort', 
        print('Abort signal from GUI!\n'),
        flush_output,
        abort
    );
    (
        Signal == 'close_window', 
        print('GUI window closed! Aborting...\n'),
        flush_output,
        terminate_gui_execution(JavaEnv),
        abort
    );
    (
        Signal == 'reject',
        fail
    );
    (
        Signal == 'accept',
        !
    );
    (
        Signal == 'auto_complete',
        !
    )),
    complete(W,C,S2,S,NextPdctStack,NewPdctStack, JavaEnv).

parse_list([C|Cs],S1,S,PdctStack, NewPdctStack, JavaEnv) :-
	PdctStack = [[TopPdct,TopPdctVars]|LowerStack],
    TopPdctVars = [ParseVar | NextVars],
	ParseVar = [C, _],
	parse_word(C,S1,S2,[[TopPdct,[ParseVar]]|LowerStack],_, JavaEnv),
	NextPdctStack = [[TopPdct,NextVars]|LowerStack],
    parse_list(Cs,S2,S,NextPdctStack,NewPdctStack, JavaEnv).

parse_list([],S,S,PdctStack,NewPdctStack, _) :-
	PdctStack = [[TopPdct,TopPdctVars]|LowerStack],
	TopPdctVars = [],
	NewPdctStack = [[TopPdct,_]|LowerStack].

complete(C,C,S,S,PdctStack,PdctStack, _) :-
	PdctStack = [[TopPdct,_]|LowerStack],
	TopPdct = [C,_],
	LowerStack = [].

complete(C,C,S,S,PdctStack,NewPdctStack, JavaEnv) :-
	PdctStack = [[TopPdct,_]|LowerStack],
	% Case 1: completed an expected symbol, plug into hole in stack, stack shrinks
	LowerStack = [[LowerPdct, LowerPdctVars]|RestStack],
	LowerPdctVars = [[C, LowerPdctVar]],
	TopPdct = [C, TopPdctCore],
	LowerPdctVar = TopPdctCore,
	NewPdctStack = [[LowerPdct,_]|RestStack],
    build_complete_1_description(C,Description),
    stack_to_hypothetical_tree(NewPdctStack,Tree),
    control_output(Tree, Description),
    report_description(JavaEnv, Description),
    await_gui_guidance(JavaEnv,Tree, Signal),
    ((
        Signal == 'abort', 
        print('Abort signal from GUI!\n'),
        flush_output,
        abort
    );
    (
        Signal == 'close_window', 
        print('GUI window closed! Aborting...\n'),
        flush_output,
        terminate_gui_execution(JavaEnv),
        abort
    );
    (
        Signal == 'reject',
        fail
    );
    (
        Signal == 'accept',
        !
    );
    (
        Signal == 'auto_complete',
        !
    )).

complete(W,C,S1,S,PdctStack,NewPdctStack, JavaEnv) :-
    rule(P,[W|Rest]),
	PdctStack = [[TopPdct,_]|LowerStack],
	% Case 2: completed the first symbol on RHS of rule, matches hole in stack
	LowerStack = [[LowerPdct, LowerPdctVars]|RestStack],
	LowerPdctVars = [[P, LowerPdctVar]],
	createHoleList(Rest,HoleList),
	LowerPdctVar = [TopPdct|HoleList],
	NextPdctStack = [[LowerPdct, HoleList]|RestStack],
    build_complete_2_description(P,[W|Rest],Description),
    stack_to_hypothetical_tree(NextPdctStack,Tree),
    control_output(Tree, Description),
    report_description(JavaEnv, Description),
    await_gui_guidance(JavaEnv, Tree, Signal),
    ((
        Signal == 'abort', 
        print('Abort signal from GUI!\n'),
        flush_output,
        abort
    );
    (
        Signal == 'close_window', 
        print('GUI window closed! Aborting...\n'),
        flush_output,
        terminate_gui_execution(JavaEnv),
        abort
    );
    (
        Signal == 'reject',
        fail
    );
    (
        Signal == 'accept',
        !
    );
    (
        Signal == 'auto_complete',
        !
    )),
    parse_list(Rest,S1,S2,NextPdctStack,CompletePdctStack, JavaEnv),
    complete(P,C,S2,S,CompletePdctStack,NewPdctStack, JavaEnv).

complete(W,C,S1,S,PdctStack,NewPdctStack, JavaEnv) :-
    rule(P,[W|Rest]),
	PdctStack = [[TopPdct,_]|LowerStack],
	% Case 2: found first symbol on RHS of rule, but category does not match stack hole
	createHoleList(Rest,HoleList),
	NewTopPdct = [P,[TopPdct|HoleList]],
	NextPdctStack = [[NewTopPdct,HoleList]|LowerStack],
    build_complete_3_description(P,[W|Rest],Description),
    stack_to_hypothetical_tree(NextPdctStack,Tree),
    control_output(Tree, Description),
    report_description(JavaEnv, Description),
    await_gui_guidance(JavaEnv, Tree, Signal),
    ((
        Signal == 'abort', 
        print('Abort signal from GUI!\n'),
        flush_output,
        abort
    );
    (
        Signal == 'close_window', 
        print('GUI window closed! Aborting...\n'),
        flush_output,
        terminate_gui_execution(JavaEnv),
        abort
    );
    (
        Signal == 'reject',
        fail
    );
    (
        Signal == 'accept',
        !
    );
    (
        Signal == 'auto_complete',
        !
    )),
    parse_list(Rest,S1,S2,NextPdctStack,CompletePdctStack, JavaEnv),
    complete(P,C,S2,S,CompletePdctStack,NewPdctStack, JavaEnv).

createHoleList([],[]).

createHoleList([First|Rest],[[First,_]|HoleList]) :-
	createHoleList(Rest,HoleList).

% predicates for debugging output and GUI integration

control_output(PdctStack, Message) :-
	print(Message),
	print(PdctStack),
	print('\n\n').

await_gui_guidance(JavaEnv, Pdct, Pressed) :-
    report_state_to_gui(JavaEnv, Pdct),
    wait_until_button_pressed(JavaEnv,Pressed),
    print(Pressed),
    print('\n'),
    !.

report_description([LCParserGUI,JVM], Message) :-
    call_foreign_meta(JVM,report_step_description(LCParserGUI,Message)).

wait_until_button_pressed(JavaEnv,Pressed) :-
    repeat,
    sleep(0.1),
    pressed_button(JavaEnv,Pressed),
    ((Pressed == 'accept');
    (Pressed == 'reject');
    (Pressed == 'auto_complete');
    (Pressed == 'abort');
    (Pressed == 'close_window')).
    
build_scan_description(Word, W, Description) :-
    format_to_chars("scanned word \"~w\", interpreted to be of category ~w\n",[Word,W],Ch),
	atom_codes(Description,Ch).

build_complete_1_description(C, Description) :-
    format_to_chars("completed the symbol ~w\n matched expectations, integrated into structure\n",[C],Ch),
	atom_codes(Description,Ch).

build_complete_2_description(P, RHS, Description) :-
    format_to_chars("completed the first symbol on RHS of rule ~w --> ~w \nhead matched expectations, structure extended\n",[P,RHS],Ch),
	atom_codes(Description,Ch).

build_complete_3_description(P, RHS, Description) :-
    format_to_chars("found first symbol on RHS of rule ~w --> ~w \nunexpected category, delaying structure decision\n",[P,RHS],Ch),
	atom_codes(Description,Ch).

%base case for hypothetical tree construction: only one tree on stack, no plugging needed
stack_to_hypothetical_tree(Stack,Tree) :-
    Stack = [[Tree,_]|[]].

%recursive case for hypothetical tree construction: multiple trees on stack, plug into upper tree
stack_to_hypothetical_tree(Stack,Tree) :-
    Stack = [[Tree1,_],Tree2WithHoles|RestStack],   
    copy_term(Tree2WithHoles,Tree2WithHolesCopy),
    Tree2WithHolesCopy = [NextTree,Holes2Copy],
    Tree1 = [Head1,Rest1],
    Holes2Copy = [PlugHole|_],
    PlugHole = [_,Var],
    format_to_chars("*~w",[Head1],HeadChars),
    atom_codes(Head,HeadChars),
    Var = [[Head,Rest1]],
    NextStack = [[NextTree,_]|RestStack],
    stack_to_hypothetical_tree(NextStack,Tree).
    

