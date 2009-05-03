% ------------------------------------------------------------------------------
%
% tralesld_hooks.pl
%
% Interface between the Prolog and Java parts of trale-sld.
%
% ------------------------------------------------------------------------------

:- use_module(library(jasper)).
:- use_module(library(charsio)).
:- use_module(library(system)).

:- dynamic jvm_store/1.
:- dynamic gui_store/1.

% ------------------------------------------------------------------------------
% CALL STACK MAINTENANCE
% These are implementations of the step/port announcement hooks in TRALE's
% debugger. On the call stack maintained here, steps are represented by step/4
% terms packed with lots of information, still largely in TRALE's internal
% format.
% ------------------------------------------------------------------------------

:- dynamic sid_stack/1.
:- dynamic sid_next_step/1.

announce_parse_begin_hook(Words) :-
    tralesld_active,
    !,
    retractall(sid_stack(_)),
    retractall(sid_next_step(_)),
    asserta(sid_stack([0])),
    tralesld_parse_begin(Words).

announce_solution_found_hook(Words,Solution,Residue,Index) :-
    tralesld_active,
    !,
    tralesld_solution_found(Words,Solution,Residue,Index).

announce_step_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    !,
    sid_set_next_step(StepID),
    tralesld_step(StepID,Command,Line,Goal).

announce_call_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    !,
    sid_next_step(StepID),
    sid_push(StepID),
    sid_stack(Stack),
    tralesld_call(Stack,Command,Line,Goal).

announce_fail_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    !,
    sid_stack(OldStack),
    sid_pop(StepID),
    sid_set_next_step(StepID), % may be retried
    tralesld_fail(OldStack,Command,Line,Goal).

announce_finished_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    !,
    sid_stack(OldStack),
    sid_pop(StepID),
    sid_set_next_step(StepID), % may be retried
    tralesld_finished(OldStack,Command,Line,Goal).

announce_exit_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    !,
    sid_stack(OldStack),
    sid_pop(StepID),
    sid_set_next_step(StepID), % may be retried
    tralesld_exit(OldStack,Command,Line,Goal).

announce_redo_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    !,
    sid_push(StepID),
    sid_set_next_step(StepID), % may be retried
    sid_stack(Stack),
    tralesld_redo(Stack,Command,Line,Goal).
  
announce_edge_added_hook(Number,Left,Right,RuleName) :-
    tralesld_active,
    !,
    tralesld_edge_added(Number,Left,Right,RuleName).

announce_edge_retrieved_hook(Number) :-
    tralesld_active,
    !,
    tralesld_edge_retrieved(Number).

sid_set_next_step(StepID) :-
    retractall(sid_next_step(_)),
    asserta(sid_next_step(StepID)).

sid_push(StepID) :-
    retract(sid_stack(Stack)),
    asserta(sid_stack([StepID|Stack])).
  
sid_pop(StepID) :-
    retract(sid_stack([StepID|Rest])),
    asserta(sid_stack(Rest)).

% ------------------------------------------------------------------------------
% JASPER INTERFACE
% ------------------------------------------------------------------------------

% wrapper predicate for easier foreign calls
call_foreign_meta(JVM, Goal) :-
   functor(Goal, Name, Arity),  % extract predicate name
   functor(ArgDesc, Name, Arity), % build template
   foreign(Method, java, ArgDesc), % look it up
   !,
   catch(jasper_call(JVM, Method, ArgDesc, Goal),
         Excp,
         (is_java_exception(JVM, Excp) ->
          print_exception_info(JVM, Excp);
          throw(Excp))).

% registry of all the GUI methods that have to be called from within Prolog 
foreign(method('tralesld/TraleSld','initializeParseTrace',[instance]),java,init_parse_trace(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerChartEdge',[instance]),java,register_chart_edge(+object('tralesld.TraleSld'),+integer,+integer,+integer,+chars)).
foreign(method('tralesld/TraleSld','registerEdgeDependency',[instance]),java,register_edge_dependency(+object('tralesld.TraleSld'),+integer,+integer)).
foreign(method('tralesld/TraleSld','registerStepInformation',[instance]),java,register_step_information(+object('tralesld.TraleSld'),+integer,+chars)).
foreign(method('tralesld/TraleSld','registerRuleApplication',[instance]),java,register_rule_application(+object('tralesld.TraleSld'),+integer,+integer,+integer,+chars)).
foreign(method('tralesld/TraleSld','registerStepSourceCodeLocation',[instance]),java,register_step_source_code_location(+object('tralesld.TraleSld'),+integer,+chars,+integer)).
foreign(method('tralesld/TraleSld','registerStepLocation',[instance]),java,register_step_location(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerStepFailure',[instance]),java,register_step_failure(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerStepFinished',[instance]),java,register_step_finished(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerStepExit',[instance]),java,register_step_exit(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerStepRedo',[instance]),java,register_step_redo(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerMessageChunk',[instance]),java,register_message_chunk(+object('tralesld.TraleSld'),+integer,+chars)).
foreign(method('tralesld/TraleSld','registerMessageEnd',[instance]),java,register_message_end(+object('tralesld.TraleSld'),+integer,+chars)).
foreign(method('tralesld/TraleSld','getPressedButton',[instance]),java,get_pressed_button(+object('tralesld.TraleSld'),[-char])).

:- multifile file_search_path/2.

file_search_path(workspace,'/home/ke/workspace').

% Fire up one JVM and store it for future use
load_jvm_if_necessary :-
    jvm_store(_).
load_jvm_if_necessary :-
%    jasper_initialize([classpath([trale_home('trale-sld/bin'),trale_home('gralej/bin'),trale_home('gralej/lib/tomato.jar'),trale_home('gralej/lib/batik-awt-util.jar'),trale_home('gralej/lib/batik-svggen.jar'),trale_home('gralej/lib/batik-util.jar')])],JVM),
    jasper_initialize([classpath([workspace('trale-sld/bin'),workspace('gralej/bin'),workspace('gralej/lib/tomato.jar'),workspace('gralej/lib/batik-awt-util.jar'),workspace('gralej/lib/batik-svggen.jar'),workspace('gralej/lib/batik-util.jar')])],JVM),
    assert(jvm_store(JVM)).

% Load one instance of the graphical SLD
open_sld_gui_window(JavaSLD) :-
	jvm_store(JVM),
	catch(jasper_new_object(JVM,'tralesld/TraleSld',init,init,JavaSLD),
              Excp,
              (is_java_exception(JVM, Excp) -> print_exception_info(JVM, Excp); throw(Excp))),
    retractall(gui_store(_)),
    assert(gui_store(JavaSLD)).

% ------------------------------------------------------------------------------
% TRACKING THE PARSING PROCESS
% ------------------------------------------------------------------------------

% Called when a parse begins. Words is the list of words to be parsed.
tralesld_parse_begin(Words) :-
    load_jvm_if_necessary,
    open_sld_gui_window(JavaSLD),
    jvm_store(JVM),
    write_to_chars(Words, WordsChars),
    call_foreign_meta(JVM,init_parse_trace(JavaSLD,WordsChars)).

tralesld_solution_found(Words,Solution,Residue,Index) :-
    send_solution_to_gui(Words,Solution,Residue,Index),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars('[0]',StackChars),
    call_foreign_meta(JVM,register_step_exit(JavaSLD,StackChars)).

% Called before a new step first appears on the stack to transmit information
% about this step to the GUI. The purpose is to keep the stack lean, with just
% step IDs and no further information about the steps on it.
tralesld_step(StepID,rule(RuleName),Line,d_add_dtrs(LabelledRuleBody,_,Left,_,_,_,_,_,_,_,_,_)) :-
    !,
    count_cats_in_labelled_rule_body(LabelledRuleBody,Width),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(RuleName,RuleNameChars),
    Right is Left + Width,
    call_foreign_meta(JVM,register_rule_application(JavaSLD,StepID,Left,Right,RuleNameChars)),
    ((Line = [AbsolutePath|LineNumber])
     -> (write_to_chars(AbsolutePath,AbsolutePathChars),
         call_foreign_meta(JVM,register_step_source_code_location(JavaSLD,StepID,AbsolutePathChars,LineNumber)))
      ; true).

tralesld_step(StepID,Command,Line,_Goal) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    command_nodelabel(Command,NodeLabel),
    write_to_chars(NodeLabel,NodeLabelChars),
    call_foreign_meta(JVM,register_step_information(JavaSLD,StepID,NodeLabelChars)),
    ((Line = [AbsolutePath|LineNumber])
     -> (write_to_chars(AbsolutePath,AbsolutePathChars),
         call_foreign_meta(JVM,register_step_source_code_location(JavaSLD,StepID,AbsolutePathChars,LineNumber)))
      ; true). % TODO move to port, do the same for other ports

% Called when a step is first called. Stack already contains the ID of this
% step.
tralesld_call(Stack,Command,Line,Goal) :-
    tralesld_state_enter(Stack,Command,Line,Goal),
    tralesld_state_call(Stack,Command,Line,Goal),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_location(JavaSLD, StackChars)),
    Stack = [StepID|_],
    send_fss_to_gui(StepID,call,Command).

% Called when a failure-driven step completes (i.e. fails). Stack still
% contains the step.
tralesld_fail(Stack,Command,Line,Goal) :-
    tralesld_state_leave(Stack,Command,Line,Goal),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_failure(JavaSLD, StackChars)),
    Stack = [StepID|_],
    send_fss_to_gui(StepID,fail,Command).

% Called when a failure-driven step completes.
tralesld_finished(Stack,Command,Line,Goal) :-
    tralesld_state_leave(Stack,Command,Line,Goal),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_finished(JavaSLD, StackChars)),
    Stack = [StepID|_],
    send_fss_to_gui(StepID,finished,Command).

% Called when a step completes successfully. Stack  still contains the step.
tralesld_exit(Stack,Command,Line,Goal) :-
    tralesld_state_leave(Stack,Command,Line,Goal),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_exit(JavaSLD, StackChars)),
    Stack = [StepID|_],
    send_fss_to_gui(StepID,exit,Command).

% Called when a previously successful step is redone.
tralesld_redo(Stack,Command,Line,Goal) :-
    tralesld_state_enter(Stack,Command,Line,Goal),
    tralesld_state_redo(Stack,Command,Line,Goal),
    % communicate with GUI:
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_redo(JavaSLD, StackChars)),
    Stack = [StepID|_],
    send_fss_to_gui(StepID,redo,Command).

% Called when an edge is added to the chart (happens as a side effect during
% application of a rule.
tralesld_edge_added(Number,Left,Right,RuleName) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(RuleName, RuleNameChars),
    call_foreign_meta(JVM,register_chart_edge(JavaSLD,Number,Left,Right,RuleNameChars)),
    edge_by_index(Number,_,_,_,Dtrs,_),
    register_edge_dependencies(Number,Dtrs).

tralesld_edge_retrieved(Number) :-
    tralesld_state_edge_retrieved(Number).

% ------------------------------------------------------------------------------
% TRACKING THE PARSING PROCESS II (STATEFUL STUFF)
% ------------------------------------------------------------------------------

:- dynamic current_rule_application/2.
:- dynamic edges_retrieved_in_current_rule_application/1.
:- dynamic edges_retrieved_before_step/2.
:- dynamic edge_index_by_daughter_position/2.

tralesld_state_enter(_,rule(RuleName),_,d_add_dtrs(LabelledRuleBody,_,_,_,LeftmostDaughterIndex,_,_,_,_,_,_,_)) :-
    !,
    count_cats_in_labelled_rule_body(LabelledRuleBody,NumberOfDaughters),
    asserta(current_rule_application(RuleName,NumberOfDaughters)),
    asserta(edges_retrieved_in_current_rule_application(1)),
    asserta(edge_index_by_daughter_position(1,LeftmostDaughterIndex)).
tralesld_state_enter(_,_,_,_).
 
tralesld_state_leave(_,rule(_),_,_) :-
    !,
    retractall(current_rule_application(_,_)),
    retractall(edges_retrieved_in_current_rule_application(_)),
    retractall(edges_retrieved_before_step(_,_)),
    retractall(edge_index_by_daughter_position(_,_)).
tralesld_state_leave(_,_,_,_).

tralesld_state_call(Stack,_,_,_) :-
    current_rule_application(_,_),
    !,
    % store number of daughter edges that have already been retrieved at this step:
    Stack = [StepID|_],
    edges_retrieved_in_current_rule_application(N),
    asserta(edges_retrieved_before_step(StepID,N)).
tralesld_state_call(_,_,_,_).

tralesld_state_redo(Stack,_,_,_) :-
    current_rule_application(_,_),
    !,
    % look up number of daughter edges that had already been retrieved at this step:
    Stack = [StepID|_],
    edges_retrieved_before_step(StepID,N),
    retractall(edges_retrieved_in_current_rule_application(_)),
    asserta(edges_retrieved_in_current_rule_application(N)).
tralesld_state_redo(_,_,_,_).

tralesld_state_edge_retrieved(Index) :-
    retract(edges_retrieved_in_current_rule_application(N)),
    O is N + 1,
    asserta(edges_retrieved_in_current_rule_application(O)),
    asserta(edge_index_by_daughter_position(O,Index)).

% ------------------------------------------------------------------------------
% CONTROL
% ------------------------------------------------------------------------------

% Called by the debugger to retrieve instructions from the GUI
get_reply_hook(Reply) :-
    tralesld_active,
    !,
    parsing(_),
    !,
    await_gui_guidance(Reply),
    write(Reply),
    nl.

await_gui_guidance(Pressed) :-
    wait_until_button_pressed(Pressed),
    !.

wait_until_button_pressed(Pressed) :-
    repeat,
        sleep(0.1),
        pressed_button(Pressed),
%write(Pressed), nl,
        \+ Pressed == 110.
%        (
%          Pressed ==  97; % (a)bort
%          Pressed ==  99; % (c)reep
%          Pressed == 102  % (f)ail
%         %Pressed == 108; % (l)eap TODO support
%         %Pressed == 115;  % (s)kip TODO support
%        ).

pressed_button(Button) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    call_foreign_meta(JVM, get_pressed_button(JavaSLD, Button)).

% ------------------------------------------------------------------------------
% NODE LABELS
% ------------------------------------------------------------------------------

command_nodelabel(unify(_,XName,_,_),Label) :-
    !,
    atoms_concat(['unify(',XName,')'],Label).
command_nodelabel(featval(Loc,Feat,_),Label) :-
    !,
    loc_desc(Loc,Desc),
    atoms_concat(['featval(',Desc,':',Feat,')'],Label).
command_nodelabel(type(Loc,Type,_),Label) :-
    !,
    loc_desc(Loc,Desc),
    atoms_concat(['type(',Desc,',',Type,')'],Label).
command_nodelabel(comp(Functor,Arity),Label) :-
    !,
    number_codes(Arity,ArityCodes),
    atom_codes(ArityAtom,ArityCodes),
    atoms_concat(['goal(',Functor,'/',ArityAtom,')'],Label).
command_nodelabel(Command,Label) :-
    Command =.. [Label|_].
loc_desc(empty,'empty cat') :-
    !.
loc_desc(ineq,'inequated desc') :-
    !.
loc_desc(feat(F),F) :-
    !.
loc_desc(lex,'lex entry') :-
    !.
loc_desc(cons(T),Desc) :-
    !,
    atom_concat(T,'-constrained FS',Desc).
loc_desc(lrin,'LR input') :-
    !.
loc_desc(lrout,'LR output') :-
    !.
loc_desc(left,'left arg') :-
    !.
loc_desc(right,'right arg') :-
    !.
loc_desc(arg(N),Desc) :-
    !,
    number_codes(N,NCodes),
    atom_codes(NAtom,NCodes),
    atom_concat('arg',NAtom,Desc).
loc_desc(query_desc,'query desc') :-
    !.
loc_desc(edge,'edge') :-
    !.
loc_desc(dtrlist,'dtr list') :-
    !.
loc_desc(mother,'mother') :-
    !.

% ------------------------------------------------------------------------------
% FEATURE STRUCTURES
% ------------------------------------------------------------------------------

send_fss_to_gui(StepID,call,_) :- % TODO clean up (Command argument no longer needed (or is it?))
    current_rule_application(RuleName,_NumberOfDaughters), % TODO use NumberOfDaughters to display daughters for which no edge has been retrieved yet
    !,
    % Get input words:
    parsing(Words),
    % Get position of leftmost word covered by active edges:
    edge_index_by_daughter_position(1,LeftmostEdgeIndex),
    get_edge_ref(LeftmostEdgeIndex,Left,_,_,_,_),
    % Get position of rightmost word covered by active edges:
    edges_retrieved_in_current_rule_application(EdgeCount),
    edge_index_by_daughter_position(EdgeCount,RightmostEdgeIndex),
    get_edge_ref(RightmostEdgeIndex,_,Right,_,_,_),
    % Build label:
    sublist(Words,Left,Right,Covered),
    append(Covered,['...'],WordsLabel),
    % Build subtrees:
    build_fragment_subtrees(1,EdgeCount,Subtrees),
    asserta(redirect_grale_output_to_tralesld(StepID,call)),
    portray_my_tree(WordsLabel,_,tree(RuleName,WordsLabel,_,Subtrees)),
    retractall(redirect_grale_output_to_tralesld(_,_)).
send_fss_to_gui(_,_,_).

build_fragment_subtrees(DaughterPosition,EdgeCount,[tree(RuleName,Covered,FS,[])|Rest]) :-
    edge_index_by_daughter_position(DaughterPosition,EdgeIndex),
    get_edge_ref(EdgeIndex,Left,Right,FS,_,RuleName),
    parsing(Words),
    sublist(Words,Left,Right,Covered),
    (DaughterPosition = EdgeCount -> (
         Rest = []
     ) ; (
         NextDaughterPosition is DaughterPosition + 1,
         build_fragment_subtrees(NextDaughterPosition,EdgeCount,Rest)
     )).

send_solution_to_gui(Words,Solution,Residue,Index) :-
    parsing(Words),
    asserta(redirect_grale_output_to_tralesld(0,'exit')),
    portray_cat(Words,_,Solution,Residue,Index),
    retractall(redirect_grale_output_to_tralesld(_,_)).

% ------------------------------------------------------------------------------
% FEATURE STRUCTURES - CALLBACK
% ------------------------------------------------------------------------------

tralesld_grale_message_chunk(StepID,Chars) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    call_foreign_meta(JVM, register_message_chunk(JavaSLD,StepID,Chars)).

tralesld_grale_message_end(StepID,Role) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Role,RoleChars),
    call_foreign_meta(JVM, register_message_end(JavaSLD,StepID,RoleChars)).

% ------------------------------------------------------------------------------
% HELPER PREDICATES
% ------------------------------------------------------------------------------

count_cats_in_labelled_rule_body((Term1,Term2), Count) :-
    !,
    count_cats_in_labelled_rule_body(Term1, Count1),
    count_cats_in_labelled_rule_body(Term2, Count2),
    Count is Count1 + Count2.

count_cats_in_labelled_rule_body(lcat(_), 1) :-
    !.

count_cats_in_labelled_rule_body(_, 0).

shorten(Atom,Shortened) :-
    sub_atom(Atom,0,25,_,Prefix),
    atom_concat(Prefix,'...',Shortened),
    !.
shorten(Atom,Atom).

atoms_concat([], '').
atoms_concat([Head|Tail], Atom) :-
    atoms_concat(Tail, TailAtom),
    atom_concat(Head, TailAtom, Atom).

register_edge_dependencies(_,[]) :-
    !.
register_edge_dependencies(Mother,[Daughter|Daughters]) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    call_foreign_meta(JVM,register_edge_dependency(JavaSLD,Mother,Daughter)),
    register_edge_dependencies(Mother,Daughters).

sublist([],_,_,[]) :-
    !.
sublist(List,0,To,SubList) :-
    !,
    sublist(List,To,SubList).
sublist([_|Rest],From,To,SubList) :-
    NewFrom is From - 1,
    NewTo is To - 1,
    sublist(Rest,NewFrom,NewTo,SubList).

sublist(_,0,[]) :-
    !.
sublist([First|Rest],To,SubList) :-
    NewTo is To -1,
    sublist(Rest,NewTo,NewRest),
    SubList = [First|NewRest].

% ------------------------------------------------------------------------------
% EXCEPTION HANDLING
% ------------------------------------------------------------------------------

is_java_exception(_JVM, Thing) :- var(Thing), !, fail.
is_java_exception(_JVM, Thing) :-
   Thing = java_exception(_),      % misc error in Java/Prolog glue
   !.
is_java_exception(JVM, Thing) :-
   jasper_is_object(JVM, Thing),
   jasper_is_instance_of(JVM, Thing, 'java/lang/Throwable').

print_exception_info(_JVM, java_exception(Message)) :- !,
   format(user_error, '~NJasper exception: ~w~n', [Message]).
print_exception_info(JVM, Excp) :-
   /*
   // Approximate Java code
   {
      String messageChars = excp.getMessage();
   }
   */
   jasper_call(JVM,
               method('java/lang/Throwable', 'getMessage', [instance]),
               get_message(+object('java/lang/Throwable'), [-chars]),
               get_message(Excp, MessageChars)),
   /* // Approximate Java code
   {
      StringWriter stringWriter = new StringWriter();
      PrintWriter printWriter = new PrintWriter(stringWriter);
      excp.printStackTrace(printWriter);
      printWriter.close();
      stackTraceChars = StringWriter.toString();
   }
   */
   jasper_new_object(JVM, 'java/io/StringWriter',
                     init, init, StringWriter),
   jasper_new_object(JVM, 'java/io/PrintWriter',
                     init(+object('java/io/Writer')),
                     init(StringWriter), PrintWriter),
   jasper_call(JVM,
               method('java/lang/Throwable', 'printStackTrace', [instance]),
               print_stack_trace(+object('java/lang/Throwable'),
                                 +object('java/io/PrintWriter')),
               print_stack_trace(Excp, PrintWriter)),
   jasper_call(JVM,
               method('java/io/PrintWriter','close',[instance]),
               close(+object('java/io/PrintWriter')),
               close(PrintWriter)),
   jasper_call(JVM,
               method('java/io/StringWriter','toString',[instance]),
               to_string(+object('java/io/StringWriter'),[-chars]),
               to_string(StringWriter, StackTraceChars)),
   jasper_delete_local_ref(JVM, PrintWriter),
   jasper_delete_local_ref(JVM, StringWriter),
   %% ! exceptions are thrown as global references
   jasper_delete_global_ref(JVM, Excp),
   format(user_error, '~NJava Exception: ~s\nStackTrace: ~s~n',
          [MessageChars, StackTraceChars]).
