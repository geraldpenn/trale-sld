% ------------------------------------------------------------------------------
%
% tralesld_hooks.pl
%
% Interface between the Prolog and Java parts of trale-sld.
%
% ------------------------------------------------------------------------------

% ------------------------------------------------------------------------------
% COMMUNICATION WITH GUI
% ------------------------------------------------------------------------------

:- use_module(library(jasper)).
:- use_module(library(charsio)).
:- use_module(library(system)).

:- dynamic jvm_store/1.
:- dynamic gui_store/1.

% wrapper predicate for easier foreign calls
call_foreign_meta(JVM, Goal) :-
   functor(Goal, Name, Arity),  % extract predicate name
   functor(ArgDesc, Name, Arity), % build template
   foreign(Method, java, ArgDesc), % look it up
   !,
   jasper_call(JVM, Method, ArgDesc, Goal).

% registry of all the GUI methods that have to be called from within Prolog 
foreign(method('tralesld.TraleSld','initializeParseTrace',[instance]),java,initialize_parse_trace(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld.TraleSld','registerChartEdge',[instance]),java,register_chart_edge(+object('tralesld.TraleSld'),+integer,+integer,+integer,+chars)).
foreign(method('tralesld.TraleSld','registerStepLocation',[instance]),java,register_step_location(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld.TraleSld','registerStepInformation',[instance]),java,register_step_information(+object('tralesld.TraleSld'),+integer,+chars)).

% Fire up one JVM and store it for future use
load_jvm_if_necessary :-
    jvm_store(_).
    
load_jvm_if_necessary :-
    jasper_initialize([classpath('./bin')],JVM),
    assert(jvm_store(JVM)).

% Load one instance of the graphical SLD
open_sld_gui_window(JavaSLD) :-
	jvm_store(JVM),
	jasper_new_object(JVM,'tralesld.TraleSld',init,init,JavaSLD),
    retractall(gui_store(_)),
    assert(gui_store(JavaSLD)).  

% Called when a parse begins. Words is the list of words to be parsed.
tralesld_parse_begin(Words) :-
    load_jvm_if_necessary,
    open_sld_gui_window(JavaSLD),
    jvm_store(JVM),
    write_to_chars(Words, WordsChars),
    call_foreign_meta(JVM,init_parse_trace(JavaSLD,WordsChars)).

% Called before a new step first appears on the stack to transmit information
% about this step to the GUI. The purpose is to keep the stack lean, with just
% step IDs and no further information about the steps on it. Currently, the
% CommandName is the only information associated with a step, this will change
% significantly.
tralesld_register_step_information(ID, CommandName) :-
    jvm_store(JVM),
    write_to_chars(CommandName, CommandNameChars),
    call_foreign_meta(JVM, register_step_information(ID, CommandNameChars)).

% The following predicates are called with the current stack as an argument,
% containing integer step IDs.

% Called when a step is first called. Stack already contains the ID of this
% step.
tralesld_stack_at_call_port(Stack) :-
  call_foreign_meta(JVM, register_step_location(Stack)).

% Called when a step fails. Stack does not contain the failed step any more.
tralesld_stack_at_fail_port(Stack).

% Called when a failure-driven step completes (i.e. fails). Stack does not
% contain the step any more.
tralesld_stack_at_fail_port(Stack).

% Called when a step completes successfully.
tralesld_stack_at_exit_port(Stack).

% Called when a previously successful step is redone.
tralesld_stack_at_redo_port(Stack).

% Called when an edge is added to the chart (happens as a side effect during
% application of a rule.
tralesld_edge_added(Number,Left,Right,RuleName) :-
	jvm_store(JVM),
	gui_store(JavaSLD),
	call_foreign_meta(JVM,register_chart_edge(JavaSLD,Number,Left,Right,RuleName).

% Called by the debugger to retrieve instructions from the GUI
get_reply_hook(Char).

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
  retractall(sid_stack(_)),
  retractall(sid_next_step(_)),
  asserta(sid_stack([])),
  tralesld_parse_begin(Words).

announce_step_hook(StepID,Command,Line,Goal) :-
  sid_set_next_step(StepID),
  Command =.. [CommandName|_],
  tralesld_register_step_information(StepID, CommandName). % TODO this will eventually contain much more information than just the command name

announce_call_hook :-
  sid_next_step(StepID),
  sid_push(StepID),
  sid_stack(Stack),
  tralesld_stack_at_call_port(Stack).

announce_fail_hook :-
  sid_pop(StepID),
  sid_set_next_step(StepID), % may be retried
  sid_stack(Stack),
  tralesld_stack_at_fail_port(Stack).

announce_finished_hook :-
  sid_pop(StepID),
  sid_set_next_step(StepID), % may be retried
  sid_stack(Stack),
  tralesld_stack_at_finished_port(Stack).

announce_exit_hook :-
  sid_pop(StepID),
  sid_set_next_step(StepID), % may be retried
  sid_stack(Stack),
  tralesld_stack_at_exit_port(Stack).

announce_redo_hook(StepID) :-
  sid_push(StepID),
  sid_set_next_step(StepID), % may be retried
  sid_stack(Stack),
  tralesld_stack_at_redo_port(Stack).
  
announce_edge_added_hook(Number,Left,Right,RuleName) :-
  tralesld_edge_added(Number,Left,Right,RuleName).

announce_edge_retrieved_hook(_Number).

sid_set_next_step(StepID) :-
  retractall(sid_next_step(_)),
  asserta(sid_next_step(StepID)).

sid_push(StepID) :-
  retract(sid_stack(Stack)),
  asserta(sid_stack([StepID|Stack])).
  
sid_pop(StepID) :-
  retract(sid_stack([StepID|Rest])),
  asserta(sid_stack(Rest)).
