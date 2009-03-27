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
   catch(jasper_call(JVM, Method, ArgDesc, Goal),
         Excp,
         (is_java_exception(JVM, Excp) ->
          print_exception_info(JVM, Excp);
          throw(Excp))).

% registry of all the GUI methods that have to be called from within Prolog 
foreign(method('tralesld/TraleSld','initializeParseTrace',[instance]),java,init_parse_trace(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerChartEdge',[instance]),java,register_chart_edge(+object('tralesld.TraleSld'),+integer,+integer,+integer,+chars)).
foreign(method('tralesld/TraleSld','registerStepInformation',[instance]),java,register_step_information(+object('tralesld.TraleSld'),+integer,+chars)).
foreign(method('tralesld/TraleSld','registerStepLocation',[instance]),java,register_step_location(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerRuleApplication',[instance]),java,register_rule_application(+object('tralesld.TraleSld'),+integer,+integer,+integer,+chars)).
foreign(method('tralesld/TraleSld','registerStepFailure',[instance]),java,register_step_failure(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerStepFinished',[instance]),java,register_step_finished(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerStepExit',[instance]),java,register_step_exit(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','getPressedButton',[instance]),java,get_pressed_button(+object('tralesld.TraleSld'),[-char])).

% Fire up one JVM and store it for future use
load_jvm_if_necessary :-
    jvm_store(_).
    
load_jvm_if_necessary :-
    jasper_initialize([classpath('/home/ke/workspace/trale-sld/bin')],JVM),
    assert(jvm_store(JVM)).

% Load one instance of the graphical SLD
open_sld_gui_window(JavaSLD) :-
	jvm_store(JVM),
	catch(jasper_new_object(JVM,'tralesld/TraleSld',init,init,JavaSLD),
              Excp,
              (is_java_exception(JVM, Excp) -> print_exception_info(JVM, Excp); throw(Excp))),
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
% step IDs and no further information about the steps on it.
tralesld_step(ID, step(rule(RuleName),
                       _Line,
                       d_add_dtrs(_,_,Left,Right,_,_,Seven,Eight,Nine,_,Eleven,Twelve))) :-
%write(-), write(Seven), nl,
%write(-), write(Eight), nl,
%write(-), write(Nine), nl,
%write(-), write(Eleven), nl,
%write(-), write(Twelve), nl,
    !,
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(RuleName,RuleNameChars),
    call_foreign_meta(JVM,register_rule_application(JavaSLD,ID,Left,Right,RuleNameChars)).

tralesld_step(ID, step(Command,_Line,_Goal)) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Command,CommandChars), % TODO
    atom_chars(CommandAtom,CommandChars), 
    shorten(CommandAtom,ShortenedAtom),
    atom_chars(ShortenedAtom,ShortenedChars),
    call_foreign_meta(JVM,register_step_information(JavaSLD,ID,ShortenedChars)).

shorten(Atom,Shortened) :-
    sub_atom(Atom,0,25,_,Prefix),
    atom_concat(Prefix,'...',Shortened),
    !.

shorten(Atom,Atom).

% The following predicates are called with the current stack as an argument,
% containing integer step IDs.

% Called when a step is first called. Stack already contains the ID of this
% step.
tralesld_call(Stack) :-
  jvm_store(JVM),
  gui_store(JavaSLD),
  write_to_chars(Stack, StackChars),
  call_foreign_meta(JVM, register_step_location(JavaSLD, StackChars)).

% Called when a step fails. Stack does not contain the failed step any more.
tralesld_stack_at_fail_port(Stack).

% Called when a failure-driven step completes (i.e. fails). Stack still
% contains the step.
tralesld_fail(Stack) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_failure(JavaSLD, StackChars)).

% Called when a failure-driven step completes.
tralesld_finished(Stack) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_finished(JavaSLD, StackChars)).

% Called when a step completes successfully. Stack  still contains the step.
tralesld_exit(Stack) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_exit(JavaSLD, StackChars)).

% Called when a previously successful step is redone.
tralesld_redo(Stack).

% Called when an edge is added to the chart (happens as a side effect during
% application of a rule.
tralesld_edge_added(Number,Left,Right,RuleName) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(RuleName, RuleNameChars),
    call_foreign_meta(JVM,register_chart_edge(JavaSLD,Number,Left,Right,RuleNameChars)).

% Called by the debugger to retrieve instructions from the GUI
get_reply_hook(Reply) :-
    parsing(_),
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
  asserta(sid_stack([0])),
  tralesld_parse_begin(Words).

announce_step_hook(StepID,Command,Line,Goal) :-
  sid_set_next_step(StepID),
write('Command: '), write(Command), nl,
write('Goal: '), write(Goal), nl,
  tralesld_step(StepID, step(Command,Line,Goal)), % TODO this will eventually contain much more information than just the command name
  write('success'), nl.

announce_call_hook :-
  sid_next_step(StepID),
  sid_push(StepID),
  sid_stack(Stack),
  tralesld_call(Stack).

announce_fail_hook :-
  sid_stack(OldStack),
  sid_pop(StepID),
  sid_set_next_step(StepID), % may be retried
  tralesld_fail(OldStack).

announce_finished_hook :-
  sid_stack(OldStack),
  sid_pop(StepID),
  sid_set_next_step(StepID), % may be retried
  tralesld_finished(OldStack).

announce_exit_hook :-
  sid_stack(OldStack),
  sid_pop(StepID),
  sid_set_next_step(StepID), % may be retried
  tralesld_exit(OldStack).

announce_redo_hook(StepID) :-
  sid_push(StepID),
  sid_set_next_step(StepID), % may be retried
  sid_stack(Stack),
  tralesld_redo(Stack).
  
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
