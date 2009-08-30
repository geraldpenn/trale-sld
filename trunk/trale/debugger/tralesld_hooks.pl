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
    retractall(solutions_found(_)),
    retractall(ra(_,_,_)),
    retractall(ra_retrieved(_,_)),
    retractall(ra_retrieved_step(_,_,_)),
    retractall(ra_position_index(_,_,_)),
    retractall(uniftrace_infs(_,_,_)),
    retractall(uniftrace_outfs(_,_)),
    retractall(uniftrace_abspath(_,_,_)),
    retractall(uniftrace_result(_,_,_,_)),
    retractall(sid_stack(_)),
    retractall(sid_next_step(_)),
    asserta(sid_stack([0])),
    tralesld_parse_begin(Words).

announce_parse_end_hook :-
    tralesld_active,
    tralesld_parse_end.

announce_abort_hook :-
    tralesld_active,
    tralesld_abort.

announce_solution_found_hook(Words,Solution,Residue,Index) :-
    tralesld_active,
    tralesld_solution_found(Words,Solution,Residue,Index).

announce_step_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    sid_set_next_step(StepID),
    tralesld_step(StepID,Command,Line,Goal).

announce_call_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    sid_next_step(StepID),
    sid_push(StepID),
    sid_stack(Stack),
    tralesld_call(Stack,Command,Line,Goal).

announce_fail_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    tralesld_multifail(StepID,Command,Line,Goal).

tralesld_multifail(StepID,Command,Line,Goal) :-
    sid_stack(OldStack),
    sid_pop(StepID),
    !,
    sid_set_next_step(StepID),
    tralesld_fail(OldStack,Command,Line,Goal).
tralesld_multifail(StepID,_,_,_) :-
    sid_stack([]),
    raise_exception(tralesld_stack_exception(StepID,fail,[])).
tralesld_multifail(StepID,Command,Line,Goal) :-
    sid_stack(OldStack),
    sid_pop(_),
    tralesld_fail(OldStack,_,_,_),                                              % HACK We don't really know what happened to the steps we didn't see the control
    tralesld_multifail(StepID,Command,Line,Goal).                               % flow leave, we just call it "fail" and are happy that we don't need to see any
                                                                                % step-related data at the fail port.

announce_finished_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    tralesld_multifinished(StepID,Command,Line,Goal).

tralesld_multifinished(StepID,Command,Line,Goal) :-
    sid_stack(OldStack),
    sid_pop(StepID),
    !,
    sid_set_next_step(StepID),
    tralesld_finished(OldStack,Command,Line,Goal).
tralesld_multifinished(StepID,_,_,_) :-
    sid_stack([]),
    raise_exception(tralesld_stack_exception(StepID,finished,[])).
tralesld_multifinished(StepID,Command,Line,Goal) :-
    sid_stack(OldStack),
    sid_pop(_),
    tralesld_fail(OldStack,_,_,_),                                              % HACK see above
    tralesld_multifinished(StepID,Command,Line,Goal).

announce_exit_hook(StepID,Command,Line,Goal,DetFlag) :-
    tralesld_active,
    tralesld_multiexit(StepID,Command,Line,Goal,DetFlag).

tralesld_multiexit(StepID,Command,Line,Goal,DetFlag) :-
    sid_stack(OldStack),
    sid_pop(StepID),
    !,
    sid_set_next_step(StepID),
    tralesld_exit(OldStack,Command,Line,Goal,DetFlag).
tralesld_multiexit(StepID,_,_,_,_) :-
    sid_stack([]),
    raise_exception(tralesld_stack_exception(StepID,exit,[])).
tralesld_multiexit(StepID,Command,Line,Goal,DetFlag) :-
    sid_stack(OldStack),
    sid_pop(_),
    tralesld_fail(OldStack,_,_,_),                                              % HACK see above
    tralesld_multiexit(StepID,Command,Line,Goal,DetFlag).

announce_redo_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    sid_push(StepID),
    sid_set_next_step(StepID), % may be retried
    sid_stack(Stack),
    tralesld_redo(Stack,Command,Line,Goal).
  
announce_edge_added_hook(Number,Left,Right,RuleName) :-
    tralesld_active,
    tralesld_edge_added(Number,Left,Right,RuleName).

announce_edge_retrieved_hook(Number,Left,Right,RuleName) :-
    tralesld_active,
    tralesld_edge_retrieved(Number,Left,Right,RuleName).

sid_set_next_step(StepID) :-
    retractall(sid_next_step(_)),
    asserta(sid_next_step(StepID)).

sid_push(StepID) :-
    retract(sid_stack(Stack)),
    asserta(sid_stack([StepID|Stack])).
  
sid_pop(StepID) :-
    retract(sid_stack([StepID|Rest])),
    asserta(sid_stack(Rest)).

sid_clear(StepID) :-
    retract(sid_stack(Stack)),
    sid_clear(StepID,Stack,NewStack),
    asserta(sid_stack(NewStack)).

% ------------------------------------------------------------------------------
% JASPER INTERFACE
% ------------------------------------------------------------------------------

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
foreign(method('tralesld/TraleSld','start',[instance]),java,start_sld_gui(+object('tralesld.TraleSld'))).
foreign(method('tralesld/TraleSld','initializeParseTrace',[instance]),java,init_parse_trace(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerChartEdge',[instance]),java,register_chart_edge(+object('tralesld.TraleSld'),+integer,+integer,+integer,+chars)).
foreign(method('tralesld/TraleSld','registerEdgeDependency',[instance]),java,register_edge_dependency(+object('tralesld.TraleSld'),+integer,+integer)).
foreign(method('tralesld/TraleSld','registerStepInformation',[instance]),java,register_step_information(+object('tralesld.TraleSld'),+integer,+chars)).
foreign(method('tralesld/TraleSld','registerRuleApplication',[instance]),java,register_rule_application(+object('tralesld.TraleSld'),+integer,+integer,+integer,+chars)).
foreign(method('tralesld/TraleSld','registerStepSourceCodeLocation',[instance]),java,register_step_source_code_location(+object('tralesld.TraleSld'),+integer,+chars,+integer)).
foreign(method('tralesld/TraleSld','registerStepLocation',[instance]),java,register_step_location(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerStepFailure',[instance]),java,register_step_exception(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerStepFinished',[instance]),java,register_step_finished(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerStepExit',[instance]),java,register_step_exit(+object('tralesld.TraleSld'),+chars,+boolean)).
foreign(method('tralesld/TraleSld','registerStepRedo',[instance]),java,register_step_redo(+object('tralesld.TraleSld'),+chars)).
foreign(method('tralesld/TraleSld','registerMessageChunk',[instance]),java,register_message_chunk(+object('tralesld.TraleSld'),+integer,+chars)).
foreign(method('tralesld/TraleSld','registerMessageEnd',[instance]),java,register_message_end(+object('tralesld.TraleSld'),+integer,+chars)).
foreign(method('tralesld/TraleSld','registerParseEnd',[instance]),java,register_parse_end(+object('tralesld.TraleSld'))).
foreign(method('tralesld/TraleSld','getPressedButton',[instance]),java,get_pressed_button(+object('tralesld.TraleSld'),[-char])).

:- multifile file_search_path/2.

file_search_path(traleslddev,Traleslddev) :-
    environ('TRALESLD_TRUNK_DIR',Traleslddev).
file_search_path(gralejdev,Gralejdev) :-
    environ('GRALEJ_TRUNK_DIR',Gralejdev).

tralesld_classpath([traleslddev('bin'),
        traleslddev('lib/jgraph.jar'),
        traleslddev('lib/jgrapht-jdk1.6.jar'),
        traleslddev('lib/derby.jar'),
        gralejdev('bin'),
        gralejdev('lib/tomato.jar'),
        gralejdev('lib/batik-awt-util.jar'),
        gralejdev('lib/batik-svggen.jar'),
        gralejdev('lib/batik-util.jar')]) :-
    environ('TRALESLD_TRUNK_DIR',TTD),
    environ('GRALEJ_TRUNK_DIR',TGDD),
    \+ TTD = '',
    \+ TGDD = '',
    !.
tralesld_classpath([trale_home('trale-sld/trale-sld.jar'),
        trale_home('trale-sld/lib/jgraph.jar'),
        trale_home('trale-sld/lib/jgrapht-jdk1.6.jar'),
        trale_home('trale-sld/lib/derby.jar'),
        trale_home('trale-sld/lib/gralej.jar'),
        trale_home('trale-sld/lib/tomato.jar'),
        trale_home('trale-sld/lib/batik-awt-util.jar'),
        trale_home('trale-sld/lib/batik-svggen.jar'),
        trale_home('trale-sld/lib/batik-util.jar')]).

% Fire up one JVM and store it for future use
load_jvm_if_necessary :-
    jvm_store(_),
    !.
load_jvm_if_necessary :-
    tralesld_classpath(Classpath),
    write('Using classpath: '),
    write(Classpath),
    nl,
    jasper_initialize([classpath(Classpath)],JVM),
    assert(jvm_store(JVM)).

% Load one instance of the graphical SLD
open_sld_gui_window(JavaSLD) :-
	jvm_store(JVM),
	catch((jasper_new_object(JVM,'tralesld/TraleSld',init,init,JavaSLD),
               call_foreign_meta(JVM,start_sld_gui(JavaSLD))),
              Excp,
              (is_java_exception(JVM, Excp) -> print_exception_info(JVM, Excp); throw(Excp))),
    retractall(gui_store(_)),
    assert(gui_store(JavaSLD)).

% ------------------------------------------------------------------------------
% TRACKING THE PARSING PROCESS
% ------------------------------------------------------------------------------

:- dynamic solutions_found/1.

% Called when a parse begins. Words is the list of words to be parsed.
tralesld_parse_begin(Words) :-
    asserta(solutions_found(0)),
    load_jvm_if_necessary,
    open_sld_gui_window(JavaSLD),
    jvm_store(JVM),
    write_to_chars(Words, WordsChars),
    call_foreign_meta(JVM,init_parse_trace(JavaSLD,WordsChars)).

tralesld_parse_end :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    call_foreign_meta(JVM,register_parse_end(JavaSLD)),
    jasper_delete_local_ref(JVM,JavaSLD),
    retractall(gui_store(_)).

tralesld_abort :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    jasper_delete_local_ref(JVM,JavaSLD),
    retractall(gui_store(_)).

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
      ; true).

% Called when a step is first called. Stack already contains the ID of this
% step.
tralesld_call(Stack,Command,Line,Goal) :-
    tralesld_ra_enter(Stack,Command,Line,Goal),
    tralesld_ra_call(Stack,Command,Line,Goal),
    tralesld_lex_enter(Stack,Command,Line,Goal),
    tralesld_uniftrace_call(Stack,Command,Line,Goal),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack,StackChars),
    send_fss_to_gui(Stack,single,Command),
    call_foreign_meta(JVM,register_step_location(JavaSLD,StackChars)).

% Called when a step fails.
tralesld_fail(Stack,Command,Line,Goal) :-
    tralesld_ra_fail(Stack,Command,Line,Goal),
    tralesld_ra_leave(Stack,Command,Line,Goal),
    tralesld_uniftrace_fail(Stack,Command,Line,Goal),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_exception(JavaSLD, StackChars)).

% Called when a failure-driven step completes.
tralesld_finished(Stack,Command,Line,Goal) :-
    tralesld_ra_leave(Stack,Command,Line,Goal),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_finished(JavaSLD, StackChars)).

% Called when a step completes successfully. Stack  still contains the step.
tralesld_exit(Stack,Command,Line,Goal,DetFlag) :-
    tralesld_ra_leave(Stack,Command,Line,Goal),
    tralesld_uniftrace_exit(Stack,Command,Line,Goal),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    send_fss_to_gui(Stack,single,Command),
    call_foreign_meta(JVM, register_step_exit(JavaSLD, StackChars,DetFlag)).

% Called when a previously successful step is redone.
tralesld_redo(Stack,Command,Line,Goal) :-
    tralesld_ra_enter(Stack,Command,Line,Goal),
    tralesld_ra_redo(Stack,Command,Line,Goal),
    tralesld_lex_enter(Stack,Command,Line,Goal),
    % communicate with GUI:
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_redo(JavaSLD, StackChars)).

% Called when an edge is added to the chart (happens as a side effect during
% application of a rule.
tralesld_edge_added(Number,Left,Right,RuleName) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(RuleName, RuleNameChars),
    call_foreign_meta(JVM,register_chart_edge(JavaSLD,Number,Left,Right,RuleNameChars)),
    edge_by_index(Number,_,_,_,Dtrs,_),
    register_edge_dependencies(Number,Dtrs).

tralesld_edge_retrieved(Number,Left,Right,RuleName) :-
    tralesld_ra_edge_retrieved(Number,Left,Right,RuleName).

% ------------------------------------------------------------------------------
% TRACKING RULE APPLICATIONS
% Manages four dynamic predicates that store information about the current rule
% application(s). All of these four predicates have RAID, the StepID of the
% associated RA (rule application), as their first argument.
% ------------------------------------------------------------------------------

:- dynamic ra/3.                % current rule application (stacked assertions)
:- dynamic ra_retrieved/2.      % how many edges (2nd arg) for this RA have already been retrieved
:- dynamic ra_retrieved_step/3. % how many edges (3rd arg) had been retrieved before a certain step (ID in 2nd arg)
:- dynamic ra_position_index/3. % stores N (2nd arg) and the edge index of the N-th daughter (3rd arg)

tralesld_ra_enter([RAID|_],rule(RuleName),_,d_add_dtrs(LabelledRuleBody,_,_,_,LeftmostDaughterIndex,_,_,_,_,_,_,_)) :-
    !,
    count_cats_in_labelled_rule_body(LabelledRuleBody,DaughterCount),
    asserta(ra(RAID,RuleName,DaughterCount)),
    asserta(ra_retrieved(RAID,1)),
    asserta(ra_position_index(RAID,1,LeftmostDaughterIndex)).
tralesld_ra_enter(_,_,_,_).

tralesld_ra_leave([RAID|_],_,_,_) :-
    retract(ra(RAID,_,_)),
    !,
    retractall(ra_retrieved(RAID,_)),
    retractall(ra_retrieved_step(RAID,_,_)),
    retractall(ra_position_index(RAID,_,_)),
    retractall(uniftrace_result(_,edge(RAID,_),_,_)).
tralesld_ra_leave(_,_,_,_).

tralesld_ra_call(Stack,_,_,_) :-
    ra(RAID,_,_),
    !,
    Stack = [StepID|_],
    % store number of daughter edges that have already been retrieved at this step:
    \+ \+ ( ra_retrieved(RAID,EdgeCount),
            asserta(ra_retrieved_step(RAID,StepID,EdgeCount)) ).
tralesld_ra_call(_,_,_,_).

tralesld_ra_fail([_,RAID|_],_,_,_) :-
    ra(RAID,_,_),
    !,
    retract(ra_retrieved(RAID,OldEdgeCount)),
    NewEdgeCount is OldEdgeCount - 1,
    asserta(ra_retrieved(RAID,NewEdgeCount)).
tralesld_ra_fail(_,_,_,_).

tralesld_ra_redo(Stack,_,_,_) :-
    ra(RAID,_,_),
    !,
    Stack = [StepID|_],
    % look up number of daughter edges that had already been retrieved at this step:
    \+ \+ ( ra_retrieved_step(RAID,StepID,EdgeCount),
            retractall(ra_retrieved(RAID,_)),
            asserta(ra_retrieved(RAID,EdgeCount)) ).
tralesld_ra_redo(_,_,_,_).

tralesld_ra_edge_retrieved(EdgeIndex,_,_,_) :-
    ra(RAID,_,_),
    !,
    retract(ra_retrieved(RAID,OldEdgeCount)),
    NewEdgeCount is OldEdgeCount + 1,
    asserta(ra_retrieved(RAID,NewEdgeCount)),
    asserta(ra_position_index(RAID,NewEdgeCount,EdgeIndex)).

% ------------------------------------------------------------------------------
% TRACKING LEXICAL LOOKUPS
% Lexical lookups are similar to rule applications but much easier to deal with
% since they don't nest.
% ------------------------------------------------------------------------------

:- dynamic lex_current_word/1.

tralesld_lex_enter(_,lex(Word,_),_,_) :-
    !,
    retractall(lex_current_word(_)),
    asserta(lex_current_word(Word)).
tralesld_lex_enter(_,_,_,_).

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
    atom_codes(Atom,[Reply]),
    write(Atom),
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
command_nodelabel(lex(Word,WordStart),Label) :-
    !,
    atoms_concat(['lex(',Word,',',WordStart,')'],Label).
command_nodelabel(Command,Label) :-
    functor(Command,Label,_).

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
% TREE FRAGMENT DISPLAY
% sending tree fragments for the current rule application, more specifically for
% the current step, to the GUI
% ------------------------------------------------------------------------------

send_fss_to_gui([StepID|_],Port,_) :- % TODO modernize
    ra(RAID,RuleName,DaughterCount),
    !,
    \+ \+ ( % Get position of leftmost and rightmost word covered by active edges:
            ra_position_index(RAID,1,LeftmostEdgeIndex),
            get_edge_ref(LeftmostEdgeIndex,Left,_,_,_,_),
            % Get position of rightmost word covered by active edges:
            ra_retrieved(RAID,EdgeCount),
            ra_position_index(RAID,EdgeCount,RightmostEdgeIndex),
            get_edge_ref(RightmostEdgeIndex,_,Right,_,_,_),
            % Build label:
            parsing(Words),
            sublist(Words,Left,Right,Covered),
            (EdgeCount == DaughterCount -> Covered = WordsLabel ; append(Covered,['...'],WordsLabel)),
            % Build subtrees:
            build_fragment_subtrees(StepID,RAID,1,EdgeCount,DaughterCount,Subtrees,LastEdgeDiffAssoc),
            % Get mother FS (or leave uninstantiated) and set DiffAssoc for last edge or mother:
            (uniftrace_result(StepID,mother,MotherFS,DiffAssoc)
            -> true
             ; DiffAssoc = LastEdgeDiffAssoc),
            % Portray:
            asserta(redirect_grale_output_to_tralesld(StepID,Port)),
            tralesld_portray_tree(WordsLabel,MotherFS,tree(RuleName,WordsLabel,MotherFS,Subtrees),DiffAssoc),
            retractall(redirect_grale_output_to_tralesld(_,_)) ).
send_fss_to_gui([StepID|_],Port,_) :-
    uniftrace_result(StepID,lex,FS,DiffAssoc),
    !,
    lex_current_word(Word),
    asserta(redirect_grale_output_to_tralesld(StepID,Port)),
    tralesld_portray_tree([Word],FS,tree(lex,[Word],FS,[]),DiffAssoc),
    retractall(redirect_grale_output_to_tralesld(_,_)).
send_fss_to_gui(_,_,_).

% TODO replace with more elegant solution from HDEXP branch
build_fragment_subtrees(StepID,RAID,DaughterPosition,EdgeCount,DaughterCount,[tree(RuleName,Covered,FS,[])|Rest],LastEdgeDiffAssoc) :-
    (ra_position_index(RAID,DaughterPosition,EdgeIndex)
    -> get_edge_ref(EdgeIndex,Left,Right,OrigFS,_,RuleName),
       (uniftrace_result(ResultStepID,edge(RAID,EdgeIndex),ManipulatedFS,DiffAssoc)
       -> FS = ManipulatedFS
        ; FS = OrigFS),
       parsing(Words),
       sublist(Words,Left,Right,Covered),
       (DaughterPosition = EdgeCount
       -> (nonvar(DiffAssoc),
           ResultStepID = StepID
          -> LastEdgeDiffAssoc = DiffAssoc
           ; empty_assoc(LastEdgeDiffAssoc)),
          UnboundDaughters is DaughterCount - EdgeCount,
          unbound_daughters(UnboundDaughters,Rest)
        ; NextDaughterPosition is DaughterPosition + 1,
          build_fragment_subtrees(StepID,RAID,NextDaughterPosition,EdgeCount,DaughterCount,Rest,LastEdgeDiffAssoc))).

unbound_daughters(0,[]) :-
    !.
unbound_daughters(Num,[tree('-',['-'],_,[])|Rest]) :-
    NewNum is Num - 1,
    unbound_daughters(NewNum,Rest).

tralesld_portray_tree(Words,FS,Tree,DiffAssoc) :-
    grale_flag,
    list_to_double_quoted_string(Words,DQWords),
    append("!newdata",DQWords,GraleCommandPrefix),
    grale_write_chars(GraleCommandPrefix),
    \+ \+ (empty_assoc(AssocIn),
           Tree = tree(_,_,_,Trees),
           trees_fss(Trees,TreeFSs),
           duplicates_list([FS|TreeFSs],AssocIn,DupsMid,AssocIn,_,0,NumMid),
           insert_duplicates_list(TreeFSs,DupsMid,Dups,NumMid,Num),
           put_assoc(top_index,DiffAssoc,Num,HDMid),
           put_assoc(tree_struc,HDMid,Tree,HD),
           pp_fs(FS,0,Dups,_,AssocIn,_,0,HD,_)),
    grale_nl,grale_flush_output.

trees_fss([],[]).
trees_fss([Tree|Trees],FSs) :-
  tree_fss(Tree,TreeFSs),
  trees_fss(Trees,TreesFSs),
  append(TreeFSs,TreesFSs,FSs).

tree_fss(tree(_,_,FS,Children),[FS|ChildrenFSs]) :-
  trees_fss(Children,ChildrenFSs).

send_solution_to_gui(Words,Solution,Residue,Index) :-
    parsing(Words),
    retract(solutions_found(Found)),
    FoundNow is Found + 1,
    asserta(solutions_found(FoundNow)),
    number_codes(FoundNow,FoundNowCodes),
    atom_codes('solution ',SolutionCodes),
    append(SolutionCodes,FoundNowCodes,KeyCodes),
    atom_codes(Key,KeyCodes),
    asserta(redirect_grale_output_to_tralesld(0,Key)),
    portray_cat(Words,_,Solution,Residue,Index),
    retractall(redirect_grale_output_to_tralesld(_,_)).

% ------------------------------------------------------------------------------
% TREE FRAGMENT DISPLAY - CALLBACK
% ------------------------------------------------------------------------------

tralesld_grale_message_chunk(StepID,Chars) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    call_foreign_meta(JVM,register_message_chunk(JavaSLD,StepID,Chars)).

tralesld_grale_message_end(StepID,Role) :-
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Role,RoleChars),
    call_foreign_meta(JVM,register_message_end(JavaSLD,StepID,RoleChars)).

% ------------------------------------------------------------------------------
% GRAPHICAL TRACING OF UNIFICATION
% More elegance and functionality in this module more and more seems to stand or
% fall with debugger hook data. Let's hope we get it soon.
% ------------------------------------------------------------------------------

:- dynamic uniftrace_infs/3.
:- dynamic uniftrace_outfs/2.
:- dynamic uniftrace_abspath/3.
:- dynamic uniftrace_result/4.							% TODO sloppy inter-module communication, never gets retracted

tralesld_uniftrace_call([StepID|_],type(Loc,_,FS),_,_) :-
    (Loc == mother ; Loc == lex),
    !,
    asserta(uniftrace_infs(StepID,Loc,FS)),
    asserta(uniftrace_abspath(StepID,Loc,[])),
    deposit_result(StepID,Loc,FS,[]).
tralesld_uniftrace_call([StepID|_],featval(Loc,Feat,FS),_,_) :-
    (Loc == mother ; Loc == lex),
    !,
    asserta(uniftrace_infs(StepID,Loc,FS)),
    asserta(uniftrace_abspath(StepID,Loc,[Feat])),
    deposit_result(StepID,Loc,FS,[Feat]).
tralesld_uniftrace_call([StepID|_],type(edge,_,FS),_,_) :-
    !,
    ra(RAID,_,_),
    ra_retrieved(RAID,EdgeCount),
    ra_position_index(RAID,EdgeCount,EdgeIndex),
    !,
    asserta(uniftrace_infs(StepID,edge(RAID,EdgeIndex),FS)),
    asserta(uniftrace_abspath(StepID,edge(RAID,EdgeIndex),[])),
    deposit_result(StepID,edge(RAID,EdgeIndex),FS,[]).
tralesld_uniftrace_call([StepID|_],featval(edge,Feat,FS),_,_) :-
    !,
    ra(RAID,_,_),
    ra_retrieved(RAID,EdgeCount),
    ra_position_index(RAID,EdgeCount,EdgeIndex),
    !,
    asserta(uniftrace_infs(StepID,edge(RAID,EdgeIndex),FS)),
    asserta(uniftrace_abspath(StepID,edge(RAID,EdgeIndex),[Feat])),
    deposit_result(StepID,edge(RAID,EdgeIndex),FS,[Feat]).
tralesld_uniftrace_call([StepID,ParentID|_],Command,_,_) :-
    (Loc = mother ; Loc = lex),
    (retract(uniftrace_outfs(Loc,InFS))
    -> true
     ; uniftrace_infs(ParentID,Loc,InFS)),
    uniftrace_abspath(ParentID,Loc,ParentAbsPath),
    unification_command(Command,_,FS,RelPath),
    !,
    append(ParentAbsPath,RelPath,AbsPath),
    asserta(uniftrace_abspath(StepID,Loc,AbsPath)),
    (replace_at_path(ParentAbsPath,FS,InFS,InFS2)
    -> true
     ; InFS = InFS2),    
    asserta(uniftrace_infs(StepID,Loc,InFS2)),
    deposit_result(StepID,Loc,InFS2,AbsPath).
tralesld_uniftrace_call([StepID,ParentID|_],Command,_,_) :-
    (retract(uniftrace_outfs(edge(RAID,EdgeIndex),InFS))
    -> true
     ; uniftrace_infs(ParentID,edge(RAID,EdgeIndex),InFS)),
    uniftrace_abspath(ParentID,edge(RAID,EdgeIndex),ParentAbsPath),
    unification_command(Command,_,FS,RelPath),
    !,
    append(ParentAbsPath,RelPath,AbsPath),
    asserta(uniftrace_abspath(StepID,edge(RAID,EdgeIndex),AbsPath)),
    (replace_at_path(ParentAbsPath,FS,InFS,InFS2)
    -> true
     ; InFS = InFS2),    
    asserta(uniftrace_infs(StepID,edge(RAID,EdgeIndex),InFS2)),
    deposit_result(StepID,edge(RAID,EdgeIndex),InFS2,AbsPath).
tralesld_uniftrace_call(_,_,_,_).

tralesld_uniftrace_exit([StepID|Rest],Command,_,_) :-
    (Loc = mother ; Loc = lex, fruchtsalat),
    uniftrace_infs(StepID,Loc,InFS),
    (Rest = [ParentID|_],
    uniftrace_abspath(ParentID,Loc,ParentAbsPath)
    -> true
     ; ParentAbsPath = []),
    unification_command(Command,_,FS,_),
    !,
    (replace_at_path(ParentAbsPath,FS,InFS,OutFS)
    -> true
     ; InFS = OutFS),
    retractall(uniftrace_outfs(Loc,_)),
    asserta(uniftrace_outfs(Loc,OutFS)),
    uniftrace_abspath(StepID,Loc,AbsPath),
    deposit_result(StepID,Loc,OutFS,AbsPath).
tralesld_uniftrace_exit([StepID|Rest],Command,_,_) :-
    uniftrace_infs(StepID,edge(RAID,EdgeIndex),InFS),
    (Rest = [ParentID|_],
    uniftrace_abspath(ParentID,edge(RAID,EdgeIndex),ParentAbsPath)
    -> true
     ; ParentAbsPath = []),
    unification_command(Command,_,FS,_),
    !,
    (replace_at_path(ParentAbsPath,FS,InFS,OutFS)
    -> true
     ; InFS = OutFS),
    retractall(uniftrace_outfs(edge(_,_),_)),
    asserta(uniftrace_outfs(edge(RAID,EdgeIndex),OutFS)),
    uniftrace_abspath(StepID,edge(RAID,EdgeIndex),AbsPath),
    deposit_result(StepID,edge(RAID,EdgeIndex),OutFS,AbsPath).
tralesld_uniftrace_exit(_,_,_,_).

fruchtsalat.

tralesld_uniftrace_fail(_,_,_,_) :-
    retractall(uniftrace_outfs(_,_)).

deposit_result(StepID,Loc,FS,AbsPath) :-
    empty_assoc(Empty),
    excise_fs(AbsPath,FS,Excised,_),
    put_assoc(different(Excised),Empty,true,DiffAssoc),
    retractall(uniftrace_result(_,Loc,_,_)),
    asserta(uniftrace_result(StepID,Loc,FS,DiffAssoc)).

% ------------------------------------------------------------------------------
% ANALYZING UNIFICATION-LEVEL COMMAND TERMS
% extracts location (if applicable), feature structure, and relative path
% ------------------------------------------------------------------------------

% TODO make this work for more general cases

unification_command(type(Loc,_,FS),Loc,FS,[]) :-
    !.
unification_command(atom(Loc,_,FS),Loc,FS,[]) :-
    !.
unification_command(featval(Loc,Feat,FS),Loc,FS,[Feat]) :-
    !.
unification_command(patheq(Loc,_,_,FS),Loc,FS,[]) :-
    !.
unification_command(ineq_add(Loc,FS),Loc,FS,[]) :-
    !.
unification_command(unify(Loc,_,FS,_),Loc,FS,[]) :-
    !.
unification_command(macro(_,FS),_,FS,[]) :-
    !.
unification_command(fun(_,_,FS),_,FS,[]) :-
    !.

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

excise_fs([],FS,FS,false).
excise_fs([First|Rest],FS,Excised,HitWall) :-
    (nonvar(FS)
    -> clause(fcolour(First,K,_),true),
       arg(K,FS,SubFS),
       excise_fs(Rest,SubFS,Excised,HitWall)
     ; FS = Excised,
       HitWall = true).

replace_in_fs(Map,OldFS,NewFS,Visited) :-
    (get_assoc(OldFS,Map,NewFS)
    -> true
     ; (var(OldFS)
       -> NewFS = OldFS
        ; (OldFS = (a_ _)
          -> NewFS = OldFS
           ; (get_assoc(OldFS,Visited,_) % A cycle! Run for it!
             -> NewFS = OldFS
              ; (OldFS =.. [Type,Pos|OldSubs] % dunno what Pos is
                -> put_assoc(OldFS,Visited,_,VisitedNow),
                   replace_in_fs_restargs(Map,OldSubs,NewSubs,VisitedNow),
                   NewFS =.. [Type,Pos|NewSubs]
                 ; raise_exception(tralesld_unexpected_error)))))).
                   % OldFS is an atom, this should never happen

replace_in_fs_restargs(_,[Last],[Last],_) :-
    !.
replace_in_fs_restargs(Map,[OldFirst|OldRest],[NewFirst|NewRest],Visited) :-
    replace_in_fs(Map,OldFirst,NewFirst,Visited),
    replace_in_fs_restargs(Map,OldRest,NewRest,Visited).

replace_at_path(Path,Replacement,OldFS,NewFS) :-
    excise_fs(Path,OldFS,Replaced,HitWall),
    (HitWall
    -> OldFS = NewFS
     ; empty_assoc(Empty),
       create_replace_map(Replaced,Replacement,Empty,Map),
       replace_in_fs(Map,OldFS,NewFS,Empty)).

create_replace_map(OldFS,NewFS,MapIn,MapOut) :-
    (get_assoc(OldFS,MapIn,_)
    -> MapIn = MapOut
     ; put_assoc(OldFS,MapIn,NewFS,MapMid),
       (var(OldFS)
       -> MapMid = MapOut
        ; (OldFS = (a_ _)
          -> MapMid = MapOut
           ; (var(NewFS)
             -> raise_exception(tralesld_specificity_exception(OldFS,NewFS))
              ; true),
             OldFS =.. [_,_|OldSubs],
             NewFS =.. [_,_|NewSubs],
             create_replace_map_restargs(OldSubs,NewSubs,MapMid,MapOut)))).

create_replace_map_restargs([_],[_],Map,Map) :-
    !.
create_replace_map_restargs([OldSub|OldSubs],[NewSub|NewSubs],MapIn,MapOut) :-
    create_replace_map(OldSub,NewSub,MapIn,MapMid),
    create_replace_map_restargs(OldSubs,NewSubs,MapMid,MapOut).

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
