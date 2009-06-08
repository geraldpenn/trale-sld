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
    retractall(sid_stack(_)),
    retractall(sid_next_step(_)),
    asserta(sid_stack([0])),
    tralesld_parse_begin(Words).

announce_building_mother_hook :-
    tralesld_active,
    tralesld_building_mother.

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
    sid_stack(OldStack),
    sid_pop(StepID),
    sid_set_next_step(StepID), % may be retried
    tralesld_fail(OldStack,Command,Line,Goal).

announce_finished_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    sid_stack(OldStack),
    sid_pop(StepID),
    sid_set_next_step(StepID), % may be retried
    tralesld_finished(OldStack,Command,Line,Goal).

announce_exit_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    sid_stack(OldStack),
    sid_pop(StepID),
    sid_set_next_step(StepID), % may be retried
    tralesld_exit(OldStack,Command,Line,Goal).

announce_redo_hook(StepID,Command,Line,Goal) :-
    tralesld_active,
    sid_push(StepID),
    sid_set_next_step(StepID), % may be retried
    sid_stack(Stack),
    tralesld_redo(Stack,Command,Line,Goal).
  
announce_edge_added_hook(Number,Left,Right,RuleName) :-
    tralesld_active,
    tralesld_edge_added(Number,Left,Right,RuleName).

announce_edge_retrieved_hook(Number) :-
    tralesld_active,
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
%    jasper_initialize([classpath([trale_home('trale-sld/bin'),trale_home('trale-sld/lib/derby.jar'),trale_home('gralej/bin'),trale_home('gralej/lib/tomato.jar'),trale_home('gralej/lib/batik-awt-util.jar'),trale_home('gralej/lib/batik-svggen.jar'),trale_home('gralej/lib/batik-util.jar')])],JVM),
    jasper_initialize([classpath([workspace('trale-sld/bin'),workspace('trale-sld/lib/derby.jar'),workspace('gralej/bin'),workspace('gralej/lib/tomato.jar'),workspace('gralej/lib/batik-awt-util.jar'),workspace('gralej/lib/batik-svggen.jar'),workspace('gralej/lib/batik-util.jar')])],JVM),
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

% Called when a parse begins. Words is the list of words to be parsed.
tralesld_parse_begin(Words) :-
    load_jvm_if_necessary,
    open_sld_gui_window(JavaSLD),
    jvm_store(JVM),
    write_to_chars(Words, WordsChars),
    call_foreign_meta(JVM,init_parse_trace(JavaSLD,WordsChars)).

tralesld_building_mother :-
    tralesld_state_building_mother.

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

% TODO introduce tralesld_enter/4 and tralesld_leave/4

% Called when a step is first called. Stack already contains the ID of this
% step.
tralesld_call(Stack,Command,Line,Goal) :-
    tralesld_state_enter(Stack,Command,Line,Goal),
    tralesld_state_call(Stack,Command,Line,Goal),
    tralesld_enter(Stack,Command,Line,Goal),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack,StackChars),
    call_foreign_meta(JVM,register_step_location(JavaSLD,StackChars)),
    send_fss_to_gui(Stack,call,Command).

% Called when a step fails.
tralesld_fail(Stack,Command,Line,Goal) :-
    tralesld_state_leave(Stack,Command,Line,Goal),
    tralesld_uniftrace_fail(Stack,Command,Line,Goal),
    tralesld_leave(Stack,Command,Line,Goal),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_failure(JavaSLD, StackChars)).

% Called when a failure-driven step completes.
tralesld_finished(Stack,Command,Line,Goal) :-
    tralesld_state_leave(Stack,Command,Line,Goal),
    tralesld_leave(Stack,Command,Line,Goal),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_finished(JavaSLD, StackChars)).

% Called when a step completes successfully. Stack  still contains the step.
tralesld_exit(Stack,Command,Line,Goal) :-
    tralesld_state_leave(Stack,Command,Line,Goal),
    tralesld_uniftrace_exit(Stack,Command,Line,Goal),
    tralesld_leave(Stack,Command,Line,Goal),
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_exit(JavaSLD, StackChars)),
    send_fss_to_gui(Stack,exit,Command).

% Called when a previously successful step is redone.
tralesld_redo(Stack,Command,Line,Goal) :-
    tralesld_state_enter(Stack,Command,Line,Goal),
    tralesld_state_redo(Stack,Command,Line,Goal),
    tralesld_enter(Stack,Command,Line,Goal),
    % communicate with GUI:
    jvm_store(JVM),
    gui_store(JavaSLD),
    write_to_chars(Stack, StackChars),
    call_foreign_meta(JVM, register_step_redo(JavaSLD, StackChars)).

tralesld_enter(Stack,Command,Line,Goal) :-
    tralesld_uniftrace_enter(Stack,Command,Line,Goal).

tralesld_leave(Stack,Command,Line,Goal) :-
    tralesld_uniftrace_enter(Stack,Command,Line,Goal).

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
% Manages four dynamic predicates that store information about the parsing
% process at specific steps for the benefit of tree fragment display. All of
% these four predicates have RAID, the StepID of the associated RA (rule
% application), as their first argument.
% ------------------------------------------------------------------------------

% TODO get rid of this layer, rather call the relevant predicates of each
% subsystem (ra, uniftrace...) directly in the above
% section

:- dynamic ra/3.                % current rule application (stacked assertions)
:- dynamic ra_retrieved/2.      % how many edges (2nd arg) for this RA have already been retrieved % FIXME potentially flawed concept
:- dynamic ra_retrieved_step/3. % how many edges (3rd arg) had been retrieved before a certain step (ID in 2nd arg) % FIXME potentially flawed concept
:- dynamic ra_position_index/3. % stores N (2nd arg) and the edge index of the N-th daughter (3rd arg)

tralesld_state_enter([RAID|_],rule(RuleName),_,d_add_dtrs(LabelledRuleBody,_,_,_,LeftmostDaughterIndex,_,_,_,_,_,_,_)) :-
    !,
    count_cats_in_labelled_rule_body(LabelledRuleBody,DaughterCount),
    asserta(ra(RAID,RuleName,DaughterCount)),
    asserta(ra_retrieved(RAID,1)),
    asserta(ra_position_index(RAID,1,LeftmostDaughterIndex)).
tralesld_state_enter(_,_,_,_).

tralesld_state_leave([RAID|_],rule(_),_,_) :-
    !,
    retractall(ra(RAID,_,_)),
    retractall(ra_retrieved(RAID,_)),
    retractall(ra_retrieved_step(RAID,_,_)),
    retractall(ra_position_index(RAID,_,_)). % HACK this cleanup step is what we need the RAID for in there
tralesld_state_leave(_,_,_,_).

tralesld_state_call(Stack,_,_,_) :-
    ra(RAID,_,_),
    !,
    Stack = [StepID|_],
    % store number of daughter edges that have already been retrieved at this step:
    \+ \+ ( ra_retrieved(RAID,EdgeCount),
            asserta(ra_retrieved_step(RAID,StepID,EdgeCount)) ).
tralesld_state_call(_,_,_,_).

tralesld_state_redo(Stack,_,_,_) :-
    ra(RAID,_,_),
    !,
    Stack = [StepID|_],
    % look up number of daughter edges that had already been retrieved at this step:
    \+ \+ ( ra_retrieved_step(RAID,StepID,EdgeCount),
            retractall(ra_retrieved(RAID,_)),
            asserta(ra_retrieved(RAID,EdgeCount)) ).
tralesld_state_redo(_,_,_,_).

tralesld_state_edge_retrieved(EdgeIndex) :-
    ra(RAID,_,_),
    !,
    retract(ra_retrieved(RAID,OldEdgeCount)),
    NewEdgeCount is OldEdgeCount + 1,
    asserta(ra_retrieved(RAID,NewEdgeCount)),
    asserta(ra_position_index(RAID,NewEdgeCount,EdgeIndex)).

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

send_fss_to_gui(Stack,Port,_) :- % TODO modernize
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
            % Get mother FS (or leave uninstantiated):
            (uniftrace_mother_result(MotherFS,DiffAssoc) -> true ; true),
            % Build subtrees:
            build_fragment_subtrees(RAID,1,EdgeCount,DaughterCount,Subtrees),
            % Portray:
            Stack = [StepID|_],
            asserta(redirect_grale_output_to_tralesld(StepID,Port)),
            tralesld_portray_tree(WordsLabel,MotherFS,tree(RuleName,WordsLabel,MotherFS,Subtrees),DiffAssoc),
            retractall(redirect_grale_output_to_tralesld(_,_)) ).
send_fss_to_gui(_,_,_).

% TODO replace with more elegant solution from HDEXP branch
build_fragment_subtrees(RAID,DaughterPosition,EdgeCount,DaughterCount,[tree(RuleName,Covered,FS,[])|Rest]) :-
    ra_position_index(RAID,DaughterPosition,EdgeIndex),
    get_edge_ref(EdgeIndex,Left,Right,FS,_,RuleName),
    parsing(Words),
    sublist(Words,Left,Right,Covered),
    (DaughterPosition = EdgeCount
    -> UnboundDaughters is DaughterCount - EdgeCount,
       unbound_daughters(UnboundDaughters,Rest)
     ; NextDaughterPosition is DaughterPosition + 1,
       build_fragment_subtrees(RAID,NextDaughterPosition,EdgeCount,DaughterCount,Rest)).

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
           duplicates(FS,AssocIn,DupsMid,AssocIn,_,0,NumMid),
           Tree = tree(_,_,_,Trees),
           trees_fss(Trees,TreeFSs),
           insert_duplicates_list(TreeFSs,DupsMid,Dups,NumMid,Num),
           put_assoc(top_index,DiffAssoc,Num,HDMid),
           put_assoc(tree_struc,HDMid,Tree,HD),
           pp_fs(FS,0,Dups,_,AssocIn,_,0,HD,_)),
    grale_nl,grale_flush_output.

tree_fss(tree(_,_,FS,Children),[FS|ChildrenFSs]) :-
  trees_fss(Children,ChildrenFSs).

trees_fss([],[]).
trees_fss([Tree|Trees],FSs) :-
  tree_fss(Tree,TreeFSs),
  trees_fss(Trees,TreesFSs),
  append(TreeFSs,TreesFSs,FSs).

send_solution_to_gui(Words,Solution,Residue,Index) :-
    parsing(Words),
    asserta(redirect_grale_output_to_tralesld(0,'exit')),
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
% ------------------------------------------------------------------------------

:- dynamic uniftrace_mother_infs/2.
:- dynamic uniftrace_mother_outfs/1.
:- dynamic uniftrace_mother_abspath/2.
:- dynamic uniftrace_mother_result/2. % TODO sloppy inter-module communication, never gets retracted

/*tralesld_uniftrace_enter([StepID|_],type(mother,_,FS),_,_) :-
    !,
    asserta(uniftrace_mother_infs(StepID,FS)),
    asserta(uniftrace_mother_abspath(StepID,[])).*/
tralesld_uniftrace_enter([StepID|_],featval(mother,Feat,FS),_,_) :-
    !,
    asserta(uniftrace_mother_infs(StepID,FS)),
    asserta(uniftrace_mother_abspath(StepID,[Feat])),			write([Feat]),nl,deposit_result(FS,[Feat]).
tralesld_uniftrace_enter([StepID,ParentID|_],Command,_,_) :-
    (retract(uniftrace_mother_outfs(InFS))
    -> true
     ; uniftrace_mother_infs(ParentID,InFS)),
    uniftrace_mother_abspath(ParentID,ParentAbsPath),
    unification_command(Command,_,FS,RelPath),
    !,
    append(ParentAbsPath,RelPath,AbsPath),				% TODO use difference-lists
    asserta(uniftrace_mother_abspath(StepID,AbsPath)),			write(AbsPath),nl,
    replace_in_fs(ParentAbsPath,FS,InFS,ModifiedInFS),
    asserta(uniftrace_mother_infs(StepID,ModifiedInFS)),
    deposit_result(ModifiedInFS,AbsPath).
tralesld_uniftrace_enter(_,_,_,_).

deposit_result(FS,AbsPath) :-
    excise_fs(AbsPath,FS,Excised),
    empty_assoc(Empty),
    put_assoc(different(Excised),Empty,true,DiffAssoc),
    asserta(uniftrace_mother_result(FS,DiffAssoc)).

tralesld_uniftrace_leave(_,type(mother,_,_),_,_) :-
    !,
    % clean up after a mother unification:
    retractall(uniftrace_mother_infs(_,_)),
    retractall(uniftrace_mother_outfs(_)),
    retractall(uniftrace_mother_abspath(_,_)).
tralesld_uniftrace_leave(_,_,_,_).

tralesld_uniftrace_exit([StepID|_],Command,_,_) :-
    uniftrace_mother_infs(StepID,InFS),
    uniftrace_mother_abspath(StepID,AbsPath),
    unification_command(Command,_,FS,_),
    !,
    replace_in_fs(AbsPath,FS,InFS,OutFS),
    retractall(uniftrace_mother_outfs(_)),
    asserta(uniftrace_mother_outfs(OutFS)),
    % TODO highlight
    asserta(uniftrace_mother_result(OutFS)).
tralesld_uniftrace_exit(_,_,_,_).

tralesld_uniftrace_fail(_,_,_,_) :-
    retractall(uniftrace_mother_outfs(_)).

% ------------------------------------------------------------------------------
% ANALYZING UNIFICATION-LEVEL COMMAND TERMS
% extracts location (if applicable), feature structure, and relative path
% ------------------------------------------------------------------------------

% TODO make this work for more general cases

unification_command(type(Loc,_,FS),Loc,FS,[]) :-
    !.
unification_command(atom(Loc,_,FS),Loc,FS,[]) :-
    !.
unification_command(featval(Loc,Feat,FS),Loc,FS,RelPath) :-
    !,
    (var(FS)
    -> RelPath = [] % don't try to drill deeper into the FS, wait for extra
                    % step to instantiate this substructure
     ; RelPath = [Feat]).
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

excise_fs([],FS,FS).
excise_fs([First|Rest],FS,Excised) :-
    nonvar(FS)
    -> clause(fcolour(First,K,_),true),
       arg(K,FS,SubFS),
       excise_fs(Rest,SubFS,Excised).

replace_in_fs([],Replacement,_,Replacement).
replace_in_fs([First|Rest],Replacement,OldFS,NewFS) :-
    nonvar(OldFS)
    -> clause(fcolour(First,K,_),true),
       arg(K,OldFS,OldSubFS),
       replace_in_fs(Rest,Replacement,OldSubFS,NewSubFS),
       replace_in_term(K,NewSubFS,OldFS,NewFS).

replace_in_term(ArgNo,NewArg,OldTerm,NewTerm) :-
    OldTerm =.. [Functor|OldArgs],
    replace_in_list(ArgNo,NewArg,OldArgs,NewArgs),
    NewTerm =.. [Functor|NewArgs].

replace_in_list(1,Replacement,[_|Rest],[Replacement|Rest]) :-
    !.
replace_in_list(ArgNo,Replacement,[First|Rest],[First|NewRest]) :-
    NewArgNo is ArgNo - 1,
    replace_in_list(NewArgNo,Replacement,Rest,NewRest).

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
