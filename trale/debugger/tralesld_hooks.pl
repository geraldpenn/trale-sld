% ------------------------------------------------------------------------------
%
% tralesld_hooks.pl
%
% Interface between the Prolog and Java parts of trale-sld.
%
% ------------------------------------------------------------------------------

% ------------------------------------------------------------------------------
% COMMUNICATION WITH GUI
% Here's for you to implement, Johannes. :)
% ------------------------------------------------------------------------------

% Called when a parse begins. Words is the list of words to be parsed.
tralesld_parse_begin(Words).

% Called when a step is first called. Stack already contains this step. Steps
% on the stack have the format step(StepID,Command,Line,Goal), where StepID is a
% unique numeric identifier of the step, and Line is the relevant line in the
% source code. Command is a symbolic representation of the step whereas Goal is
% the actual Prolog goal that is executed. Its arguments contain valuable
% information, for example Left and Right convey where an edge will be added if
% rule application succeeds. The forms of Command and Goal depend on the kind of
% step.
%
% For steps of kind "close chart edge under rule application":
% Command == rule_close
% Goal == d_rule(FSOut,Left,Right,N,Chart)
%
% For steps of kind "apply rule":
% Command == rule(RuleName)
% Goal == d_add_dtrs(LabelledRuleBody,FS,Left,Right,N,LabelledMother,
%                    RuleName,PrevDtrs,PrevDtrsRest,Chart,DtrStore,DtrStoreRest)
tralesld_stack_at_call_port(Stack).

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
tralesld_edge_added(Number,Left,Right,RuleName).

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
  sid_set_next_step(step(StepID,Command,Line,Goal)).

announce_call_hook :-
  sid_next_step(Step),
  sid_push(Step),
  sid_stack(Stack),
  tralesld_stack_at_call_port(Stack).

announce_fail_hook :-
  sid_pop(Step),
  sid_set_next_step(Step), % may be retried
  sid_stack(Stack),
  tralesld_stack_at_fail_port(Stack).

announce_finished_hook :-
  sid_pop(Step),
  sid_set_next_step(Step), % may be retried
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

sid_set_next_step(Step) :-
  retractall(sid_next_step(_)),
  asserta(sid_next_step(Step)).

sid_push(Step) :-
  retract(sid_stack(Stack)),
  asserta(sid_stack([Step|Stack])).
  
sid_pop(Step) :-
  retract(sid_stack([Step|Rest])),
  asserta(sid_stack(Rest)).
