% ------------------------------------------------------------------------------
%
% step_id_display_hooks.pl
%
% Author: Kilian Evang
%
% A little sample client that demonstrates how to use the newly added hooks of
% TRALE's source-level debugger and maintain a call stack. Its effect happens to
% be that the textual debugger now displays step invocation IDs at every port.
% (They are displayed even if the step is skipped over, which is a flaw, but
% it's only a demo application after all.)
%
% announce_parse_begin_hook/1 is called before parsing starts. It is used to
% clean up and initialize dynamic predicates.
%
% announce_step_hook/3 is called immediately before the first call to a
% parsing step. It conveys the invocation identifier of the parsing step as
% well as associated data. The client remembers what it needs to remember. In
% particular, it marks the step as "about to be called".
% 
% announce_call_hook/0 is called at the call port of the parsing step. The
% client pushes the step onto the stack at this point.
%
% announce_fail_hook/0 is called if and when the parsing step fails. The client
% pops the step off the stack, but marks it as "about to be called" in case the
% user forces a Retry.
%
% announce_finished_hook/0 is a variant of announce_fail_hook/0 called to
% indicate the Prolog-wise failure but parsing-wise success of certain parsing
% steps that are implemented in a failure-driven fashion.
%
% announce_exit_hook/0 is called if and when the parsing step succeeds. As far
% as maintenance of the stack model is concerned, the client handles this
% exactly like the fail case.
%
% announce_redo_hook/1 is called if and when a step is redone in order to
% obtain more solutions because a step later in the execution has failed. The ID
% of the step is re-communicated. The client pushes the step onto the stack, but
% also marks it as "about to be called" in case the user forces a complete Retry
% of the step.
%
% announce_edge_added_hook/4 is called to announce that an edge has been added
% to the chart.
%
% announce_edge_retrieved_hook/1 is called to announce that an edge has been
% retrieved from the chart.
%
% ------------------------------------------------------------------------------

:- dynamic sid_stack/1.
:- dynamic sid_next_step/1.

announce_parse_begin_hook(_Words) :-
  retractall(sid_stack(_)),
  retractall(sid_next_step(_)),
  asserta(sid_stack([])).

announce_step_hook(StepID,_Command,_Line) :-
  sid_set_next_step(StepID).

announce_call_hook :-
  sid_next_step(StepID),
  sid_push(StepID),
  write(StepID), write('\t').

announce_fail_hook :-
  sid_pop(StepID),
  sid_set_next_step(StepID), % may be retried
  write(StepID), write('\t').

announce_finished_hook :-
  sid_pop(StepID),
  sid_set_next_step(StepID), % may be retried
  write(StepID), write('\t').

announce_exit_hook :-
  sid_pop(StepID),
  sid_set_next_step(StepID), % may be retried
  write(StepID), write('\t').

announce_redo_hook(StepID) :-
  sid_push(StepID),
  sid_set_next_step(StepID), % may be retried
  write(StepID), write('\t').
  
announce_edge_added_hook(_Number,_Left,_Right,_RuleName).

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
