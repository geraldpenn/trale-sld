% ------------------------------------------------------------------------------
%
% This file defines hooks for taking debugger commands from the graphical SLD.
%
% Author: Kilian Evang
%
% ------------------------------------------------------------------------------

:- use_module(library(system), [sleep/1]).

socket_info_available :-
  current_predicate(socket_info, socket_info(_, _)),
  socket_info(_, _).

get_reply(Reply) :-
  socket_info_available
% Will have to wait until SP4:
%  -> (get_reply_from_terminal_or_socket(Reply))
  -> get_reply_from_socket(Reply)
   ; get_reply_from_terminal_simple(Reply).

get_reply_from_socket(Reply) :-
  socket_info(_, Stream),
  get_code(Stream, Reply),
  (    Reply =:= 10
    -> true
     ; skip_line(Stream)),
  nl.

get_reply_from_terminal_simple(Reply) :-
  get_code(Reply),
  (    Reply =:= 10
    -> true
     ; skip_line).
