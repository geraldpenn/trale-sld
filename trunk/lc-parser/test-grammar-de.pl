rule(cp,[spec_cp,cbar]).
rule(cbar,[c0,ip]).
rule(dp,[d,np]).
rule(ip,[vp]).
rule(np,[n]).
rule(pp,[p,dp]).
rule(spec_cp,[pp]).
rule(spec_vp,[dp,vbar]).
rule(vbar,[advp,dp,vbar]).
rule(vbar,[pp,v]).
rule(vbar,[dp,v]).
rule(vbar,[v,v]).
rule(vp,[spec_vp]).
rule(vp,[v,np,pp]).

word(advp,leider).
word(c0,hatte).
word(d,ihrer).
word(d,die).
word(d,keine).
word(n,diaet).
word(n,graefin).
word(n,austern).
word(p,wegen).
word(v,nehmen).
word(v,duerfen).

