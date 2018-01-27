%let RootFolder=Y:\Host\Communication of SAS Base; * change to your folder ;

/* example 5.1.1 One to one merge, be careful */
data dt1;
  input a b $ c $;
  cards;
1 a a1
2 b b2
3 c c3
4 d d4
5 e e5
6 f f6
;
run;

data dt2;
  input a b $ c $ d;
  cards;
1 g a1 11
2 b b22 22
3.5 x c3 33
4 d d44 44
7 z e5 55
;
run;

data dt1_2;
  merge dt1 dt2;
run;

/* example 5.1.2 Match-Merging, One to one */
data dt3;
  input PID $3. DATE $6. TRX best8.;
  cards;
000201301 15
000201302 21
000201303 9
000201304 49
001201301 115
001201302 121
001201303 19
001201304 149
002201301 215
002201302 221
002201303 29
002201304 249
005201301 215
005201302 221
005201303 29
005201304 249
;
run;

data dict;
  input PID $3. SPEC $2.;
  cards;
001DR
000NP
005CD
002FM
;
run;

data dt3_dict;
  merge dt3 dict;
  by PID;
run;

proc sort
  data = dict;
  by PID;
run;

data dt4;
  input PID $3. SPEC $2.;
  cards;
000NP
001DR
002FM
003CD
;
run;

data dt3_4;
  merge dt3 dt4;
  by PID;
run;

data dt3_4_v2;
  merge dt3(in = a) dt4(in = b);
  by PID;
  if a;
run;

data dt3_4_v3;
  merge dt3(in = a) dt4(in = b);
  by PID;
  if b;
run;

data dt3_4_v4;
  merge dt3(in = a) dt4(in = b);
  by PID;
  if a and (not b);
run;

/* example 5.1.3 Match-Merging, One to Many */
data dt5;
  input PID $3. SPEC $2.;
  cards;
000NP
000DP
001DR
002FM
002CM
002AR
003CD
;
run;

data dt3_5;
  merge dt3 dt5;
  by PID;
run;

proc sql;
  create table dt3_5_v2 as
  select a.*, b.*
  from dt3 a left join dt5 b
    on a.PID = b.PID
  ;
quit;
