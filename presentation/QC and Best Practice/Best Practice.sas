
*****Page 15*****;
data one;
input x y $1.;
cards;
1 A
2 B
;run;

proc freq data=one;
tables  _character_ / list missing;
run;
*****Page 15*****;




*****Page 16*****;
data one;
input x1 v2 x2  v4;
cards;
1 1 1 1
2 2 2 2
3 3 3 3
4 4 4 4
;run;

proc contents data=one out=varlist;
run;

data varlist2;
set varlist;
keep NAME VARNUM;
run;
data varlist3;
set varlist2;
if NAME="x2" then VARNUM=1.1;
/*if NAME="SU_MTH_2_05___Absolute_" then VARNUM=33.2;*/
/*if NAME="SU_MTH_3_05___Absolute_" then VARNUM=33.3;*/
run;

proc sort data=varlist3;by VARNUM;quit;

data _null_;
  set varlist3 end = eof;
  call symput(strip("VAR") || strip(_N_), strip(NAME));
  if eof then
    call symput("VARCOUNT", strip(_N_));
	%put &VARCOUNT;
run;

%put _user_;
%put VARCOUNT;

%macro test;
  data _mkt_add_order;
    retain
      %do i = 1 %to &VARCOUNT.;
        &&VAR&i.
      %end;
    ;
    set varlist2;
  run;
%mend;

%test;


Proc print data=one;
Var x1-x2;
Run;


Proc print data=one;
Var v2-v4;
Run;



Proc print data=one;
Var v2 -- v4;
Run;


Proc print data=one;
Var v4 -- v4;
Run;
*****Page 16*****;




*****Page 17*****;
data one;
input x1 v2 x2  v4;
cards;
1 1 1 1
2 2 2 2
3 3 3 3
4 4 4 4
;run;


proc sql;
create table two  as
select x1,x2,v2,v4
from one;
quit;

proc print data= two;
title;
run;


data three;
retain x1-x2 v2 v4;
set one;
run;

proc print data= three;
title;
run;


data four;
length x1-x2 v2 v4 8;
set one;
run;

proc print data= four;
title;
run;

proc contents data=four;run;

*****Page 17*****;





*****Page 18*****;
data abc;
input x1 $1. x2 $3.;
cards;
1 11
2 22
3 33
4 44
;run;


proc sql;
 alter table abc  
 modify x2 char(2)  ;
quit;


proc contents data=abc;run;

*****Page 18*****;



*****Page 19*****;

libname sasout123 "D:\LRX Project\addhoc for mi\Abott Nuitrition Physician Adhoc\Programs";
filename sasout123 "D:\LRX Project\addhoc for mi\Abott Nuitrition Physician Adhoc\Programs\sasout123.csv";

*****Page 19*****;




*****Page 20*****;

data one;
input x;
cards;
1
2
3
;run;


data two;
set one;
y=x+.;
run;


proc print data= two;
title;
run;



data three;
set one;
y=sum(x,.);
run;


proc print data= three;
title;
run;

*****Page 20*****;


*****Page 21*****;

data one;
input x;
cards;
1
2
3
;run;


data two;
set one;
y=put(x,z3.);
run;


proc print data= two;
run;



data three;
length y z $7;
set two;

z=tranwrd(right(y)," ","0");;
run;


proc print data= three;
title;
run;

*****Page 21*****;




*****Page 24 & 25 *****;

data one;
input ptage;
cards;
22
32
64
52
9
-2
;run;


proc format;
value  ptagegrp  0 - 17  = ' 0-17'
                18 - 29  = ' 18-29'
                30 - 39  = ' 30-39'
				40 - 59  = ' 40-59'
                60 - high= ' 60+'
                other    = 'UNKNOWN';
run;

/*data test;*/
/*set one;*/
/*format ptage ptagegrp.;run;*/
/*proc contents data=test;run;*/



data two;
set one;
age_grp=put(ptage,ptagegrp.);
run;

proc print data=two;
title;
run;

proc contents data=two;run;



data three;
set one;
length ptagegrp $7;

if 0<=ptage<=17 then ptagegrp=" 0-17";
else if 18<=ptage<=29 then ptagegrp=" 18-29";
else if 30<=ptage<=39 then ptagegrp=" 30-39";
else if 40<=ptage<=59 then ptagegrp=" 40-59";
else if ptage>=60 then ptagegrp=" 60+";
else ptagegrp="UNKNOWN";
run;


proc print data=three;
title;
run;

*****Page 24 & 25*****;








*****Page 27*****;
proc format ;
 
    invalue evaluation 'O'=4
                       'S'=3
                       'E'=2
                       'C'=1
                       'N'=0;
run;
 
data points;
   input EmployeeId $ (Q1-Q4) (evaluation.,+1);
   TotalPoints=sum(of q1-q4);
   datalines;
2355 S O O S
5889 2 2 2 2
3878 C E E E
4409 0 1 1 1
3985 3 3 3 2
0740 S E E S
2398 E E C C
5162 C C C E
4421 3 2 2 2
7385 C C C N
;
run;


proc print data=points noobs;

title 'The POINTS Data Set';
run;
 

proc contents data=points;run;

*****Page 27*****;




*****Page 34*****;
data one;
input x date9.;
cards;
21jan2015
;run;


proc print data= one ;
run;

proc print data= one ;
format x date9.;
run;


data two; 
   numdate=12115;
   chardate=put(numdate,z6.);
   sasdate=input(chardate,mmddyy6.);
run;


proc print data=two;
title;
run;

proc contents data=two; run;

*****Page 34*****;






*****Page 37*****;
Proc sql noprint;
select name into :vars separated by " "
From dictionary.macros
;
Quit;

proc sql;
select name
from  dictionary.macros;
quit;

%put &vars.;

*****Page 37*****;



*****Page 38*****;
data one; length month $6; retain m 201201;
do i=1 to 3;
   month="month"||put(i,z1.);m+1;call symput(month,m);
   output;end;run;
%put &month1.;
 %macro print();
%do i=1 %to 3;%put &&month&i.;%end;
%mend;
%print;

*****Page 38*****;




*****Page 39*****;
%let no_of_months=12;
data one;
length k k1 $500;
k="";k1="";
do i=&no_of_months to 0 by -1;
k="'month"||compress(i)||",'"||" 'percent"||compress(i)||",' "||k;
k1="month"||compress(i)||" percent"||compress(i)||" "||k1;
end;
call symput('value',k);
call symput('value1',k1);
run;


proc print data=one;
title;
run;

%put &value. &value1. ;
*****Page 39*****;




