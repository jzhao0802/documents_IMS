

%let dir=\\Plyssfa30p.production.imsglobal.com\study data\Y2014;*\please change to your folder\;

option mautosource sasautos="&dir\sas macros";

PROC IMPORT 
     OUT= cities
     DATAFILE= "&dir\bpeng\30 cities.xlsx"
     DBMS=EXCEL 
     replace
;
     GETNAMES=yes;
     sheet="data"
;
RUN;

%imexcel
  (/* Deistination SAS Data Set */ cities
  ,/* "Input file path and name"*/ "&dir\bpeng\30 cities.xlsx"
  ,/* Replace or not (blank)    */ replace
  ,/* Get names (YES|NO)        */ yes
  ,/* Range Description         */ 
  ,/* Additional clauses        */ %nrstr(sheet="data")
  );

data cities2 /debug;
 	set cities;	
/*	population13=population13*1;*/
	keep city population14 population15;
run; 

data cities2;
 	set cities(keep=city population14 population15);
/*	population13=population13*1;*/
/*	keep city population14 population15;*/
run; 

%macro singlecity(num);
	data city;
		set cities2;
		if city="City&num";
		run;
	PROC EXPORT 
	  DATA= city
	  OUTFILE= "&dir\bpeng\city output.xlsx"
	  DBMS=EXCEL 
	  replace
	;
	  SHEET="City&num"; 
	RUN;
%mend;

%singlecity(01);
%singlecity(02);


/*Another way, using %foreach*/

proc sql noprint;
	select distinct city
	into :ct
	separated by ' ' 
	from cities
	;
       quit;

%put &ct;

%foreach(v,&ct,%nrstr(
	%exexcel
	(/* Source SATA Data Set       */ cities2(where=(city="&v"))
	,/* "Output file path and name"*/ "&dir\bpeng\city output foreach.xlsx"
	,/* Replace or not (blank)     */ replace
	,/* "Sheet name"               */ "&v"
)));
