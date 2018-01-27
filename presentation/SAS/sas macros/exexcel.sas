/*
  Export SAS data into Excel
*/

%macro exexcel
(/* Source SATA Data Set       */ _exexcel_indsn
,/* "Output file path and name"*/ _exexcel_outfsn
,/* Replace or not (blank)     */ _exexcel_opt_replace
,/* "Sheet name"               */ _exexcel_wsh_name
);

PROC EXPORT 
  DATA= &_exexcel_indsn.
  OUTFILE= &_exexcel_outfsn.
  DBMS=EXCEL 
  &_exexcel_opt_replace.
;
  SHEET=&_exexcel_wsh_name.; 
RUN;

%mend  exexcel;
