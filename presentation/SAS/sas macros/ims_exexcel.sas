/*
  Export SAS data into Excel
*/
/*all positional parameters must precede keyword parameters*/
%macro ims_exexcel
(/*Positional Paremeters*/
/*<libref>.SAS-data-set*/dsn
,/*filename*/filename
,/*spreadsheet-name*/spreadsheet

/*Keyword Parameters*/
,/*data-source-identifier: EXCEL|(Others)*/identifier=EXCEL
,/*overwrites an existing SAS data set: REPLACE|(Blank)*/REPLACE=REPLACE
,/*(Blank)|LABEL */LABEL=
,/*NO|YES*/NEWFILE=NO 
); 

PROC EXPORT DATA=&dsn 
            OUTFILE="&filename" 
            DBMS=&identifier &REPLACE &LABEL;
  SHEET="&spreadsheet"; 
  NEWFILE=&NEWFILE;
RUN;

%mend ims_exexcel;
