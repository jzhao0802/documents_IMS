
%macro ims_excsv
(/*Positional Paremeters*/
/*<libref>.SAS-data-set*/dsn
,/*filename*/filename

/*Keyword Parameters*/
,/*overwrites an existing SAS data set: REPLACE|(Blank)*/REPLACE=REPLACE
,/*YES|NO*/PUTNAMES=YES);

PROC EXPORT DATA=&dsn
            OUTFILE="&filename" 
            DBMS=CSV &REPLACE;
     PUTNAMES=&PUTNAMES;
RUN;

%mend ims_excsv;
