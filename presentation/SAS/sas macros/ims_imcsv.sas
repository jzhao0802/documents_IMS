
%macro ims_imcsv
(/*Positional Paremeters*/
/*<libref>.SAS-data-set*/dsn
,/*filename*/filename

/*Keyword Parameters*/
,/*YES|NO*/GETNAMES=YES
,GUESSINGROWS=20
,/*2*/DATAROW=2
);

PROC IMPORT OUT= &dsn 
            DATAFILE= "&filename" 
            DBMS=CSV REPLACE;
     GETNAMES=&GETNAMES;
	 GUESSINGROWS=&GUESSINGROWS;
     DATAROW=&DATAROW; 
RUN;

%mend ims_imcsv;
