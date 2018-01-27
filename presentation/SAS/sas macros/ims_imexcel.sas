/*
Note:
  Import Excel data into SAS 
  MUST define a data Range 
  in the Excel workbook
  in one of the two following ways
  1. Named Range
  2. SheetName!ExcelAddress

/*all positional parameters must precede keyword parameters*/

%macro ims_imexcel
(/*Positional Paremeters*/
/*<libref>.SAS-data-set*/dsn
,/*filename*/filename
,/*spreadsheet-name*/spreadsheet

/*Keyword Parameters*/
,/*data-source-identifier: EXCEL|XLS|(Others)*/identifier=EXCEL
,/*overwrites an existing SAS data set: REPLACE|(Blank)*/REPLACE=REPLACE
,/*YES|NO*/GETNAMES=YES;
,/*YES|NO*/MIXED=YES;
,/*YES|NO*/SCANTEXT=YES;
,/*YES|NO*/USEDATE=YES;
,/*YES|NO*/SCANTIME=YES
); 

PROC IMPORT OUT= &dsn
            DATAFILE= "&filename" 
            DBMS=&identifier &REPLACE;
     RANGE="&spreadsheet"; 
     GETNAMES=&GETNAMES;
     MIXED=&MIXED;
     SCANTEXT=&SCANTEXT;
     USEDATE=&USEDATE;
     SCANTIME=&SCANTIME;
RUN;

%mend ims_imexcel;
