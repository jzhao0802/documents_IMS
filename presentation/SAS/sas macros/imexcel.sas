/*
Note:
  Import Excel data into SAS 
  MUST define a data Range 
  in the Excel workbook
  in one of the two following ways
  1. Named Range
  2. SheetName!ExcelAddress

Example:
%imexcel
  (work.channels
  ,\\plyvnas01\statservices\CustomStudies\Promotion Management Studies\Training_2008Q4\Case_Study1\lsu\Symbicort 22apr08 001_Nigel.xls
  ,replace
  ,yes
  ,all_channels
  ,
  );
*/

%macro imexcel
  (/* Deistination SAS Data Set */ _imexcel_dsn
  ,/* "Input file path and name"*/ _imexcel_pathfile
  ,/* Replace or not (blank)    */ _imexcel_opt_replace
  ,/* Get names (YES|NO)        */ _imexcel_opt_get_names
  ,/* Range Description         */ _imexcel_opt_range_desc
  ,/* Additional clauses        */ _imexcel_opt_additinal
  );

PROC IMPORT 
     OUT= &_imexcel_dsn.
     DATAFILE= &_imexcel_pathfile. 
     DBMS=EXCEL 
     &_imexcel_opt_replace.
;
     RANGE="&_imexcel_opt_range_desc.";
     GETNAMES=&_imexcel_opt_get_names.;
     &_imexcel_opt_additinal.
;
RUN;

%mend  imexcel;
