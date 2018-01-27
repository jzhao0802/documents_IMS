%let RootFolder=Y:\Host\Communication of SAS Base; * change to your folder ;

/* example 1.2.1 GETNAMES = YES or NO */
proc import
  out = ex11_v2
  datafile = "&RootFolder.\ex01-import\ex1.1.csv"
  dbms = csv replace;
  GETNAMES = NO; * default value is YES;
run;

/* example 1.2.2 DATAROW = ? */
proc import
  out = ex11_v3
  datafile = "&RootFolder.\ex01-import\ex1.1.csv"
  dbms = csv replace;
  DATAROW = 3; * When GETNAMES = YES, default value is 2| When GETNAMES = NO, default value is 1;
run;

/* example 1.2.3 GETNAMES = with DATAROW = */
proc import
  out = ex11_v4
  datafile = "&RootFolder.\ex01-import\ex1.1.csv"
  dbms = csv replace;
  GETNAMES = NO;
  DATAROW = 2;
run;

/* example 1.2.4 DELIMITER = */
proc import
  out = ex15
  datafile = "&RootFolder.\ex01-import\ex1.5.txt"
  dbms = dlm replace;
  DELIMITER = "~";   * visible delimiter, directly use it;
run;

proc import
  out = ex16
  datafile = "&RootFolder.\ex01-import\ex1.6.txt"
  dbms = dlm replace;
  DELIMITER = "09"x; * TAB is invisible, need to use the hexadecimal coding, the x stands for hexadecimal;
run;                 * http://www.asciima.com/;

/* example 1.2.5 GUESSINGROWS = ? */
proc import
  out = ex11_v5
  datafile = "&RootFolder.\ex01-import\ex1.1.csv"
  dbms = csv replace;
  GUESSINGROWS = 23;
run;

/* example 1.2.6 lrecl = ? */
filename csvfile "&RootFolder.\ex01-import\ex1.7.csv" lrecl = 32767;

proc import
  out = ex17
  datafile = csvfile
  dbms = csv replace;
run;

/* example 1.2.7 Sheet = "?" */
proc import
  out = ex12_s2
  datafile = "&RootFolder.\ex01-import\ex1.2.xlsx"
  dbms = excel replace;
  sheet = "ex1.2 2";
run;

/* example 1.2.8 Range = "?" */
proc import
  out = ex12_s3_v2
  datafile = "&RootFolder.\ex01-import\ex1.2.xlsx"
  dbms = excel replace;
  range = "ex1.2 3$A1:C7";
run;

/* example 1.2.9 Column names don't in the first row */
filename exTemp1 temp; 
data _null_;
  infile "&RootFolder.\ex01-import\ex1.8.txt" firstobs = 3;
  file exTemp1;
  input;
  put _infile_;
run;

proc import
  out = ex18
  datafile = exTemp1
  dbms = dlm replace;
  DELIMITER = "09"x; * TAB is invisible, need to use the hexadecimal coding, the x stands for hexadecimal;
run; 


proc import
  out = ex11_v2
  datafile = "&RootFolder.\ex01-import\ex1.9.csv"
  dbms = csv replace;
run;

data WORK.EX11_V2    ;
     %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     infile 'Y:\Host\Communication of SAS Base\ex01-import\ex1.9.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
        informat Country $19. ;
        informat Area_wise best32. ;
        informat Populations best32. ;
        informat Dummy1 $4. ;
        informat Dummy2 $1. ;
        format Country $19. ;
        format Area_wise best12. ;
        format Populations best12. ;
        format Dummy1 $4. ;
        format Dummy2 $1. ;
     input
                 Country $
                 Area_wise
                 Populations
                 Dummy1 $ ~
                 Dummy2 $
     ;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
     run;
