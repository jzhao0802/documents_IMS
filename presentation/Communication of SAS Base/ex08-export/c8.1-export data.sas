%let RootFolder=Y:\Host\Communication of SAS Base; * change to your folder ;

/* example 8.1.1 export to the csv file */
proc export
  data = ex11
  outfile = "&RootFolder.\ex08-export\ex8.1.1.csv"
  dbms = csv replace;
run;

/* example 8.1.2 export to the excel file */
proc export
  data = ex12
  outfile = "&RootFolder.\ex08-export\ex8.1.2.xlsx"
  dbms = excel replace;
  range = "sheet2$A2:D23";
run;

proc export
  data = ex12
  outfile = "&RootFolder.\ex08-export\ex8.1.2.xlsx"
  dbms = excel replace;
  sheet = "export data";
run;

/* example 8.1.3 import the Access file : ex1.3.mdb */
proc export
  data = ex13
  outtable = "table3"
  dbms = access replace;
  database = "&RootFolder.\ex08-export\ex8.1.3.mdb";
run;

/* example 8.1.4 import the SPSS file : ex1.4.sav */
proc export
  data = ex14
  outfile = "&RootFolder.\ex08-export\ex8.1.4.sav"
  dbms = spss replace;
  fmtlib = work.formats;
run;

/* example 8.1.5 DELIMITER = */
proc export
  data = ex15
  outfile = "&RootFolder.\ex08-export\ex1.5.txt"
  dbms = dlm replace;
  DELIMITER = "&";
run;

proc export
  data = ex16
  outfile = "&RootFolder.\ex08-export\ex1.6.txt"
  dbms = dlm replace;
  DELIMITER = "09"x;
run;
