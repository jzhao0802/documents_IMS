%let RootFolder=Y:\Host\Communication of SAS Base; * change to your folder ;

/* example 1.1.1 import the csv file : ex1.1.csv */
proc import
  out = ex11
  datafile = "&RootFolder.\ex01-import\ex1.1.csv"
  dbms = csv replace;
run;

/* example 1.1.2 import the excel file : ex1.2.xlsx */
proc import
  out = ex12
  datafile = "&RootFolder.\ex01-import\ex1.2.xlsx"
  dbms = excel replace;
run;

/* example 1.1.3 import the Access file : ex1.3.mdb */
proc import
  out = ex13
  table = "table2"
  dbms = access replace;
  database = "&RootFolder.\ex01-import\ex1.3.mdb";
run;

/* example 1.1.4 import the SPSS file : ex1.4.sav */
proc import
  out = ex14
  datafile = "&RootFolder.\ex01-import\ex1.4.sav"
  dbms = spss replace;
run;
