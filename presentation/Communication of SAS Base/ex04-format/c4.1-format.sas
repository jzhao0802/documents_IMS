%let RootFolder=Y:\Host\Communication of SAS Base; * change to your folder ;

/* example 4.1.1 Create format for numeric variable */
proc format;
  value boarded
    0  - 30   = '0  - 30'
    31 - 50   = '31 - 50'
    51 - 70   = '51 - 70'
    70 - high = '70+'
  ;
run;

data t1;
  a = 32;
  b = put(a, boarded.);
  put "age = " a ", belong to " b " group";
  output;
  a = 99;
  b = put(a, boarded.);
  put "age = " a ", belong to " b " group";
  output;
run;

data _null_;
  a = 32;
  b = put(a, boarded.);
  put "age = " a ", belong to " b " group";
  a = 99;
  b = put(a, boarded.);
  put "age = " a ", belong to " b " group";
run;

/* example 4.1.2 Create format for character variable and create a dataset */
proc format
  cntlout = fmt_partys;
  value $partys
    'D' = 'Democratic'
    'R' = 'Republican'
    other = 'Other';
  select $partys;
run;

data _null_;
  a = 'D';
  b = put(a, $partys.);
  put "PartyID = " a ", PartyName = " b;
  a = 'S';
  b = put(a, $partys.);
  put "PartyID = " a ", PartyName = " b;
run;

/* example 4.1.3 Create format picture for numeric variable */
proc format;
  picture cell low - high = '000-0000';	               *5551212   555-1212;
  picture sal low - high = '000,009.99' (prefix = '~');  *25981.542 ~25,981.54;
  picture numpic low - high = '000,009.99' (fill = '*'); *44.44     *****44.44;	
run;

data _null_;
  a = 1234567;
  b = put(a, cell.);
  put "Exact number is " a ", formated version is " b;

  a = 1234567.234;
  c = put(a, sal.);
  put "Exact number is " a ", formated version is " c;

  a = 123.423452;
  d = put(a, numpic.);
  put "Exact number is " a ", formated version is " d;
run;

/* example 4.1.4 Specify the library for format */
libname myfmtlib "&RootFolder.\ex04-format";
proc format
  library = myfmtlib;
  picture longdate (default = 30)
    '01jan1950'd - '31dec04'd = '%A,  %B %d,' (datatype = date);
run;
options fmtsearch = (myfmtlib, work); * where to find the format.;

data _null_;
  a = '01jan1950'd;
  b = put(a, date9.);
  c = put(a, longdate.);
  put "Exact number is " a "Exact date is " b ", formated version is " c;
run;

proc format
  fmtlib library = myfmtlib;
  select $Longdate;
run;

/* example 4.1.5 Create Format via a dataset */
data dict;
  input x y $;
  datalines;
1 a
2 s
3 d
4 f
5 g
;
run;

data my_fmt;
  set dict end = eof;
  fmtname = "my_fmt";
  start = x;
  label = y;
  output;

  if eof then do;
    fmtname = "my_fmt";
    HLO = 'O';    *HLO : HIGH LOW OTHER;
    label = "Other";
    output;
  end;
run;

proc format 
  cntlin = my_fmt;
run;
quit;

data temp;
  input x;
  datalines;
1
6
;
run;

data _null_;
  set temp;
  result = put(x, my_fmt.);
  put x result;
run;

/* example 4.1.5 Multiple Label */
proc format;
  value key
    low   - 0.20 = 'a1'
    0.20 <- 0.25 = 'a2'
    0.25 <- 0.35 = 'a3'
    0.35 <- 0.80 = 'd1'
    0.80 <- high = 'd6'
  ;

  value $deccode (multilabel notsorted)
    'a1'        = '11'
    'a2'        = '12'
    'a3'        = '13'
    'a0' - 'a9' = '14'
    'd1'        = '15'
    'd6'        = '16'
    'd0' - 'd9' = '17'
  ;
run;
