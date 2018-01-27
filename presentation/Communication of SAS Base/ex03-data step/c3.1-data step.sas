%let RootFolder=Y:\Host\Communication of SAS Base; * change to your folder ;

/* example 3.1.1 Create dataset from existing dataset */
libname mylib "&RootFolder.\ex03-data step";

data mylib.adomsg;
  set sashelp.adomsg;
run;

data mylib.adomsg_even(keep = MSGID MNEMONIC) mylib.adomsg_odd(drop = LEVEL);
  set sashelp.adomsg;

  if MOD(MSGID, 2) = 0 then
    output mylib.adomsg_even;
  else
    output mylib.adomsg_odd;
run;

data mylib.adomsg_even_odd_v1;
  set mylib.Adomsg_even mylib.Adomsg_odd;
run;

data mylib.adomsg_even_odd_v2;
  set mylib.Adomsg_even;
  set mylib.Adomsg_odd;
run;

/* 
  example 3.1.2 Sort a dataset 
  proc sort : http://support.sas.com/documentation/cdl/en/proc/61895/HTML/default/viewer.htm#a000057941.htm

  difference between  noduprecs(nodup) and nodupkey 
  http://www2.sas.com/proceedings/sugi30/037-30.pdf
*/
proc sort
  data = mylib.Adomsg_even_odd_v1 out = mylib.Adomsg_even_odd_v1_sorted;
  by MSGID;
run;

data mylib.temp;
  input a $ b $ c d;
  cards;
a a 1 1
a a 1 1
a a 1 3
a a 1 1
a a 1 3
;
run;

proc sort
  data = mylib.temp out = mylib.temp_v2 nodupkey;
  by a b;
run;

proc sort
  data = mylib.temp out = mylib.temp_v3 nodup;
  by a b;
run;

/* example 3.1.3 Read external file */
* fixed column file ;
/*
filename xpd_raw
  ftp "'STS.ALCON.PED.D13APR15'"
  host = "162.44.75.196"
  user = "TCNLNGH" pass = "wertyu2"
  recfm = v debug
;
*/
filename xpd_raw "&RootFolder.\ex03-data step\D13APR15.txt";
data mylib.xpo_raw_data;
  infile xpd_raw missover lrecl = 32767;
  informat
    PROJECTED_TRX             12.4
    MONTH_YEAR_CODE           $6.
    PRODUCT_ID                $7.
    PRODUCT_DESCRIPTION       $24.
    MANUFACTURER_NAME         $18.
    USC_CODE                  $5.
    USC_DESCRIPTION           $27.
    SPECIALTY_DESCRIPTION     $25.
    NPA_SPECIALTY_CODE        $2.
    NPA_SPECIALTY_GROUP_CODE  $2.
  ;

  format
    PROJECTED_TRX             12.4
    MONTH_YEAR_CODE           $6.
    PRODUCT_ID                $7.
    PRODUCT_DESCRIPTION       $24.
    MANUFACTURER_NAME         $18.
    USC_CODE                  $5.
    USC_DESCRIPTION           $27.
    SPECIALTY_DESCRIPTION     $25.
    NPA_SPECIALTY_CODE        $2.
    NPA_SPECIALTY_GROUP_CODE  $2.
  ;

  input
    PROJECTED_TRX             1   -  12
    MONTH_YEAR_CODE           13  -  18
    PRODUCT_ID                19  -  25
    PRODUCT_DESCRIPTION       26  -  49
    MANUFACTURER_NAME         50  -  67
    USC_CODE                  68  -  72
    USC_DESCRIPTION           73  -  99
    SPECIALTY_DESCRIPTION     100 - 124
    NPA_SPECIALTY_CODE        125 - 126
    NPA_SPECIALTY_GROUP_CODE  127 - 128
  ;
run;
* 107,267 ;

/*
  example 3.1.4 Read mixed text, use Modified List Input @ @@
  web site : http://support.sas.com/documentation/cdl/en/lrdict/64316/HTML/default/viewer.htm#a000144370.htm
*/
data mixedimpt;
  infile "&RootFolder.\ex03-data step\imptdt03.txt";
  input
    SSN $       1 - 11
    HireDate    date7.
    Salary:     comma6.
    Department: $9.
    Phone_No
  ;
  format HireDate yymmdd10. Salary Dollar7.;
run;

/* example 3.1.5 infile, flowover, missover, truncover and pad */
data test1;
  infile "&RootFolder.\ex03-data step\Armyman.txt" pad;
  input
    Lastn   $ 1  - 21
    Firstn  $ 22 - 31
    Empid   $ 32 - 36
    Jobcode $ 37 - 45
  ;
run;

data test2;
  infile "&RootFolder.\ex03-data step\Armyman.txt" missover;
  input
    Lastn   $
    Firstn  $
    Empid   $
    Jobcode $
  ;
run;

data test3;
  input
    Lastn   $ 1  - 21
    Firstn  $ 22 - 31
    Empid   $ 32 - 36
    Jobcode $ 37 - 45
  ;
  cards;
LANGKAMM             SARAH     E0045 Mechanic
TORRES               JAN       E0029 Pilot
SMITH                MICHAEL   E0065
LEISTNER             COLIN     E0116 Mechanic
TOMAS                HARALD
WADE                 KIRSTEN   E0126 Pilot
WAUGH                TIM       E0204 Pilot
;
run;

data test;
  input
    Lastn   $
    Firstn  $
    Empid   $
    Jobcode $
  ;

  cards;
LANGKAMM             SARAH     E0045 Mechanic
TORRES               JAN       E0029 Pilot
SMITH                MICHAEL   E0065
LEISTNER             COLIN     E0116 Mechanic
TOMAS                HARALD
WADE                 KIRSTEN   E0126 Pilot
WAUGH                TIM       E0204 Pilot
;
run;

/* example 3.1.6 useful statements */
* retain ;
data temp;
  do i = 1, 3, 5;
    output;
  end;
run;

data temp2;
  set temp;
  retain y 0;
  if _N_ = 1 then
    y = 1;
run;

* missing input ;
data temp;
  input x $1.;
  cards;
a
6
3
b
6
8
c
8
2
;
run;

data temp2; 
  set temp;
  if missing(input(x, best32.)) then
    delete;
  y = input(x, best32.);
run;

data _null_;
  a = '01Jan2014'd;
  put a;
  b = put(a, yymmddn8.);
  put b;
run;

data temp3;
  x=1257000;
  y = put(x, best32.);
  z = strip(y);
  aa = trim(y);
run;

* by ;
data temp4;
  input a $ b;
  cards;
a 1
a 2
a 3
b 4
b 5
b 6
c 7
c 8
c 9
d 3
d 5
d 7
e 8
;
run;

data temp4_v2;
  set temp4;
  retain c 0;
  by a;

  if first.a then
    c = 0;

  c = c + b;

  if last.a then
    output;
run;

* diff between if and where ;
data temp4_v3;
  set temp4_v2;
  d = strip(a) || strip("1");
/*  if a = "d";*/
/*  where a = "d";*/
/*  if strip(d) = strip("d1");*/
  where strip(d) = strip("d1");
run;

data test1;
  input a;
  cards;
1
2
3
4
5
6
7
8
9
10
;
run;

data test1_v3;
  set test1(obs = 3);
/*  if MOD(a, 2) = 0;*/
  where MOD(a, 2) = 0;
run;

data test1_v4;
  set test1(firstobs = 3 obs = 3);
run;

* tranwrd and index ;

* How to connect to Oracle ;
%let ora_un= xxx ;
%let ora_pw= yyy;
libname dbSDIDW oledb init_string="Provider=OraOLEDB.Oracle;Data Source=IDWDEV01;USER ID=&ora_un;PASSWORD=&ora_pw.;PORT=1521" 
schema="GHU";

libname ORADB oracle user = "&ora_un." password = "&ora_pw." path = "IDWDEV01";

* Chech which module do you have;
proc setinit;
run;
