%let analogue=Tekturna;
%let user=Jie;
%*** 1/1/2008	8/1/2011 	8/1/2014 ***;;
%let announce_date="01JAN2008"d;
%let released_date="01AUG2011"d;
%let post_release_3yr="01AUG2014"d;
%let time_length=12;
%let ATC4_Mkt=AAI;
%let segment=par;
%let id=3;
%let data_start="01SEP2002"d;
%let INTPROD=Tekturna;

%let data_path=\\plyvnas01\statservices2\CustomStudies\Promotion Management\2014\Pfizer clinical trial impact\02 data\Absolute Value;
%let out_path=\\Plyvnas01\Statservices2\CustomStudies\Promotion Management\2014\Pfizer clinical trial impact\05 Output\Phase I Result\&analogue.&user.;

libname &analogue "&out_path";

proc datasets lib=work kill;
run;
quit;


%*** Statins Market ***;;
proc import out=raw_&analogue._mkt
			datafile="\\plyvnas01\statservices2\CustomStudies\Promotion Management\2014\Pfizer clinical trial impact\02 data\Absolute Value\HVA C9X0 Sales and SUs NWRWM2 Jul14.xls"
			dbms=excel replace;
			sheet="HVA C9X0 Sales and SUs NWRWM2 J";
run;
/*LCD_MNF_MTH_@_12_Absolute_ excluded*/


/*proc import out=raw_&analogue._mkt_Sale*/
/*			datafile="\\plyvnas01\statservices2\CustomStudies\Promotion Management\2014\Pfizer clinical trial impact\02 data\Absolute Value\HVA C9X0 Sales and SUs NWRWM2 Jul14.xls"*/
/*			dbms=excel replace;*/
/*			sheet="Sales";*/
/*run;*/
/**/
/*data raw_&analogue._mkt;*/
/*	merge raw_&analogue._mkt_SU raw_&analogue._mkt_Sale;*/
/*run;*/

/*add with "" to SU MTH/1/05  (Absolute)-SU MTH/3/05  (Absolute)*/
data raw_&analogue._mkt_add;
format SU_MTH_1_05___Absolute_ best11. SU_MTH_2_05___Absolute_ best11. SU_MTH_3_05___Absolute_ best11.;
set raw_&analogue._mkt;
array missing SU_MTH_1_05___Absolute_  SU_MTH_2_05___Absolute_ SU_MTH_3_05___Absolute_;
do over missing;
		missing="";
end;
run;
proc sort data=raw_&analogue._mkt;
	by ATC4 Region INTPRD;
run;
/*34*/

/*order month by macro*/

proc contents data=&analogue..raw_Effient_mkt short
out=varlist;
quit;
data varlist2;
set varlist;
keep NAME VARNUM;
run;
data varlist3;
set varlist2;
if NAME="SU_MTH_1_05___Absolute_" then VARNUM=33.1;
if NAME="SU_MTH_2_05___Absolute_" then VARNUM=33.2;
if NAME="SU_MTH_3_05___Absolute_" then VARNUM=33.3;
run;

proc sort data=varlist3;by VARNUM;quit;

data _null_;
  set varlist3 end = eof;
  call symput(strip("VAR") || strip(_N_), strip(NAME));
  if eof then
    call symput("VARCOUNT", strip(_N_));
run;

%put _user_;

%macro test;
  data raw_&analogue._mkt_add_order;
    retain
      %do i = 1 %to &VARCOUNT.;
        &&VAR&i.
      %end;
    ;
    set raw_&analogue._mkt_add;
  run;
%mend;

%test;


proc sort data=raw_&analogue._mkt_add_order out=raw_&analogue._mkt_add_order_sort;by ATC4 Region INTPRD;run;
proc means data=raw_&analogue._mkt_add_order_sort noprint;  
	by ATC4 Region INTPRD;
	var SU_MTH_9_02___Absolute_ -- LCD_MNF_MTH_1_12___Absolute_;
	output out=sum_&analogue._mkt sum=;
run;


data &analogue._mkt_Units_Sales(drop=i SU_MTH_9_02___Absolute_ -- LCD_MNF_MTH_1_12___Absolute_);
	format ATC4 $50. Region $7. INTPRD $30. /*study $25. index_date date9.*/ month date9.;
	set sum_&analogue._mkt(drop=_type_ _freq_);
	by ATC4 Region INTPRD;
	retain ATC4 Region INTPRD;
	array a_SU[1:143] SU_MTH_9_02___Absolute_ -- SU_MTH_7_14___Absolute_;
/*	array a_Sale[1:109] LCD_MNF_MTH_9_02___Absolute_ -- LCD_MNF_MTH_1_12___Absolute_;*/
	if first.INTPRD then do;
		do i=1 to 143;
			month=intnx('month', &data_start, i-1);
			Units=a_su[i];
/*			Sales=a_sale[i];*/
			/*
			if month >= intnx('month', &announce_date, %eval(0-&time_length)) and month <= intnx('month', &announce_date, max(%eval(&time_length-1), 0)) then do;
				study="Announcement";
				index_date=&announce_date;
			end;
			else if month >= intnx('month', &released_date, %eval(0-&time_length)) and month <= intnx('month', &released_date, max(%eval(&time_length-1), 0)) then do;
				study="Result Released";
				index_date=&released_date;
			end;
			else if month >= intnx('month', &post_release_3yr, %eval(0-&time_length)) and month <= intnx('month', &post_release_3yr, max(%eval(&time_length-1), 0)) then do;
				study="Three Year Post-Release";
				index_date=&post_release_3yr;
			end;
			else study ="";
			*/
			if month >= intnx('month', &announce_date, %eval(0-&time_length)) and month <= intnx('month', &post_release_3yr, max(%eval(&time_length-1), 0)) then do;
				*if month < index_date then period="Pre";
				*else period="Post";
				output;
			end;
		end;
	end;	
run;
/*910/EU+JP+US*/

%*** Get data for core product ***;;
/*
proc import out=raw_&analogue
			datafile="\\plyvnas01\statservices2\CustomStudies\Promotion Management\2014\Pfizer clinical trial impact\02 data\Absolute Value\HVA Vytorin Sales and SUs V.3 NWRWM2 Jul14.xls"
			dbms=excel replace;
			sheet="Sales and SUs";
run;

proc sort data=raw_&analogue;
	by ATC4 Region INTPRD;
run;

proc means data=raw_&analogue noprint;
	by ATC4 Region INTPRD;
	var SU_MTH_4_05___Absolute_ -- LCD_MNF_MTH_7_14___Absolute_;
	output out=sum_&analogue sum=;
run;

data &analogue._Units_Sales(drop=i SU_MTH_4_05___Absolute_ -- LCD_MNF_MTH_7_14___Absolute_);
	format ATC4 $50. Region $7. INTPRD $30. study $25. index_date date9. period $4. month date9.;
	set sum_&analogue(drop=_type_ _freq_);
	by ATC4 Region INTPRD;
	retain ATC4 Region INTPRD;
	array a_SU[1:112] SU_MTH_4_05___Absolute_ -- SU_MTH_7_14___Absolute_;
	array a_Sale[1:112] LCD_MNF_MTH_4_05___Absolute_ -- LCD_MNF_MTH_7_14___Absolute_;
	if first.INTPRD then do;
		do i=1 to 112;
			month=intnx('month', '01APR05'd, i-1);
			Units=a_su[i];
			Sales=a_sale[i];
			if month >= intnx('month', &announce_date, %eval(0-&time_length)) and month <= intnx('month', &announce_date, %eval(&time_length-1)) then do;
				study="Announcement";
				index_date=&announce_date;
			end;
			else if month >= intnx('month', &released_date, %eval(0-&time_length)) and month <= intnx('month', &released_date, %eval(&time_length-1)) then do;
				study="Result Released";
				index_date=&released_date;
			end;
			else if month >= intnx('month', &post_release_3yr, %eval(0-&time_length)) and month <= intnx('month', &post_release_3yr, %eval(&time_length-1)) then do;
				study="Three Year Post-Release";
				index_date=&post_release_3yr;
			end;
			else study ="";
			if study ne "" then do;
				if month < index_date then period="Pre";
				else period="Post";
				output;
			end;
		end;
	end;	
run;
*/
data &analogue._Units_Sales;
	set &analogue._mkt_Units_Sales;
	where INTPRD contains upcase("&INTPROD");
/*	where INTPRD = upcase("&INTPROD");*/

run;
/*455/US+JP+EU
273*/

%*** Market share ***;;
proc sort data=&analogue._mkt_Units_Sales;
	by ATC4 Region month;
run;
/*910*/
proc means data=&analogue._mkt_Units_Sales noprint;
	by ATC4 Region month;
	var Units 
/*Sales*/
;
	output out=&analogue._mkt_total sum=;
run;
/*273*/
proc sort data=&analogue._mkt_total;by ATC4 Region month;run;

%*** Yan modified the following part ***;;
%*** Check if core product is included in market ***;;

proc sql noprint;
	select count(distinct INTPRD)>0 into: prod_exist
	from &analogue._mkt_Units_Sales 
	where INTPRD contains upcase("&INTPROD");
/*	where INTPRD = upcase("&INTPROD");*/

quit;
%put &prod_exist;*1;

%macro model_data;
proc sort data=&analogue._Units_Sales ;by Region month;run;

%if &prod_exist %then %do;
	%put "&prod_exist";
	data &analogue._model;
		merge &analogue._Units_Sales(in=a) &analogue._mkt_total (drop=_type_ _freq_ rename=(ATC4=Mkt_ATC4 units=Mkt_Units /*sales=Mkt_Sales*/));
		by Region month;
		if a;
		%*** Use market total units ***;;
		Market_Units= Mkt_Units;
		Mkt_ATC4 = "&ATC4_Mkt";
		drop Mkt_Units /*mkt_sales*/;
	run;
%end;

%else %do;
	%put "&prod_exist";
	data &analogue._model;
		merge &analogue._Units_Sales(in=a) &analogue._mkt_total (drop=_type_ _freq_ rename=(ATC4=Mkt_ATC4 units=Mkt_Units /*sales=Mkt_Sales*/));
		by Region month;
		if a;
		%*** Add core product units to market total units ***;;
		Market_Units= Mkt_Units+Units;
		Mkt_ATC4 = "&ATC4_Mkt";
		drop Mkt_Units /*mkt_sales*/;
	run;
%end;
%mend model_data;

%model_data;
/*455*/

/*Hui added numbers for overall market*/
proc sort data=&analogue._model;
	by month ;
run;
/*455*/
proc means data=&analogue._model noprint;
	id ATC4 INTPRD;
	by month ;
	var Units  /*sales*/ Market_Units;
	output out=&analogue._overall(drop=_type_ _freq_) sum=;
run;
/*91 in overall*/
data &analogue._overall_f;
	format ATC4 $50. Region $7. INTPRD $30. /*study $25. index_date date9. */ month date9.;
	set &analogue._overall;
	Mkt_ATC4 = "&ATC4_Mkt";
	Region="OVERALL";
run;

proc sort data=&analogue._model;
	by Region month;
run;
/*455
273*/

%*** Yan fixed a bug for overlapping study period ***;;
/*set 3 country with overall data by period*/
data announce;
	format ATC4 $50. Region $7. INTPRD $30. study $25. index_date date9.  period $4. month date9.;
	set &analogue._model &analogue._overall_f;
	if month >= intnx('month', &announce_date, %eval(0-&time_length)) and month <= intnx('month', &announce_date, %eval(&time_length-1)) then do;
				study="Announcement";
				index_date=&announce_date;
				if month >= index_date then period="Post";
				else if month < index_date then period="Pre";
				output;
	end;
run;
/*96*/
data Release;
	format ATC4 $50. Region $7. INTPRD $30. study $25. index_date date9.  period $4. month date9.;
	set &analogue._model &analogue._overall_f;
	if month >= intnx('month', &released_date, %eval(0-&time_length)) and month <= intnx('month',  &released_date, %eval(&time_length-1)) then do;
				study="Result Released";
				index_date=&released_date;
				if month >= index_date then period="Post";
				else if month < index_date then period="Pre";
				output;
	end;
run;
/*96*/

data Release_3yr;
	format ATC4 $50. Region $7. INTPRD $30. study $25. index_date date9.  period $4. month date9.;
	set &analogue._model &analogue._overall_f;
	if month >= intnx('month', &post_release_3yr, %eval(0-&time_length)) and month <= intnx('month',  &post_release_3yr, %eval(&time_length-1)) then do;
				study="Three Year Post-Release";
				index_date=&post_release_3yr;
				if month >= index_date then period="Post";
				else if month < index_date then period="Pre";
				output;
	end;
run;
/*91
48*/

data &analogue..&analogue._model_&segment._&id;
	set announce release release_3yr;
	Units_share=Units/Market_Units;
run;
/*240*/
/*360*/


/*data &analogue..&analogue._model_&segment._&id._remove &analogue..&analogue._model_&segment._&id._rest;*/
/*set &analogue..&analogue._model_&segment._&id;*/
/*if  INTPRD=UPCASE("&analogue.") then output &analogue..&analogue._model_&segment._&id._remove;*/
/*else output &analogue..&analogue._model_&segment._&id._rest;*/
/*run;*/
/*180*/
/*ATC4	Region	INTPRD	study	index_date	period	month	Units	Sales	Mkt_ATC4	Market_Units	Units_share*/

proc means data=&analogue..&analogue._model_&segment._&id;
id ATC4	period	index_date  Mkt_ATC4;
class Region study month;
var Units Market_Units Units_share;
output out=&analogue..&analogue._model_&segment._&id._TotalProd (drop=_type_ _freq_ ) sum=;
run;
/*240*/

data &analogue..&analogue._model_&segment._&id.;
set &analogue..&analogue._model_&segment._&id._TotalProd;
INTPRD="&analogue.";
Units_share=Units/Market_Units;
if region ne "" and study ne "" and month ne "" ;
run;
/*360=144+144+72*/
/*240*
proc export data=&analogue..&analogue._model_&segment._&id.
			outfile="&out_path.\&analogue._model_&segment._&id._&sysdate._TotalProd.xlsx"
			dbms=excel replace;
run;




