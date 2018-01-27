%let analogue=Effient;
%let user=Jie;
%*** 11/1/2010	9/1/2011	9/1/2014 ***;;
%let announce_date="01NOV2010"d;
%let released_date="01SEP2011"d;
%let post_release_3yr="01SEP2014"d;
%let time_length=12;
%let ATC4_Mkt=AAI;
%let segment=pos;
%let id=7;
%let data_start="01SEP2002"d;
%let INTPROD=EFIENT;

%let data_path=\\plyvnas01\statservices2\CustomStudies\Promotion Management\2014\Pfizer clinical trial impact\02 data\Absolute Value;
%let out_path=\\plyvnas01\statservices2\CustomStudies\Promotion Management\2014\Pfizer clinical trial impact\05 Output\&analogue._&user.;

libname &analogue "&out_path";

proc datasets lib=work kill;
run;
quit;


%*** Statins Market ***;;
proc import out=raw_&analogue._mkt_SU
			datafile="\\plyvnas01\statservices2\CustomStudies\Promotion Management\2014\Pfizer clinical trial impact\02 data\Absolute Value\HVA B1C2 Sales and SUs NWRWM2 Jul14.xlsx"
			dbms=excel replace;
			sheet="SUs";
run;
proc import out=raw_&analogue._mkt_Sale
			datafile="\\plyvnas01\statservices2\CustomStudies\Promotion Management\2014\Pfizer clinical trial impact\02 data\Absolute Value\HVA B1C2 Sales and SUs NWRWM2 Jul14.xlsx"
			dbms=excel replace;
			sheet="Sales";
run;

data raw_&analogue._mkt;
	merge raw_&analogue._mkt_SU raw_&analogue._mkt_Sale;
run;

proc sort data=raw_&analogue._mkt;
	by ATC4 Region INTPRD;
run;

proc means data=raw_&analogue._mkt noprint;
	by ATC4 Region INTPRD;
	var SU_MTH_9_02___Absolute_ -- LCD_MNF_MTH_7_14___Absolute_;
	output out=sum_&analogue._mkt sum=;
run;

data &analogue._mkt_Units_Sales(drop=i SU_MTH_9_02___Absolute_ -- LCD_MNF_MTH_7_14___Absolute_);
	format ATC4 $50. Region $7. INTPRD $30. /*study $25. index_date date9.*/ month date9.;
	set sum_&analogue._mkt(drop=_type_ _freq_);
	by ATC4 Region INTPRD;
	retain ATC4 Region INTPRD;
	array a_SU[1:143] SU_MTH_9_02___Absolute_ -- SU_MTH_7_14___Absolute_;
	array a_Sale[1:143] LCD_MNF_MTH_9_02___Absolute_ -- LCD_MNF_MTH_7_14___Absolute_;
	if first.INTPRD then do;
		do i=1 to 143;
			month=intnx('month', &data_start, i-1);
			Units=a_su[i];
			Sales=a_sale[i];
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
run;

%*** Market share ***;;
proc sort data=&analogue._mkt_Units_Sales;
	by ATC4 Region month;
run;

proc means data=&analogue._mkt_Units_Sales noprint;
	by ATC4 Region month;
	var Units Sales;
	output out=&analogue._mkt_total sum=;
run;

%*** Yan modified the following part ***;;
%*** Check if core product is included in market ***;;

proc sql noprint;
	select count(distinct INTPRD)>0 into: prod_exist
	from &analogue._mkt_Units_Sales 
	where INTPRD contains upcase("&INTPROD");
quit;
%put &prod_exist;

%macro model_data;
%if &prod_exist %then %do;
	%put "&prod_exist";
	data &analogue._model;
		merge &analogue._Units_Sales(in=a) &analogue._mkt_total (drop=_type_ _freq_ rename=(ATC4=Mkt_ATC4 units=Mkt_Units sales=Mkt_Sales));
		by Region month;
		if a;
		%*** Use market total units ***;;
		Market_Units= Mkt_Units;
		Mkt_ATC4 = "&ATC4_Mkt";
		drop Mkt_Units mkt_sales;
	run;
%end;

%else %do;
	%put "&prod_exist";
	data &analogue._model;
		merge &analogue._Units_Sales(in=a) &analogue._mkt_total (drop=_type_ _freq_ rename=(ATC4=Mkt_ATC4 units=Mkt_Units sales=Mkt_Sales));
		by Region month;
		if a;
		%*** Add core product units to market total units ***;;
		Market_Units= Mkt_Units+Units;
		Mkt_ATC4 = "&ATC4_Mkt";
		drop Mkt_Units mkt_sales;
	run;
%end;
%mend model_data;

%model_data;

/*Hui added numbers for overall market*/
proc sort data=&analogue._model;
	by month ;
run;

proc means data=&analogue._model noprint;
	id ATC4 INTPRD;
	by month ;
	var Units  sales Market_Units;
	output out=&analogue._overall(drop=_type_ _freq_) sum=;
run;

data &analogue._overall_f;
	format ATC4 $50. Region $7. INTPRD $30. /*study $25. index_date date9. */ month date9.;
	set &analogue._overall;
	Mkt_ATC4 = "&ATC4_Mkt";
	Region="OVERALL";
run;

proc sort data=&analogue._model;
	by Region month;
run;

%*** Yan fixed a bug for overlapping study period ***;;
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

data &analogue..&analogue._model_&segment._&id;
	set announce release release_3yr;
	Units_share=Units/Market_Units;
run;

proc export data=&analogue..&analogue._model_&segment._&id
			outfile="&out_path.\&analogue._model_&segment._&id._&sysdate..xlsx"
			dbms=excel replace;
run;




