

%macro mysummary(mydata,myvar,myby,myfile,mysheet);
      proc sort data=&mydata; 
	  by &myby;
      run;
	  Proc means data=&mydata noprint;
	  var &myvar;
	  by &myby;	
	  output out=sumtemp mean=;
		run;
	  Data sumtemp (Drop=_Type_);
	  set sumtemp;
	  run;

	PROC EXPORT DATA= work.sumtemp
            OUTFILE= "&myfile" 
                  DBMS=EXCEL2000 REPLACE;
     SHEET="&mysheet"; 
	 run;
%mend mysummary;


/************************************************************************/

/********* same summarize macro as above only weight variable added *****/

/************************************************************************/



%macro mysummary_weighted(mydata,myvar,myby,myfile,mysheet,myweight);
      proc sort data=&mydata; 
	  by &myby;
      run;
	  Proc means data=&mydata noprint;
	  var &myvar;
	  by &myby;	
	  weight &myweight; /* weight statement added */
	  output out=sumtemp mean=;
		run;
	  Data sumtemp (Drop=_Type_);
	  set sumtemp;
	  run;

	PROC EXPORT DATA= work.sumtemp
            OUTFILE= "&myfile" 
                  DBMS=EXCEL2000 REPLACE;
     SHEET="&mysheet"; 
	 run;
%mend mysummary_weighted;

