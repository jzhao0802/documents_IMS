%macro mysummaryv2(mydata,mydep,dsname,myfile,mysheet,myweight);
      proc sort data=&mydata; 
	  by &dsname;
      run;
	  Proc means data=&mydata noprint;
	  weight &myweight;
	  var &mydep;
	  by &dsname;	
	  output out=dsname mean=;
		run;
	  Data dsname (Drop=_Type_);
	  set dsname;
	  run;

	PROC EXPORT DATA=dsname
            OUTFILE= "&myfile" 
            DBMS=EXCEL2000 REPLACE;
     SHEET="&mysheet"; 

RUN;
%mend mysummaryv2;

