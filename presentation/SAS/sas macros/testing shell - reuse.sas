%let macro_path=\\plyvnas01\StatServices\CustomStudies\Sales & Account Management\_SAS macros_;

%include "&macro_path.\macro_pk.sas";
%include "&macro_path.\macro_lib_dsn.sas";

/*proc copy*/
/*  in=sashelp*/
/* out=work*/
/*  memtype=data*/
/*;*/
/*select zipcode;*/
/*run;quit;*/

data zipcode;
set sashelp.zipcode;
run;

%pk(zipcode,city_county_state,city countynm statecode);
%pk(zipcode,zipc,zip);

%let testlib=;
%let testdsn=;
%lib_dsn(zipcde,testlib,testdsn);

%put testlib=&testlib.;
%put testdsn=&testdsn.;
