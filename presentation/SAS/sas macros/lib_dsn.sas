/*Detect libname portion of a two-level SAS dsn*/
/*and save libname in &_lib_dsn_outlib. and the*/
/*pure dsn in &_lib_dsn_outdsn..*/
/*If one-level SAS dsn is fed, 'work' is the*/
/*presumed libname .*/

/*All 3 parameters required*/

/*The resolved values of '_lib_dsn_outlib'*/
/*and '_lib_dsn_outdsn', i.e. &_lib_dsn_outlib.*/
/*and &_lib_dsn_outdsn. MUST be macro variables*/
/*already in existance.*/

%macro lib_dsn
(/*A two- or one-level dsn     */ _lib_dsn_indsn
,/*macro var name for lib in ''*/ _lib_dsn_outlib
,/*macro var name for dsn in ''*/ _lib_dsn_outdsn
);
data _null_;
indsn=symget('_lib_dsn_indsn');
if index(indsn,'.') lt 0 then do;
  outlib=scan(indsn,1,'.');
  outdsn=scan(indsn,2,'.');
end; else do;
  outlib='work';
  outdsn=indsn;
end;
call symput("&_lib_dsn_outlib.",outlib);
call symput("&_lib_dsn_outdsn.",outdsn);
run;
%mend  lib_dsn;
