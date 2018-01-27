/*Create a Primary Key Integrity Constraint*/
/*named '_pk_pknm' referring to variables listed*/
/*in '_pk_pkvars' on data set '_pk_indsn'.*/

/*All parameters required*/
/*REFER to %lib_dsn*/

%macro pk
(/*input dataset name*/ _pk_indsn
,/*primary key name  */ _pk_pknm
,/*primary key vars  */ _pk_pkvars
);
%if %index(&_pk_indsn.,.) lt 0 %then %do;
  %let _pk_inlib=%scan(&_pk_indsn.,1,.);
  %let _pk_indsn=%scan(&_pk_indsn.,2,.);
%end; %else %do;
  %let _pk_inlib=work;
%end;

proc datasets library=&_pk_inlib.;
modify &_pk_indsn.;
  ic create &_pk_pknm.=primary key(&_pk_pkvars.);
run;quit;
%mend  pk;
