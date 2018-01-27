options remote=psc01 comamid=tcp;
filename rlink "D:\psc01.txt";   /****change location where PSC01.txt stores********/
signon psc01;

/*rsubmit;*/
*  RSUBMIT;
libname current "";
%let libn = SASOUT;
proc sql;
create table this_counts as
select  "&libn.."||memname as filename,nobs, nvar from dictionary.tables
where libname='CURRENT';/* should be UPCASE */
quit;
*  RSUBMIT;
proc print data = this_counts(obs = 10);run;


	PROC EXPORT DATA=this_counts
	OUTFILE= "/re_cnt.CSV"
	DBMS=CSV REPLACE;
	RUN;

*signoff;
