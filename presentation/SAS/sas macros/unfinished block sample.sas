%let srn=5000;

proc means noprint nway
	data=pamlab.&client._stock_&prod._&modset_date.
		(keep=segment_mtx imsid)
;
class segment_mtx imsid;
output
	out=indsn_unq_key(drop=_type_ _freq_)
;
run;

proc contents noprint
	data=indsn_unq_key out=cont_indsn_unq_key
;run;

data _null_;
set  cont_indsn_unq_key;
call symput('obsmax',trim(left(put(nobs,8.))) );
run;
%put obsmax=&obsmax.(EOL);

%let sample_n
=%eval(
 (&srn.< 0)           *%sysevalf(&obsmax.)
+(0<=&srn.)*(&srn.<=1)*%sysfunc(min(%sysevalf(&srn.*&obsmax.,floor),&obsmax.))
+(1< &srn.)           *%sysfunc(min(%sysevalf(&srn.         ,floor),&obsmax.))
);
%put sample_n=&sample_n.(EOL);

proc surveyselect
	data=indsn_unq_key out=srs_key
	method=srs n=5000 selectall
;
strata segment_mtx ;
run;
