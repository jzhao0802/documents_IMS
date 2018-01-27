/***********************************************************/
/* Make a sub directory when it does not exist;            */
/* Do nothing when the sub directory already exists.       */
/* _subpath_ is REQUIRED and MUST be the full path string. */
/***********************************************************/
%macro make_subpath(_subpath_);
%LET MAKE_SUBPATH
=md %str("&_subpath_.");
%put make_subpath=&make_subpath.;

options noxwait;
data _null_;
format command $255.;
file print;
rc=filename("test","&_subpath_.");
rce=fexist("test");
put "'rce=1' means that &_subpath_. EXISTS already.";
put;
put rce=;
put;
if rce ne 1 then do;
	put "&_subpath_. does NOT exist. Make directory.";
	command=symget('MAKE_SUBPATH') ;
	call system(command);
end;
else do;
	put "Skip making directory.";
end;
run;

options xwait;
%mend  make_subpath;

/*EXAMPLE:*/
/*%let sub*/
/*=\\Plyvnas01\statservices\CustomStudies\Sales & Account Management\Lilly SASI Pilot - Cymbalta\Q1 National w Enhancements\sas codes\test\1234;*/
/*%make_subpath(&sub.);*/
