/*OBSERVATION RESTRICTION OTHER THAN FORMATS*/
%let obs_limit=max;  /*****TEST SET=5000*******/
%let obs_suffix=%sysfunc(tranwrd(%upcase(_o&obs_limit.),_OMAX,));
%put obs_suffix=(BOL)&obs_suffix.(EOL);

proc sql noprint;
        select setting into :cv_obs
        from sashelp.voption
        where optname eq "%upcase(obs)";
quit;
%let cv_obs=%sysfunc(trim(&cv_obs.));
%put cv_obs=&cv_obs.(EOL);
%macro obs_suffix(m_obs_suffix_obs_limit=);
%if %length(&m_obs_suffix_obs_limit.)=0 %then %do;
*When &m_obs_suffix_obs_limit. is empty use Current Value of OBS= system option;
	proc sql noprint;
	        select setting into :cv_obs
	        from sashelp.voption
	        where optname eq "%upcase(obs)";
	quit;
	%let cv_obs=%sysfunc(trim(&cv_obs.));
%end;
%else %do;
	%let cv_obs=%sysfunc(trim(&m_obs_suffix_obs_limit.));
%end;
%put &cv_obs.;
%mend  obs_suffix;

options symbolgen mlogic merror;
%let test=%obs_suffix;
%put test=&test.(EOL);
