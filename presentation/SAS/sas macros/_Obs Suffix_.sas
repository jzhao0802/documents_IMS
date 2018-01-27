/******************************************/
/*Auto-Detect current value of OBS= option*/
/*Assign to macro variable 'obs_suffix'****/
/******************************************/
options obs=max;
proc sql noprint;
        select setting into :obs_suffix
        from sashelp.voption
        where optname eq "%upcase(obs)";
quit;
%let obs_suffix=%sysfunc(tranwrd(%upcase(_o%sysfunc(compress(&obs_suffix.))),_OMAX,));
%put (BOL)obs_suffix(EOL)=(BOL)&obs_suffix.(EOL);
