/*****************************************/
/*Auto-Detect OS filename of envelop code*/
/*Assign to macro variable 'this_code'   */
/*****************************************/
%let this_code=%sysfunc(tranwrd(%sysget(SAS_EXECFILEPATH),.sas,));
%put (BOL)this_code(EOL)=(BOL)&this_code.(EOL);

