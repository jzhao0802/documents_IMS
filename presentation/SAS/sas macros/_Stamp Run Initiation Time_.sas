/***********************************************************/
/*Auto-Detect OS system time at code current run initiation*/
/*Assign to macro variable 'time_stamp'*********************/
/***********************************************************/

%let _d=%sysfunc(date() );
%let _t=%sysfunc(time() );
%let time_stamp=
Y%substr(%sysfunc(year(&_d.)),3,2)M%sysfunc(month(&_d.))D%sysfunc(day(&_d.))h%sysfunc(hour(&_t.))m%sysfunc(minute(&_t.))
;
%put (BOL)time_stamp(EOL)=(BOL)&time_stamp.(EOL);
