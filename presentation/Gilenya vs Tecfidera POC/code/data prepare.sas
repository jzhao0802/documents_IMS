
%let path = C:\Documents and Settings\ymgui\My Documents\HEOR\Gilenya vs Tecfidera response prediction\data;
libname fact "&path.";

data pred_analytics_file;
	set fact.pred_analytics_file;
run;

proc export data=pred_analytics_file outfile="&path.\pred_analytics_file.csv"
	dbms=csv replace;
run;

proc means data=pred_analytics_file  P25 P50 P75;
	var age;
run;
