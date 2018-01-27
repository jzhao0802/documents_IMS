%let RootFolder=Y:\Host\Communication of SAS Base; * change to your folder ;

/* example 2.1.1 Selecting Variables to Print */
proc print
  data = ex11_v2;
/*  var Country Area_wise;*/
run;

/* example 2.1.2 Add title and footnote to Print */

*options nodate linesize=80 pagesize=30 obs=10;

proc print
  data = ex11_v2;
  var Country Area_wise;

  title "Twenty-two countries' Area wise.";
  footnote "Area wise in sq km";
run;

/* example 2.1.3 Create html or pdf report */

options nodate linesize = 80 pagesize = 35 obs = max;

ods html file = "&RootFolder.\ex02-print\ex213.html";
proc print
  data = ex11;
  var Country Area_wise;

  title "Twenty-two countries' Area wise.";
  footnote "Area wise in sq km";
run;
ods html close;

ods pdf file = "&RootFolder.\ex02-print\ex213.pdf";
proc print
  data = ex11;
  var Country Area_wise;

  title "Twenty-two countries' Area wise.";
  footnote "Area wise in sq km";
run;
ods pdf close;

/* example 2.1.4 Customizing Text in Column Headings and format the variables */
proc print
  data = ex11 split = '*' n = 'Total Record = ' obs = 'Observation*Number*===========';
  var Country Area_wise Populations;

  label
    country='Country Name*============'
    Area_wise='The proportion*of the country*=========='
    Populations='The number of*the populations*=============='
  ;
   
  format Area_wise comma10. Populations comma10.;
  
  title "Twenty-two countries' Area wise.";
  footnote "Area wise in sq km";
run;

/* example 2.1.5 Creating Separate Sections of a Report for Groups of Observations */
options obs = max;
proc print
  data = ex11 split = '*' n = 'Total Record = ' obs = 'Observation*Number*===========';
  var Country Area_wise Populations;
  by dummy2;
  label
    country='Country Name*============'
    Area_wise='The proportion*of the country*=========='
    Populations='The number of*the populations*=============='
  ;

  format Area_wise comma10. Populations comma10.;

  title "Twenty-two countries' Area wise.";
  footnote "Area wise in sq km";
run;
