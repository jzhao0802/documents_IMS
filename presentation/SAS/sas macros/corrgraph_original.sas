/********************************************
 * Name: corrgraph                          *
 * Function: create scatter plot with       *
 * correlation coefficient displyed         *
 * most of gplot features are available.    *
 * %corgraph(data=, varx=, vary=)         *
 *******************************************/

/*
%corrgraph
-----------------------------------------------------------------------
  Create scatter plot with correlation coefficient displyed

Description
  This macro takes a data file and two numeric variables in the data 
  file. It calculates the correlation coefficient of the two 
  variables and displays the correlation coefficient of the 
  scatter plot of the two variables

Syntax
  corrgraph(data=, varx=, vary=, outfile= ' ', dev=gif373,
            title=' ',axis1=, axis2=, symbol=, gopt=, plot=, 
            labelx=' ', labely=' '); 

    data  - name of the SAS data file
    varx    - name of the variable for the x axis
    vary    - name of the variable for the y axis
    outfile - saves the graph as a .gif file with the given name
    dev     - specifies the device used for saving the graph, which
              determines the size of the graph
    title   - the title displayed at the top of the graph
    axis1   - options for the "axis1" statement (the x axis)
    axis2   - options for the "axis2" statement (the y axis)
    symbol  - options for the "symbol" statement
    gopt    - options for the "goptions" statement
    plot    - options for the "plot" statement 
    labelx  - label for the x variable
    labely  - label for the y variable

Examples
  %corgraph(data=temp, varx=x, vary=y)
  %corgraph(data=temp, varx=x, vary=y, symbol= i=r )
*/

%macro corrgraph(data=, varx=, vary=, outfile= ' ', dev=gif373,
                 title=' ',axisx=, axisy=, symbol=, gopt=, plot=, 
                 labelx=' ', labely=' ', byvar=); 

 proc corr data=&data.  noprint outp=_rcorr_;
  var &varx. &vary.;
 run;

 /*trim r to three decimal places or less*/
 data _null_; /*put correlation coefficient to r*/
   set _rcorr_;
   if _n_=4 then do;
   r = &vary. + 0;
   if r = 1 then r1 = 1;
   else if r = -1 then r1 = -1;
   else if r = 0 then r1 = 0;
   else  r1 = input(r, 5.);
   call symput('r1',left(r1));
   end;
 run;
 
  %if &title. ne ' ' %then %do;
      %let tle = &title.; 
	  %let tle2 = "Correlation Coefficient = &r1. ";
	  %end;
  %else %do;
      %let tle = "Correlation Coefficient = &r1."; 
      %let tle2 = " ";
	  %end;
  %if &labelx ne ' ' %then 
      %let lx=&labelx.;
  %else %let lx="&varx."; 

  %if &labely. ne ' ' %then   %let ly = &labely.; 
  %else %let ly="&vary."; 


/*proc sort data = &data.; by &byvar.; run;*/

 goptions reset = all;
 %if &outfile ne ' ' %then %do;
    goptions dev=&dev.;
    goptions gsfname=out;
	filename out &outfile.;
  %end;
 goptions &gopt.;
 axis1 label= (r=0 a=90) minor=none &axisx.;
 axis2 minor=none &axisy.;
 symbol c=yellow i=none v=circle &symbol.;

 proc gplot data=&data.;
/*   by &byvar.;*/
   plot &vary.*&varx.=1 /haxis=axisy vaxis=axisx cframe=lib &plot.;
   title &tle.;
   title2 &tle2.;
   label &varx. =&lx.;
   label &vary.=&ly.;
 run;
 quit;

goptions reset = all;
title;

%mend corrgraph;

