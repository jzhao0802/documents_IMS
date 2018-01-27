/** The objective of the following macro is to execute the
 same task for each string(could be year,spec);  */

%MACRO ForEach(Obj, ObjList, SAScode, DeLim=%str(, ), DocMode= );
  %* Note: If ObjList contains comma delimiters, enclose in bquote();
  %* Note: SAScode may include macro calls but not macro control code;
  %* Note: Always enclose SAScode in nrstr();
  %local DMode;
  %let DMode=&DocMode;
  %if (&DMode=) %then %do;
    %global mode;
	%if not(&mode=) %then %let DMode=&mode;
  %end;
  %if (%upcase(&DMode)=TEST or %upcase(&DMode)=DEBUG)
    %then %do;
      %put * NOTE: Macro ForEach invoked with arguments:;
      %put *             Obj=&Obj;
      %put *         ObjList=&ObjList;
      %put *         SAScode=&SAScode;
      %put *           DeLim=&DeLim;
	  %put * DocMode or Mode=&DMode;
    %end;
  %if (&Obj=) %then %do;
      %put * NOTE: Macro ForEach called with Obj undefined;
      %let Obj=Obj;
    %end;
  %if (%bquote(&ObjList)=) %then
      %put * NOTE: Macro ForEach called with ObjList empty;
  %else %do;
    %local &Obj;
    %local _i;
    %let _i=1;
    %do %while(not(%qscan(&ObjList,&_i,&Delim)=));
      %let &Obj=%scan(&ObjList,&_i,&Delim);
        %unquote(&SAScode)
      %let _i=%eval(&_i+1);
    %end;
  %end;
%mend;
