/*
	Transpose a data set EVEN when there are Multiple VAR variables
	Can automatically generate prefix
*/

%macro transpose
(/* Input Data Set Name       */ _transpose_indsn
,/* Outout Data Set Name      */ _transpose_outdsn
,/* BY Variables              */ _transpose_byvar
,/* ID Variables              */ _transpose_idvar
,/* VAR Variables             */ _transpose_var
,/* Prefix (auto or custom)   */ _transpose_prefix_type
,/* Custom Prefix Statements  */ _transpose_custom_prefix
);

proc sort 
	data=&_transpose_indsn.
	 out=_transpose_indsn_sorted_
;
by 
	&_transpose_byvar.
	&_transpose_idvar.
  &_transpose_var.
;
run;

%if %scan(&_transpose_var.,2) ne  %then %do;
    proc transpose
    	data=_transpose_indsn_sorted_
    	 out=_transpose_indsn_t_
    ;
    by 
    	&_transpose_byvar.
    	&_transpose_idvar.
    ;
    var 
      &_transpose_var.
    ;
    run;

    proc datasets library=work;
    delete _transpose_indsn_sorted_;run;
    quit;

    proc contents noprint
    	data=_transpose_indsn_t_
    	 out=cont_transpose_indsn_t
    ;
    run;

    %let _transpose_idvar_q=%upcase(%sysfunc(tranwrd(&_transpose_idvar.,%str( ),%str(","))) );

    proc sql noprint;
    select distinct
    	"trim(left("||trim(left(name))||"))"
    	into :_transpose_idvar_tl separated by " "
    from cont_transpose_indsn_t
    where upcase(name) in ("&_transpose_idvar_q.")
    ;quit;

    data _transpose_indsn_tp_;
    set  _transpose_indsn_t_;
    by 
    	&_transpose_byvar.
    	&_transpose_idvar.
    ;

    %if %sysfunc(upcase(&_transpose_prefix_type.))=AUTO %then %do;
    	_transpose_prefix_=trim(left(_name_))||"_"||&_transpose_idvar_tl.;
    %end;
    %else %do;
    	&_transpose_custom_prefix.
    %end;

    drop
    	&_transpose_idvar.
    	_name_
    ;
    run;

    proc datasets library=work;
    delete _transpose_indsn_t_;run;
    quit;

    proc transpose
    	data=_transpose_indsn_tp_
    	 out=&_transpose_outdsn.
    ;
    by &_transpose_byvar.;
    id _transpose_prefix_;
    var col1;
    run;

    proc datasets library=work;
    delete _transpose_indsn_tp_;run;
    quit;
  %end;
%else %do;
    proc transpose
    	data=_transpose_indsn_sorted_
    	 out=&_transpose_outdsn.
    prefix=&_transpose_idvar._
    ;
    by 
    	&_transpose_byvar.
    ;
    id
    	&_transpose_idvar.
    ;
    var 
      &_transpose_var.
    ;
    run;

    proc datasets library=work;
    delete _transpose_indsn_sorted_;run;
    quit;
  %end;
%mend  transpose;

