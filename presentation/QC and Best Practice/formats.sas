

/*************************************************************************\
  PROGRAM     :03_create_cmf10_formats.sas                                                                  
  PURPOSE     :creates product cmf10 formats for usc,product,form,strength 
                       and product_group,therapy_class and broad_therapy_class                           
  CLIENT      :Gilead                            
  PROJECT     :HCV Tracking                
  PROGRAMMER  :Usha Iyer                                       
  CREATED     :06/29/2012 07/02/2012 07/16/2012 08/15/2012  09/12/2012 10/24/2012 11/24/2012 12/10/2012 01/18/2013 02/18/2013

  REVIEWER    :                  
  METHODOLOGIST:Elva Chu                    
  SAS VERSION :9.2  
  COMMENTS    :
\*************************************************************************/;

***************************************************************************;

/*rsubmit;*/
title1 'create_cmf10_formats.sas';


libname  studylib    ''; /* change yyyymm */
libname  library     ''; /* change yyyymm */

%let pasofdate=12jan2015;/* change -  product master file date */
%let prdmstrfl=studylib.product_master_&pasofdate.;
%let mktcmf10fl=studylib.mktdef_hcv_cmf10; 


options pageno=1 date symbolgen obs=max mprint;


proc datasets library=work kill;
run;

proc contents data=&mktcmf10fl.;
     run;
data mktdef_cmf10;
     set &mktcmf10fl.;
     run;
proc sort data=mktdef_cmf10 nodupkey;
     by cmf10;
     run;

%macro genfmt(inprods,fname,labelv,leng);

data  mktcmf10(keep=fmtname start end label);
      length label $&leng. start end $10;
      set &inprods end=eof;
      retain fmtname "&fname" type 'c';
      start=cmf10;
      end=cmf10;
      label=&labelv.;
      output;
      if eof then do;
      start='other';
      end='other';
      label=' ';
      output;
      end;
      run;
proc format cntlin=mktcmf10 library=library;
      run;

%mend genfmt;

%macro cmf10_formats(cmf10s);
***<<<   Create product name format;
data prodname (keep=fmtname start end label);
      length label $20 start end $10;
      set &cmf10s. end=eof;
      retain fmtname '$cmprd' type 'c';
      start=cmf10;
      end=cmf10;
      label=product;
      output;
      if eof then do;
      start='other';
      end='other';
      label=' ';
      output;
      end;
      run;
proc format cntlin=prodname library=library;
      run;

***<<<   Create form format;
data form (keep=fmtname start end label);
      length label $20 start end $10;
      set &cmf10s. end=eof;
      retain fmtname '$cmform' type 'c';
      start=cmf10;
      end=cmf10;
      label=form;
      output;
      if eof then do;
      start='other';
      end='other';
      label=' ';
      output;
      end;
      run;
proc format cntlin=form library=library;
      run;

***<<<   Create strength format;
data stren (keep=fmtname start end label);
      length label $20 start end $10;
      set &cmf10s. end=eof;
      retain fmtname '$cmstr' type 'c';
     start=cmf10;
     end=cmf10;
     label=strength;
      output;
      if eof then do;
      start='other';
      end='other';
      label=' ';
      output;
      end;
      run;
proc format cntlin=stren library=library;
      run;

***<<<   Create usc format;
data usc5 (keep=fmtname start end label);
     length label $5 start end $10;
     set &cmf10s. end=eof;
     retain fmtname '$cmusc' type 'c';
     start=cmf10;
    end=cmf10;
    label=usc;
     output;
     if eof then do;
     start='other';
     end='other';
     label=' ';
     output;
     end;
     run;
proc format cntlin=usc5 library=library;
     run;
%mend cmf10_formats;

%cmf10_formats(&prdmstrfl.);
run;

%genfmt(mktdef_cmf10,$cmprdg,product_group,20); 
%genfmt(mktdef_cmf10,$cmclas,therapy_class,10); 
%genfmt(mktdef_cmf10,$cmclasbr,broad_therapy_class,10);




run;

proc sort data=mktdef_cmf10 out=prods nodupkey;
     by cmf10;
     run;

proc freq data=prods;

     tables  usc*product*product_group*therapy_class*broad_therapy_class/list missing;
     title2 ' market products - ORIGINAL';
     run;

data check;
     length usc $5 product form strength product_group $20 therapy_class broad_therapy_class $10;
     set prods(keep=cmf10);
     therapy_class=put(cmf10,$cmclas.);
     broad_therapy_class=put(cmf10,$cmclasbr.);
     product=put(cmf10,$cmprd.);
     product_group=put(cmf10,$cmprdg.);
     form=put(cmf10,$cmform.);
     strength=put(cmf10,$cmstr.);
     usc=put(cmf10,$cmusc.);
run;
proc freq data=check;
     tables   usc*product*product_group*therapy_class*broad_therapy_class/list missing;
     title2 'market products - USING FORMATS';
run;
