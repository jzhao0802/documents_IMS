/*
Make informat from regular SAS data set
Lingyun Su
*/
%MACRO MAKE_INFORMAT
(/* INFORMAT NAME       */ INFMTNM=
,/* INPUT DATA SET NAME */ INDSN=
,/* START VARIABLE NAME */ START=
,/* LAVEL VARIABLE NAME */ LABEL=
,/* OTHER HANDLING Y/N  */ HLO=N
,/* OTHER HANDLING LABEL*/ HLOLB=
,/* OUTPUT DATA SET NAME*/ ODSN=CTRL
,/* CLEAN UP TEMP DATA  */ CTEMP=Y
,/* Make Format Y/N 	*/ MAKE=Y
,/* CLEAN UP CTRL DATA  */ CCTRL=Y
);
OPTIONS MLOGIC SYMBOLGEN MERROR MPRINT;
PROC CONTENTS DATA=&INDSN. NOPRINT
     OUT=CONT_TEMP(KEEP=NAME TYPE LENGTH);
RUN;
PROC SQL NOPRINT;
SELECT   CASE TYPE WHEN 1 THEN 'I' ELSE 'J' END
INTO :LABEL_TYPE
FROM CONT_TEMP
WHERE UPCASE(TRIM(LEFT(NAME)))=UPCASE("&LABEL.")
;
SELECT    TRIM(LEFT(PUT(LENGTH,8.)))
    ,CASE TYPE WHEN 1 THEN ' ' ELSE '$' END
INTO :LABEL_LEN, :LABEL_FMT
FROM CONT_TEMP
WHERE UPCASE(TRIM(LEFT(NAME)))=UPCASE("&LABEL.")
;
QUIT;
PROC SORT NODUPKEY DATA=&INDSN. OUT=CONT_TEMP;
BY &START. &LABEL.;
RUN;

DATA &ODSN.;
   LENGTH LABEL &LABEL_FMT. &LABEL_LEN. ;

     SET CONT_TEMP(RENAME=(&START.=START &LABEL.=LABEL)
                   KEEP=&START. &LABEL.) END=LAST;
     RETAIN FMTNAME "&INFMTNM." TYPE "&LABEL_TYPE.";

     OUTPUT;
%IF (&HLO.=Y OR &HLO.=y) %THEN %DO;
     IF LAST THEN DO;
      HLO='O';
        LABEL="&HLOLB.";
        OUTPUT;
   END;
%END;
RUN;
%IF (&CTEMP.=Y OR &CTEMP.=y) %THEN %DO;
  PROC DELETE DATA=CONT_TEMP;
  RUN;
%END;
%IF (&MAKE.=Y OR &MAKE.=y) %THEN %DO;
  PROC FORMAT CNTLIN=&ODSN.;
  RUN;
%END;
%IF (&CCTRL.=Y OR &CCTRL.=y) %THEN %DO;
  PROC DELETE DATA=&ODSN.;
  RUN;
%END;
%MEND MAKE_INFORMAT;
