/*Bucket by volume*/
%MACRO DECILE(DATASET,VAR,BREAKS,OUTVAR);

%LET B=%EVAL(&BREAKS-1);

PROC SORT DATA=&DATASET; BY &VAR;

DATA COPY;
  SET &DATASET;
    DEC=&VAR;
    DUMMY=1;
    KEEP DEC &VAR DUMMY;

PROC SUMMARY DATA=COPY NWAY;
   CLASS &VAR DUMMY;
   VAR DEC;
   OUTPUT OUT=DECC(DROP=_TYPE_) SUM=;

DATA DECC;
   SET DECC;
   IF _N_ EQ 1 THEN CUM=0; RETAIN CUM;
   CUM=CUM+DEC;

PROC SORT DATA=DECC; BY DESCENDING CUM;

DATA DECC;
  SET DECC;
  BY DESCENDING CUM;
  IF _N_ EQ 1 THEN CUMM=CUM; RETAIN CUMM;
  PCT=CUM/CUMM;

   ARRAY BREAK(&B);
   DO I=1 TO &B;
   BREAK(I)=I/&BREAKS;
   END;
   DROP I;

DATA DECC;
  SET DECC;
   ARRAY DIST(&B);
   ARRAY    B(&B) BREAK1-BREAK&B;
  DO I=1 TO &B;
    DIST(I)=ABS(PCT-B(I));
  END;
  DROP I;

PROC SUMMARY DATA=DECC NWAY;
  CLASS DUMMY;
  VAR DIST1-DIST&B;
  OUTPUT OUT=DIST(DROP=_TYPE_ _FREQ_) MIN=MDIST1-MDIST&B;

  /*
PROC PRINT;
  TITLE'MINIMUM DISTANCE FROM EACH BREAKPOINT';
  */

DATA DECC;
  MERGE DECC DIST;
  BY DUMMY;

DATA BREAKS;
  SET DECC;
  ARRAY  D(&B)  DIST1- DIST&B;
  ARRAY  M(&B) MDIST1-MDIST&B;
 DO DECIL=1 TO &B;
    IF D(DECIL) EQ M(DECIL) THEN OUTPUT BREAKS;
 END;

  /*
PROC PRINT DATA=BREAKS;
   TITLE'SELECTED BREAK RECORDS';
  */

PROC SORT DATA=BREAKS NODUPKEY; BY DECIL &VAR;

DATA BREAKS;
  SET BREAKS;
  ARRAY SPLIT(&B);
  DO I=1 TO &B;
  IF I EQ DECIL THEN SPLIT(I) = &VAR;
  END;

  /*
PROC PRINT DATA=BREAKS;
   TITLE'CREATION OF SPLIT VALUES';
     */

PROC SUMMARY DATA=BREAKS NWAY;
  CLASS DUMMY;
  VAR SPLIT1-SPLIT&B;
  OUTPUT OUT=SPLIT(DROP=_TYPE_ _FREQ_) SUM=;

DATA SPLIT;
   SET SPLIT;
   ARRAY S(&B) SPLIT1-SPLIT&B;
   DO I= 2 TO &B;
     IF S(I-1) EQ S(I) THEN S(I-1) = .;
   END;

PROC PRINT DATA=SPLIT;
   VAR SPLIT1-SPLIT&B;
   TITLE'DECILE BREAK POINTS';

DATA COPY;
  MERGE COPY SPLIT;
  BY DUMMY;

DATA COPY;
   SET COPY;
   ARRAY S(&B) SPLIT1-SPLIT&B;
   DONE=0;
   DO I=1 TO &B;
    IF &VAR LE S(I) AND DONE=0 THEN DO; &OUTVAR=I; DONE=1; END;
   END;
    IF DONE=0 THEN DO; &OUTVAR=&BREAKS; DONE=1; END;
	if &var eq 0 then &outvar=0;
    DROP I DONE SPLIT1-SPLIT&B;

PROC SUMMARY DATA=COPY;
  CLASS &OUTVAR;
  VAR DUMMY &VAR;
  OUTPUT OUT=CHECK SUM=;

DATA CHECK;
  SET CHECK;
  IF _N_ EQ 1 THEN TOT=&VAR; RETAIN TOT;
  PCT=ROUND(&VAR/TOT*100,.001);
  IF _TYPE_ EQ 1;

PROC PRINT;
   VAR &OUTVAR _FREQ_ &VAR PCT;
   SUM         _FREQ_ &VAR PCT;
  TITLE'COUNTS BY DECILE';

DATA COPY;
  SET COPY;
  KEEP &VAR &OUTVAR;

PROC SORT DATA=COPY NODUPKEY; BY &VAR &OUTVAR;

DATA &DATASET OOPS;
  MERGE &DATASET(IN=A) COPY(IN=B);
  BY &VAR;
  IF A AND B THEN OUTPUT &DATASET;
    ELSE OUTPUT OOPS;

PROC PRINT DATA=OOPS;
   TITLE1'RECORDS NOT MATCHED IN DECILING PROCESS';
   TITLE2'LOGIC ERROR MUST BE FIXED';

data &dataset;
  set &dataset;
  if &var eq . then &outvar=0;

proc print data=&dataset;
  where &var=.;

PROC FREQ DATA=&DATASET;
  TABLES &OUTVAR;
  TITLE;
run;


%MEND DECILE;