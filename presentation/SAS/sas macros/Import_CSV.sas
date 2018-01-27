PROC IMPORT OUT= WORK.TEST_IMPORT 
            DATAFILE= "C:\Documents and Settings\lsu\Desktop\Cymbalta Pi
lot\June National w Enhancements\data\dump_XPO_lsu.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
