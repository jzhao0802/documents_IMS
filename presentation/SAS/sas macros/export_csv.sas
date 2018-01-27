PROC EXPORT DATA= WORK.DET_MD_SUM 
            OUTFILE= "\\plyvnas02\lifeline\DPSA\det_md_sum.csv" 
            DBMS=CSV LABEL REPLACE;
     PUTNAMES=YES;
RUN;
