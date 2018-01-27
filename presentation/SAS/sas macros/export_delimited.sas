PROC EXPORT DATA= WORK.Lantus_cp_cut 
            OUTFILE= "\\Plyvnas01\statservices\CustomStudies\Sales & Acc
ount Management\SA Insulin Decile\07 Analysis & Deliverables\Call Planni
ng\Back From Client\Lantus_Call_Plan_STPC4_and_STSP4_2010MAR05.txt" 
            DBMS=DLM REPLACE;
     DELIMITER='7C'x; 
RUN;
