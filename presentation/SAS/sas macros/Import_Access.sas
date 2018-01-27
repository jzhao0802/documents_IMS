
PROC IMPORT OUT= SRCDATA.VARx_w_Alignment 
            DATATABLE= "VARx_w_ALIGNMENT" 
            DBMS=ACCESS REPLACE;
     DATABASE="C:\Documents and Settings\lsu\Desktop\Cymbalta Pilot\June National w Enhancements\raw data\VARx_w_Alignment.mdb"; 
     SCANMEMO=YES;
     USEDATE=NO;
     SCANTIME=YES;
RUN;
