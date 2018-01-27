PROC IMPORT OUT= WORK.dcorr 
            DATAFILE= "C:\Documents and Settings\lsu\My Documents\@My Pr
ojects\2009 - AZ Vimovo Market Structure Analysis\working\3 Market Struc
tures\Distance Test - v2.xls" 
            DBMS=EXCEL REPLACE;
     SHEET="distance"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
