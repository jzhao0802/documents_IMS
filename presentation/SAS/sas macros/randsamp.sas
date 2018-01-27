 options mlogic symbolgen;

%MACRO RANDsamp(inFILE,SIZE,outfile);                          
DATA RANDOM;                                       
     SAMPSIZE=&SIZE;                               
         DO WHILE(SAMPSIZE>0);                     
            READIT+1;                              
            IF UNIFORM(0)<SAMPSIZE/TOTOBS THEN DO; 
               OUTPUT;                             
               SAMPSIZE=SAMPSIZE-1;                
            END;                                   
         TOTOBS=TOTOBS-1;                          
         END;                                      
         STOP;                                     
         SET &inFILE POINT=_N_ NOBS=TOTOBS; RUN;         
DATA &outfile;                                     
     SET RANDOM;                                   
     SET &inFILE POINT=READIT;    
RUN;                   
                                                   
%MEND RANDsamp;  

  
