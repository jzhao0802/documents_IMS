/* Include Formulary Focus Macros */

%include "P:\Pricing Studies\Macros\Formulary Focus Macros.sas";

/* Set Name of DataDirectory (Location of data and output of analysis), datasetname, and
   Extension for Datadirectory */ 

%let Datadirname=Datadir2;
%let Dataname=DEMO;
%let Directory=P:\Pricing Studies\Novartis Rasilez War Gaming\001 Data;

/* Set Datadirectory */

libname &Datadirname 'P:\Pricing Studies\Novartis Rasilez War Gaming\001 Data';


/* Rename Dataset (if needed)  */


Data Datadir2.DEMO;
	set Datadir2.ff_kjell2;
run;



/* Run Formulary Focus Analysis Macro  */
/* Note: Produces a series of outputs
/* A.  SAS DataSet &Datadirname.Mappeddata_&Dataname  - Contains initial data with
	   Merged in Mapping Information 
   B.  SAS DataSet  
*/
 

%FF_Analysis(&Datadirname, &Dataname,&Directory);


%Fill_Tiers(Datadir2.prod_demo,Datadir2.prod_demo);



Proc Freq;
table Prod_code*Product_name /list;
run;

Proc Freq;
table Prod_code*tier_num /list;
run;