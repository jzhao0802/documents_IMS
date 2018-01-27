
/* Imports Payer Model 
 1. prod is name of the product (from R code)
 2. myext is the extension where the imported data is to be stored */


%macro Import_Payer_Model(prod,myext);

PROC IMPORT OUT= DataDir.Aggregate_&prod 
            DATAFILE= "%str(&myext)\%str(&prod)%str(_Aggs.csv)" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

data DataDir.Aggregate_&prod;
	set  DataDir.Aggregate_&prod;
	iter=_N_;
run;

Data DataDir.Aggregate_&prod (Drop=alpha);
	set DataDir.Aggregate_&prod;
	if alpha_1=. then alpha_1=alpha;
	if mu_1=. then mu_1=mu;
run; 


PROC IMPORT OUT= DataDir.Individual_&prod 
            DATAFILE= "%str(&myext)\%str(&prod)%str(_Inds.csv)"
	DBMS=CSV REPLACE;
    GETNAMES=YES;
    DATAROW=2; 
RUN;

Data DataDir.Individual_&prod (Drop=int);
	set DataDir.Individual_&prod;
	if int_1=. then int_1=int;
run; 

%mend Import_Payer_Model;


/* Imports Physician Model 
1. prod is name of the product (from R code)
2. myext is the extension where the imported data is to be stored */

%macro Import_Physician_Model(prod,myext);

PROC IMPORT OUT= DataDir.Aggregate_&prod 
            DATAFILE= "%str(&myext)\%str(&prod)%str(_Aggs.csv)" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

data DataDir.Aggregate_&prod;
	set  DataDir.Aggregate_&prod;
	iter=_N_;
run;

Data DataDir.Aggregate_&prod (Drop=alpha mu delta);
	set DataDir.Aggregate_&prod;
	if alpha_1=. then alpha_1=alpha;
	if mu_1=. then mu_1=mu;
	if delta_1=. then delta_1=delta;
run; 

PROC IMPORT OUT= DataDir.Individual_&prod 
            DATAFILE= "%str(&myext)\%str(&prod)%str(_Inds.csv)"
DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

Data DataDir.Individual_&prod (Drop=int);
	set DataDir.Individual_&prod;
	if int_1=. then int_1=int;
run; 

%mend Import_Physician_Model;

/* Imports Physician Model 
1. prod is name of the product (from R code)
2. myext is the extension where the imported data is to be stored */

%macro Import_Physician_Model2(prod,myext);

PROC IMPORT OUT= DataDir.Aggregate_&prod 
            DATAFILE= "%str(&myext)\%str(&prod)%str(_Aggs.csv)" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

data DataDir.Aggregate_&prod;
set  DataDir.Aggregate_&prod;
iter=_N_;
run;

Data DataDir.Aggregate_&prod (Drop=/*alpha*/ mu delta);     * Modified by hqxu on 022312;
set DataDir.Aggregate_&prod;
if mu_1=. then mu_1=mu;
if delta_1=. then delta_1=delta;
run; 

PROC IMPORT OUT= DataDir.Individual_&prod 
            DATAFILE= "%str(&myext)\%str(&prod)%str(_Inds.csv)"
DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

Data DataDir.Individual_&prod (Drop=int);
set DataDir.Individual_&prod;
if int_1=. then int_1=int;
run; 

%mend Import_Physician_Model2;



/* Computes Base Utilities for Payer Model
1. scendata= data set containing scenarios
2. iterations= # of iterations desired 
3. AggData= Sas data set containing draws for Aggregate Parameters
4. Prod=  name of product
5. tiers= # of Coverage Levels in model
6. atts= # of attributes in model
7. Keepers= Variables that describe scenarios (e.g., prices, and clinical data) 
*/ 

%macro BaseUtilities(scendata,iterations,Aggdata,prod,tiers,atts,keepers);

%let tiers2 = %eval(&tiers -1);
%let tier_atts = %eval((&tiers2)*&atts);

Data Scenario_Merge;
	set &scendata;
	do iter=1 to &iterations;
	output;
	end;
run;

Proc sort;
	by iter scenario;
run;

Data DataDir.Base_Utilities_&prod (keep=iter scenario &keepers u1-u&tiers2);
	Merge &Aggdata Scenario_Merge;
	by iter;
	Array A(&tier_Atts) alpha_1-alpha_&tier_Atts;
	*Array B(&atts) x1-x&atts;
	Array B(&atts) &keepers; * Modified by hqxu on 013012;
	Array C(&tiers2) u1-u&tiers2;
	Array D(&tiers2) mu_1-mu_&tiers2;

/* Loop Through Tiers and Compute Utilities (except reference) */

	do i=1 to &tiers2;
		C(i)=D(i);
		do j=1 to &atts;
			C(i)=C(i)+A(j+&atts*(i-1))*B(j);
		end;
	end;

run;

%mend BaseUtilities;


/* Computes Base Utilities for Payer Model
1. scendata= data set containing scenarios
2. iterations= # of iterations desired 
3. AggData= Sas data set containing draws for Aggregate Parameters
4. Prod=  name of product
5. tiers= # of Coverage Levels in model
6. atts= # of payer type attributes in model
7. atts2= # of product attributes in model
8. Key= Variables defining inclusion of product attributes for each product 
9. Keepers= Variables that describe scenarios (e.g., prices, and clinical data) 
*/ 


%macro BaseUtilities_physician(scendata,iterations,Aggdata,prod,tiers,atts,atts2,Key,keepers);

%let tiers2 = %eval(&tiers -1);
%let tier_atts = %eval((&tiers2)*&atts);
%let tier_atts2 = %eval((&tiers)*&atts2);

Data Scenario_Merge;
set &scendata;
do iter=1 to &iterations;
output;
end;
run;

Proc sort;
by iter scenario;
run;

Data DataDir.Base_Utilities_&prod (keep=iter scenario &keepers u1-u&tiers);
Merge &Aggdata Scenario_Merge;
by iter;
Array A(&tier_Atts) alpha_1-alpha_&tier_Atts;
Array B(&atts) x1-x&atts;
Array C(&tiers) u1-u&tiers;
Array D(&tiers2) mu_1-mu_&tiers2;
Array E(&atts2) delta_1-delta_&atts2;
Array F(&atts2) z1 - z&atts2;
Array G(&tier_atts2) &Key;
/* Loop Through Tiers and Compute Utilities based On Environmental Variables (except reference) */


do i=1 to &tiers2;
C(i)=D(i);
do j=1 to &atts;
C(i)=C(i)+A(j+&atts*(i-1))*B(j);
end;
end;

/* Loop Through and Add In Product Specific Coefficients/Attributes */

C(&tiers)=0;

do i=1 to &tiers;
do j=1 to &atts2;
C(i)=C(i)+E(j)*F(j)*G(j+&atts2*(i -1) );
end;
end;


run;

%mend BaseUtilities_physician;


/* prods=tiers=11, atts =10 , atts2=5 */

/* 9 Arguments */ 

%macro BaseUtil_local(scendata,iterations,Aggdata,prod,tiers,atts,atts2,Key,keepers);

/* tiers2=10, tieratts=10&10=100, tier_atts2=11*5=55 */
%let tiers2 = %eval(&tiers -1);
%let tier_atts = %eval((&tiers2)*&atts);
%let tier_atts2 = %eval((&tiers)*&atts2);

Data Scenario_Merge;
set &scendata;
do iter=1 to &iterations;
output;
end;
run;

Proc sort;
by iter scenario;
run;

Data Base_Utilities_&prod (keep=iter scenario &keepers u1-u&tiers);
Merge &Aggdata (in=ina) Scenario_Merge (in=inb);
by iter;
if ina and inb;
Array A(&tier_Atts) alpha_1-alpha_&tier_Atts;
Array B(&atts) x1-x&atts;
Array C(&tiers) u1-u&tiers;
Array D(&tiers2) mu_1-mu_&tiers2;
Array E(&atts2) delta_1-delta_&atts2;
Array F(&atts2) z1 - z&atts2;
Array G(&tier_atts2) &Key;
/* Loop Through Tiers and Compute Utilities based On Environmental Variables (except reference) */


do i=1 to &tiers2;
C(i)=D(i);
do j=1 to &atts;


C(i)=C(i)+A(j+&atts*(i-1))*B(j);
end;
end;

/* Loop Through and Add In Product Specific Coefficients/Attributes */


C(&tiers)=0;
do i=1 to &tiers;
do j=1 to &atts2;


C(i)=C(i)+E(j)*F(j)*G(j+&atts2*(i -1) );

end;
end;

run;

%mend BaseUtil_local;


/* Computes Base Utilities for Payer Model
1. scendata= data set containing scenarios
2. iterations= # of iterations desired 
3. AggData= Sas data set containing draws for Aggregate Parameters
4. Prod=  name of product
5. tiers= # of Coverage Levels in model
6. atts= # of payer type attributes in model
7. atts2= # of product attributes in model
8. Key= Variables defining inclusion of product attributes for each product 
9. Keepers= Variables that describe scenarios (e.g., prices, and clinical data) 
*/ 


%macro BaseUtilities_physician2(scendata,iterations,Aggdata,prod,tiers,atts2,Key,keepers);

%let tiers2 = %eval(&tiers -1);
%let tier_atts2 = %eval((&tiers)*&atts2);

Data Scenario_Merge;
set &scendata;
do iter=1 to &iterations;
output;
end;
run;

Proc sort;
by iter scenario;
run;

Data DataDir.Base_Utilities_&prod (keep=iter scenario &keepers u1-u&tiers);
Merge &Aggdata Scenario_Merge;
by iter;
Array C(&tiers) u1-u&tiers;
Array D(&tiers2) mu_1-mu_&tiers2;
Array E(&atts2) delta_1-delta_&atts2;
Array F(&atts2) z1 - z&atts2;
Array G(&tier_atts2) &Key;
/* Loop Through Tiers and Compute Utilities based On Environmental Variables (except reference) */


do i=1 to &tiers2;
C(i)=D(i);
end;

/* Loop Through and Add In Product Specific Coefficients/Attributes */

C(&tiers)=0;

do i=1 to &tiers;
do j=1 to &atts2;
C(i)=C(i)+E(j)*F(j)*G(j+&atts2*(i -1) );
end;
end;


run;

%mend BaseUtilities_physician2;



/* 
1. prod= Name of Product
2. IDs= # of IDs in data set 
3. tiers=# of Tier levels 
3. keepers= Variable that are to be retained
*/

%macro IND_Probabilities(prod,IDs,tiers, keepers);

%let tiers2 = %eval(&tiers -1);

Data temp;
set DataDir.Base_Utilities_&prod;
do ID=1 to &IDs;
output;
end;
run;

Proc Sort data=Temp;
by iter ID;
run;

Data Temp2;

Merge DataDir.Individual_&prod (in = in1) Temp(in = in2);

by iter ID;

if in1 and in2;


Array A(&tiers2) u1-u&tiers2;
Array B(&tiers2) int_1-int_&tiers2;
Array C(&tiers2) w1-w&tiers2;
Array D(&tiers) p1-p&tiers;

w=1;

do i= 1 to &tiers2;
A(i)=A(i)+ B(i);
C(i)=exp(A(i));
w=w+C(i);

end;

do i= 1 to &tiers2;
D(i)=C(i)/w;
end;

D(&tiers)=1/w;

run;

Proc Sort data=temp2;
by ID scenario;
run;

Proc means data=temp2 noprint ;
var p1-p&tiers;
by ID scenario;
ID &keepers;
output out=DataDir.IND_Probs_&prod mean=; 
run;


%mend IND_Probabilities;

/* 
1. prod= Name of Product
2. IDs= # of IDs in data set 
3. tiers=# of Tier levels 
3. keepers= Variable that are to be retained
*/

%macro IND_Probabilities_physician(prod,IDs,tiers, keepers);

%let tiers2 = %eval(&tiers -1);

Data temp;
set DataDir.Base_Utilities_&prod;
do ID=1 to &IDs;
output;
end;
run;


Proc Sort data=Temp;
by iter ID;
run;

Data Temp2;

Merge DataDir.Individual_&prod (in = in1) Temp(in = in2);


by iter ID;

if in1 and in2;

int_&tiers=0;

Array A(&tiers) u1-u&tiers;
Array B(&tiers) int_1-int_&tiers;
Array C(&tiers) w1-w&tiers;
Array D(&tiers) p1-p&tiers;

w=0;


do i= 1 to &tiers;
A(i)=A(i)+ B(i);
C(i)=exp(A(i));
w=w+C(i);

end;

do i= 1 to &tiers;
D(i)=C(i)/w;
end;

run;

Proc Sort data=temp2;
	by ID scenario;
run;

Proc means data=temp2 noprint ;
	var p1-p&tiers;
	by ID scenario;
	ID &keepers;
	output out=DataDir.IND_Probs_&prod mean=; 
run;

%mend IND_Probabilities_physician;

/* Product Segment Level Payer Database (/

/* Prod        = Name of Product(s) 0 Starting data in Ind_Probs_&prod */
/* pvars       = Variables containing category probabilities - Could be for single product or cross product */ 
/* weightfile  = Sas dataset containing segment weights and prodfiling variables */
/* profilevars = Profiling variables - sumamries produced by this and scenario variables */
/* attributes  = variables to carry along into final database */
/* myext       = directory into which final dataset should be written */

%macro Segment_Probabilities(prod,pvars,weightfile,profilevars,attributes,myext);

/* Merge probabilities and weighting/profilingfile */

data DataDir.IND_Probs_&prod;
	merge DataDir.IND_Probs_&prod &weightfile;
	by ID; 
run;

/* Sort by scenarios and profilingvariables */

Proc sort data=DataDir.IND_Probs_&prod;
	by scenario &profilevars;
run;

Proc Means data=DataDir.IND_Probs_&prod noprint;
	var &pvars;
	by scenario &profilevars;
	ID &attributes;
	weight wt;
	output out=datadir.weighted_Probs_&prod mean=;
run;

Proc Means data=DataDir.IND_Probs_&prod noprint;
	var wt;
	by scenario &profilevars;
	ID &attributes;
	output out=tempweight sum=;
run;

Data datadir.weighted_Probs_&prod (Drop=_TYPE_ _Freq_);
	merge datadir.weighted_Probs_&prod tempweight;
	by scenario &profilevars;
run;

PROC EXPORT DATA= datadir.weighted_Probs_&prod  
            OUTFILE= "%str(&myext)\%str(&prod)%str(_weighted_Probs.csv)" 
            DBMS=CSV REPLACE;
RUN;

%mend;



/* 
1. prod= Name of Product
2. IDs= # of IDs in data set 
3. tiers=# of Tier levels 
3. keepers= Variable that are to be retained
*/

%macro IND_Prob_ph_local(prod,IDs,tiers, keepers);

%let tiers2 = %eval(&tiers -1);

Data temp;
set Base_Utilities_&prod;
do ID=1 to &IDs;
output;
end;
run;


Proc Sort data=Temp;
by iter ID;
run;

Data Temp2;

Merge Datadir.Individual_&prod (in = in1) Temp(in = in2);


by iter ID;

if in1 and in2;

int_&tiers=0;

Array A(&tiers) u1-u&tiers;
Array B(&tiers) int_1-int_&tiers;
Array C(&tiers) w1-w&tiers;
Array D(&tiers) p1-p&tiers;

w=0;


do i= 1 to &tiers;
A(i)=A(i)+ B(i);
C(i)=exp(A(i));
w=w+C(i);

end;

do i= 1 to &tiers;
D(i)=C(i)/w;
end;

run;

Proc Sort data=temp2;
by ID scenario;
run;

Proc means data=temp2 noprint ;
var p1-p&tiers;
by ID scenario;
ID &keepers;
output out=IND_Probs_&prod mean=; 
run;

%mend IND_Prob_ph_local;









/* 
1. prod= Name of Product
2. IDs= # of IDs in data set 
3. tiers=# of Tier levels 
3. keepers= Variable that are to be retained
*/

%macro IND_Probabilities_phys_lipitor(prod,IDs,tiers, keepers);

%let tiers2 = %eval(&tiers -1);

/*
Data temp;
set DataDir.Base_Utilities_&prod;
do ID=1 to &IDs;
output;
end;
run;


Proc Sort data=Temp;
by iter ID;
run;
*/

Data Temp2;

Merge Individual_&prod (in = in1) Base_Utilities_&prod(in = in2);


by iter /*ID*/;

if in1 and in2;

int_&tiers=0;

Array A(&tiers) u1-u&tiers;
Array B(&tiers) int_1-int_&tiers;
Array C(&tiers) w1-w&tiers;
Array D(&tiers) p1-p&tiers;

w=0;


do i= 1 to &tiers;
A(i)=A(i)+ B(i);
C(i)=exp(A(i));
w=w+C(i);

end;

do i= 1 to &tiers;
D(i)=C(i)/w;
end;

run;

Proc Sort data=temp2;
by /*ID*/ scenario;
run;

Proc means data=temp2 noprint ;
var p1-p&tiers;
by /*ID*/ scenario;
ID &keepers;
output out=DataDir.IND_Probs_&prod mean=; 
run;

%mend IND_Probabilities_phys_lipitor;




/* Weights physician Output for inclusion in the model 
1. prod1= Name of Product
2. Weighting file containing weights for each physician-patient type 
3. pa_types=# of Patient types to be processed
4. pa_typer_var = Variable containing patient type that corresponds to output in ind_probs data set 
5. wt_vars = Variables containing physician weights for each patient type
6. Spec=Variable containing specialty information
7. Keepers=Variable to be retained
*/

%macro Weighted_Physicians (prod1, weightfile,Share_vars, pa_types, pa_type_var,wt_vars, spec, Keepers);

/* Sort Individual Probabilities by Specialty and ID */

proc sort data = work.IND_Probs_&prod1 out = IND_Probs_&prod1;
   by &spec ID;
run;

/* Sort weighting file by Specialty and ID */

proc sort data = datadir.&weightfile out = datadir.&weightfile;
   by &spec ID;
run;


/* Merge Indvidual Probabilities and weights */

data IND_Probs_&prod1;
   merge work.IND_Probs_&prod1 (in = in1) datadir.&weightfile (in = in2);
   by &spec ID;
   if ^in1 or ^in2 then abort;
run;

/* Create Weight Variable for Processing */

data IND_Probs_&prod1;
set IND_Probs_&prod1;
Array A(&pa_types ) &wt_vars;

do i= 1 to &pa_types;
if &pa_type_var=i  then Processing_weight=A(i);
end;

Run;

/* Sort Data For Processing*/

proc sort data = IND_Probs_&prod1;
   by &spec scenario;
run;


Proc means data=IND_Probs_&prod1 noprint ;
var &Share_vars;
by &spec scenario;
weight Processing_weight;
ID &Keepers;
output out=DataDir.&prod1._IND_Probs mean=; 
run;

%mend Weighted_Physicians;


%macro adjust_data(in_data, othattr,desc,attrname,lvl1, lvl2, factor,varname,out_data);

proc sort data = &in_data out = &out_data;
   by &othattr ID &desc &attrname;
run;

data &out_data.;
   set &out_data.;
   by &othattr ID &desc &attrname;

   retain temp;
   if &attrname = &lvl1 then temp = &varname;
   if &attrname = &lvl2 then do;
      if &varname > temp then &varname = temp*&factor.;
   end;
   drop temp;
run;

proc sort data = &out_data;
   by ID Scenario;
run;

%mend;



/*
data DataDir.&prod1._IND_Probs (drop = _TYPE_ _FREQ_);
   set DataDir.&prod1._IND_Probs;

   pr1 = p1;
   pr2 = p6;
   pr3 = p7;
   pr4 = p12;
   pr5 = p8;
   pr6 = p2;
   pr7 = p3;
   pr8 = p4;
   pr9 = p9;
   pr10 = p10;
   pr11 = p11;
   pr12 = p5;

   drop p1-p12;

   specialty = &spec;
   st = &stage;
run;

*/

