/*Audit data set '_audit_ic_indsn' and Capture Rejected*/
/* Obs if its obs do not exceed '_audit_ic_obs'.       */
/* Otherwise, no auditing and write a message to sas   */
/*log of exceeding set maximum obs allowed.            */

/*1 parameter required*/
/*1 parameter optional*/

/*REFER to %lib_dsn*/

%macro audit_ic
(/*input dataset name */ _audit_ic_indsn
,/*maximum obs allowed*/ _audit_ic_obs=100000
);

%local _audit_ic_lib;
%local _audit_ic_dsn;
%lib_dsn(_audit_ic_indsn,_audit_ic_lib,_audit_ic_dsn);
%mend  audit_ic;
