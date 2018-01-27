%let RootFolder=Y:\Host\Communication of SAS Base; * change to your folder ;
libname ex07 "&RootFolder.\ex07-transpose";

/* example 7.1.1 proc transpose RXs from Rows to Columns */
proc transpose
  data = ex07.xpo_summarize out = tran_xpo_summarize_trx(drop = _NAME_) prefix = TRx_ /*suffix = a*/;
  by IMS_ID;
  id DATE;
  var TRx;
run;

proc transpose
  data = ex07.xpo_summarize out = tran_xpo_summarize_trx_nbrx;
  by IMS_ID;
  id DATE;
  var TRx NBRx;
run;

proc transpose
  data = ex07.xpo_summarize out = tran_xpo_summarize_nbrx(drop = _NAME_) prefix = NBRx suffix = a;
  by IMS_ID;
  id DATE;
  var NBRx;
run;

data tran_xpo_summarize_trx_nbrx;
  merge tran_xpo_summarize_trx tran_xpo_summarize_nbrx;
  by IMS_ID;
  TOTAL_TRx = sum(of TRx:);
  TOTAL_NBRx = sum(of NBRx:);
run;

/* example 7.1.2 proc transpose RXs from Columns to Rows */
proc transpose
  data = Tran_xpo_summarize_trx_nbrx out = tran_xpo_summarize_trx_v2(rename = (COL1 = TRx)) name = DATE;
  by IMS_ID;
  var TRx:;
run;

/* example 7.1.3 data step RXs from Columns to Rows */
data tran_xpo_summarize_trx_v3(keep = IMS_ID DATE TRx);
  set Tran_xpo_summarize_trx_nbrx;
  format DATE $8.;
  array ArrTRx{9} TRx_20150424 -- TRx_20150320;

  do i = 1 to 9;
    DATE = strip(substr(vname(ArrTRx{i}), 5, 8));
    TRx = ArrTRx{i};
    output;
  end;
run;
