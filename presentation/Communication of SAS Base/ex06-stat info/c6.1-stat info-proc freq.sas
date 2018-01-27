%let RootFolder=Y:\Host\Communication of SAS Base; * change to your folder ;

/* example 6.1.1 Output Data Set of Frequencies */
data Color;
  input Region Eyes $ Hair $ Count @@;
  label
    Eyes   = 'Eye Color'
    Hair   = 'Hair Color'
    Region = 'Geographic Region'
  ;

  datalines;
1 blue  fair   23  1 blue  red     7  1 blue  medium 24
1 blue  dark   11  1 green fair   19  1 green red     7
1 green medium 18  1 green dark   14  1 brown fair   34
1 brown red     5  1 brown medium 41  1 brown dark   40 
1 brown black   3  2 blue  fair   46  2 blue  red    21
2 blue  medium 44  2 blue  dark   40  2 blue  black   6
2 green fair   50  2 green red    31  2 green medium 37
2 green dark   23  2 brown fair   56  2 brown red    42
2 brown medium 53  2 brown dark   54  2 brown black  13
;
run;

proc freq
  data = Color;
  tables Eyes Hair Eyes * Hair;
  weight Count;
  title 'Eye and Hair Color of European Children';
run;

proc freq
  data = Color;
  tables Eyes Hair Eyes * Hair / out = FreqCount outexpect sparse chisq;
  weight Count;
  title 'Eye and Hair Color of European Children';
run;

proc print
  data = FreqCount noobs;
  title2 'Output Data Set from PROC FREQ';
  footnote;
run;

/* example 6.1.2 Frequency Dot Plots */
ods graphics on;
proc freq
  data = Color order = freq;
  tables Hair Eyes * Hair / plots = freqplot(type = dot);
  tables Region * Hair / plots = freqplot(type = dot scale = percent);
  weight Count;
  title 'Eye and Hair Color of European Children';
run;
ods graphics off;
