%let RootFolder=Y:\Host\Communication of SAS Base; * change to your folder ;

/* example 6.2.1 Computing Specific Descriptive Statistics */
options nodate pageno = 1 linesize = 80 pagesize = 60;

data cake;
  input
    LastName $   1  - 12
    Age          13 - 14
    PresentScore 16 - 17 
    TasteScore   19 - 20
    Flavor   $   23 - 32
    Layers       34 - 34
  ;

  datalines;
Orlando     27 93 80  Vanilla    1
Ramey       32 84 72  Rum        2
Goldston    46 68 75  Vanilla    1
Roe         38 79 73  Vanilla    2
Larsen      23 77 84  Chocolate  .
Davis       51 86 91  Spice      3
Strickland  19 82 79  Chocolate  1
Nguyen      57 77 84  Vanilla    .
Hildenbrand 33 81 83  Chocolate  1
Byron       62 72 87  Vanilla    2
Sanders     26 56 79  Chocolate  1
Jaeger      43 66 74             1
Davis       28 69 75  Chocolate  2
Conrad      69 85 94  Vanilla    1
Walters     55 67 72  Chocolate  2
Rossburger  28 78 81  Spice      2
Matthew     42 81 92  Chocolate  2
Becker      36 62 83  Spice      2
Anderson    27 87 85  Chocolate  1
Merritt     62 73 84  Chocolate  1
;
run;

proc means
  data = cake n mean max min range std fw = 5;
  var PresentScore TasteScore;
  title 'Summary of Presentation and Taste Scores';
run;

proc summary
  data = cake print;
  var PresentScore TasteScore;
run;

/* example 6.2.2 Computing Specific Descriptive Statistics */
options nodate pageno = 1 linesize = 80 pagesize = 60;

data grade;
  input
    Name    $  1  -  8
    Gender  $  11 - 11
    Status  $  13 - 13
    Year    $  15 - 16 
    Section $  18 - 18
    Score      20 - 21
    FinalGrade 23 - 24
  ;

  datalines;
Abbott    F 2 97 A 90 87
Branford  M 1 98 A 92 97
Crandell  M 2 98 B 81 71
Dennison  M 1 97 A 85 72
Edgar     F 1 98 B 89 80
Faust     M 1 97 B 78 73
Greeley   F 2 97 A 82 91
Hart      F 1 98 B 84 80
Isley     M 2 97 A 88 86
Jasper    M 1 97 B 91 93
;
run;

proc means
  data = grade maxdec = 3 fw = 5;
  var Score;

  class Gender Status Year Section;
  types status * year;
  title 'Final Exam Grades for Student Status and Year of Graduation';
run;

/* example 6.2.3 Using the BY Statement with Class Variables */
options nodate pageno = 1 linesize = 80 pagesize = 60;

proc sort
  data = Grade out = GradeBySection;
  by section;
run;

proc means
  data = GradeBySection min max median;
  by Section;
  var Score;
  class Status Year;
  title1 'Final Exam Scores for Student Status and Year of Graduation';
  title2 ' Within Each Section';
run;

/* example 6.2.4 Using a CLASSDATA= Data Set with Class Variables */
options nodate pageno = 1 linesize = 80 pagesize = 60;

data caketype;
  input
    Flavor $ 1  - 10
    Layers   12 - 12;
  datalines;
Vanilla    1
Vanilla    2
Vanilla    3
Chocolate  1
Chocolate  2
Chocolate  3
;
run;

proc means
  data = cake range median min max fw = 5 maxdec = 0
  classdata = caketype exclusive printalltypes;
  var TasteScore; 
  class flavor layers;
  title 'Taste Score For Number of Layers and Cake Flavor';
run;

/* example 6.2.5 Use multiple label to caculate the subgroup */
data decision;
  do id = 1 to 1000;
    x = ranuni(7);
    y = put(x, key.);
    output;
  end;
run;

proc means
  data = decision n;
  class y / mlf /*order = data*/;
  format y $deccode.;
run;
