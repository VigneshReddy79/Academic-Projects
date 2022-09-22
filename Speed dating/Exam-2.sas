/* Exam - 2 Take Home Exam*/

libname datasets "I:\Documents\Spring'22\STA512-PrinExpAnalysis\DATASETS";

data dating;
set datasets.speed_dating;
run;

proc contents data=dating;
run;

proc means data=dating n nmiss;
run;

data dating_2 (keep= Like Ambitious Attractive Fun Intelligent SharedInterests Sincere PartnerYes 
                    ageM ageF raceM raceF decisionM decisionF);
set dating;
Like = mean(LikeM,LikeF);
Ambitious = mean(AmbitiousM,AmbitiousF);
Attractive = mean(AttractiveM,AttractiveF);
Fun = mean(FunM,FunF);
Intelligent = mean(IntelligentM,IntelligentF);
SharedInterests = mean(SharedInterestsM,SharedInterestsF);
Sincere = mean(SincereM,SincereF);
PartnerYes = mean(PartnerYesM,PartnerYesF);
run;

proc means data=dating_2 n nmiss;
run;

proc glm data=dating_2;
model like = ambitious | attractive | intelligent | fun | sharedinterests | sincere @1;
run;

proc glm data=dating_2;
model like = ambitious | attractive | intelligent | fun | sharedinterests | sincere @2;
run;

/*
for individual variables
SSR = 332.7976835
MSE = 0.6205454
df = 6

for along with interaction variables
SSR = 344.3856319
MSE = 0.6112780
df = 21

[[(344.3856319 - 332.7976835)/6] / 0.6112780 = 3.159486
p = 0.0002381874  0.0051964065 0.0053101391
Atleast one pairwise is significant
*/

data helpme;
ans = 1-probf(3.159486,6,241);
run;


data dating_allvar;
length same_race 3 age_close 3 age_grp_M $5 age_grp_F $5;
set dating_2;

amb_att = Ambitious*Attractive;
amb_intg = Ambitious*Intelligent;
amb_fun = Ambitious*Fun;
amb_sharedintr = Ambitious*SharedInterests;
amb_sin = Ambitious*Sincere;

att_intg = Attractive*Intelligent;
att_fun = Attractive*Fun;
att_sharedintr = Attractive*SharedInterests;
att_sin = Attractive*Sincere;

intg_fun = Intelligent*Fun;
intg_sharedintr = Intelligent*SharedInterests;
intg_sin = Intelligent*Sincere;

fun_sharedintr = Fun*SharedInterests;
fun_sin = Fun*Sincere;

sharedintr_sin = SharedInterests*Sincere;

amb_2 = Ambitious**2;
amb_3 = Ambitious**3;

att_2 = Attractive**2;
att_3 = Attractive**3;

intg_2 = Intelligent**2;
intg_3 = Intelligent**3;

fun_2 = Fun**2;
fun_3 = Fun**3;

sharedintr_2 = SharedInterests**2;
sharedintr_3 = SharedInterests**3;

sin_2 = Sincere**2;
sin_3 = Sincere**3;

if racem = "" | racef = "" then same_race = .;
else if racem = racef then same_race = 1;
else same_race = 0;

if agem = . | agef = . then age_close = .;
else if abs(agem-agef) <= 2 then age_close = 1;
else age_close = 0;

if agem =. then age_grp_M = "";
else if agem <= 24 then age_grp_M = "<=24";
else if agem > 24 & agem <=30 then age_grp_M = "25-30";
else if agem > 30 then age_grp_M = ">30";

if agef =. then age_grp_F = "";
else if agef <= 24 then age_grp_F = "<=24";
else if agef > 24 & agef <=30 then age_grp_F = "25-30";
else if agef > 30 then age_grp_F = ">30";

run;

/* Descriptive statistics */

ods rtf file="I:\Documents\Spring'22\STA512-PrinExpAnalysis\Week-11, Exam-2\descrip_stats.rtf";

ods listing close;

proc means data=dating_allvar n nmiss mean median min max;
var like ambitious attractive intelligent fun sharedinterests sincere;
run;

proc freq data=dating_allvar;
tables same_race racem*racef age_close age_grp_M*age_grp_F decisionm*decisionf;
run;

proc univariate data=dating_allvar;
var like;
run;

proc sgplot data=dating_allvar;
  *title "Cholesterol Distribution by Weight Class";
  hbox like / category=same_race;
run;

proc sgpanel data=dating_allvar;
  panelby same_race age_close / layout=lattice;
  hbox like;
run;

proc corr data=dating_allvar;
var like ambitious attractive intelligent fun sharedinterests sincere;
run;

ods rtf close;





/* Model building */


ods rtf file="I:\Documents\Spring'22\STA512-PrinExpAnalysis\Week-11, Exam-2\model_building.rtf";

ods listing close;
proc reg data=dating_allvar;
main: model like = Ambitious Attractive Intelligent Fun SharedInterests Sincere
                   amb_att amb_intg amb_fun amb_sharedintr amb_sin 
                   att_intg att_fun att_sharedintr att_sin 
			       intg_fun intg_sharedintr intg_sin
			       fun_sharedintr fun_sin
			       sharedintr_sin
                   amb_2 att_2 intg_2 fun_2 sharedintr_2 sin_2 amb_3 att_3 intg_3 fun_3 sharedintr_3 sin_3;
test_poly: test amb_2,att_2,intg_2,fun_2,sharedintr_2,sin_2,amb_3,att_3,intg_3,fun_3,sharedintr_3,sin_3;
test_int:  test amb_att,amb_intg,amb_fun,amb_sharedintr,amb_sin,att_intg,
                att_fun,att_sharedintr,att_sin,
			    intg_fun,intg_sharedintr,intg_sin,
			    fun_sharedintr,fun_sin,
			    sharedintr_sin;
run;quit;


proc reg data=dating_allvar;
model like = Ambitious Attractive Intelligent Fun SharedInterests Sincere 
             /*amb_att amb_intg amb_fun amb_sharedintr amb_sin att_intg 
             att_fun att_sharedintr att_sin 
			 intg_fun intg_sharedintr intg_sin
			 fun_sharedintr fun_sin
			 sharedintr_sin*/
             amb_2 att_2 intg_2 fun_2 sharedintr_2 sin_2 amb_3 att_3 intg_3 fun_3 sharedintr_3 sin_3 /
			 selection= rsquare cp mse include=6;
run;

/*

include, w/o int, 8 var
Ambitious Attractive Intelligent Fun SharedInterests Sincere  intg_2 intg_3 
R^2=0.6858 Cp=7.6313 MSE=0.60018 

*/

proc reg data=dating_allvar;
model like = Ambitious Attractive Intelligent Fun SharedInterests Sincere 
             /*amb_att amb_intg amb_fun amb_sharedintr amb_sin att_intg 
             att_fun att_sharedintr att_sin 
			 intg_fun intg_sharedintr intg_sin
			 fun_sharedintr fun_sin
			 sharedintr_sin*/
             amb_2 att_2 intg_2 fun_2 sharedintr_2 sin_2 amb_3 att_3 intg_3 fun_3 sharedintr_3 sin_3 /
			 selection= forward slentry=0.15 include=6;
run;

/*

include, w/o int 9 var slentry=0.15
Ambitious Attractive Intelligent Fun SharedInterests Sincere
intg_2 intg_3 amb_2
R-Square = 0.6903 and C(p) = 5.9637 MSE=0.59393

*/

proc reg data=dating_allvar;
model like = Ambitious Attractive Intelligent Fun SharedInterests Sincere 
             /*amb_att amb_intg amb_fun amb_sharedintr amb_sin att_intg 
             att_fun att_sharedintr att_sin 
			 intg_fun intg_sharedintr intg_sin
			 fun_sharedintr fun_sin
			 sharedintr_sin*/
             amb_2 att_2 intg_2 fun_2 sharedintr_2 sin_2 amb_3 att_3 intg_3 fun_3 sharedintr_3 sin_3 /
			 selection= backward slstay=0.15 include=6;
run;

/*

include, w/o int, slstay=0.15 9 var
Ambitious Attractive Intelligent Fun SharedInterests Sincere 
amb_2 intg_2 intg_3
R-Square = 0.6903 and C(p) = 5.9637 MSE=0.59393
 
*/

proc reg data=dating_allvar;
model like = Ambitious Attractive Intelligent Fun SharedInterests Sincere 
             /*amb_att amb_intg amb_fun amb_sharedintr amb_sin att_intg 
             att_fun att_sharedintr att_sin 
			 intg_fun intg_sharedintr intg_sin
			 fun_sharedintr fun_sin
			 sharedintr_sin*/
             amb_2 att_2 intg_2 fun_2 sharedintr_2 sin_2 amb_3 att_3 intg_3 fun_3 sharedintr_3 sin_3 /
			 selection= stepwise slentry=0.15 slstay=0.15 include=6;
run;

/*

include, w/o int slentry=0.15 slstay=0.15 9 var
Ambitious Attractive Intelligent Fun SharedInterests Sincere
intg_2 intg_3 amb_2 
R-Square = 0.6903 and C(p) = 5.9637 MSE=0.59393


removed:
1) intg_2_c
2)Ambitious_c
3)amb_2_c

Final model Y = 0.51861  + 0.37108*Attractive + 0.27720*Intelligent_c + 0.19714*Fun + 0.13192*SharedInterests  
                + 0.19622*Sincere - 0.02553*intg_3_c

R^2=0.6859  MSE=0.59342
read data = 269 
*/

ods rtf close;




/* Diagnostics */





ods rtf file="I:\Documents\Spring'22\STA512-PrinExpAnalysis\Week-10\diagnostics.rtf";

data dating_allvar;
set dating_allvar;
if like^=2.5;
run;

data center;
set dating_allvar;
/*Ambitious_c = Ambitious - 7.1368613;*/
/*Attractive_c = Attractive - 6.4736364;*/
Intelligent_c = Intelligent-7.7663636;
/*Fun_c = Fun-6.8629630;*/
/*sharedinterests_c = sharedinterests-5.5371747;*/
/*Sincere_c = Sincere-7.8145455;*/
/*amb_intg_c = Ambitious_c*Intelligent_c;*/
/*att_fun_c = Attractive_c*Fun_c;*/
/*amb_2_c = Ambitious_c**2  ;*/
/*att_3_c = Attractive_c**3  ;*/
intg_2_c = Intelligent_c**2;
intg_3_c = Intelligent_c**3;
run;

proc reg data=dating_allvar;
model like = Attractive Intelligent Fun SharedInterests Sincere intg_3 
             / vif collinoint;
run;

proc reg data=center ;
model like =  Attractive Intelligent_c Fun SharedInterests Sincere intg_3_c
             / vif collinoint spec;
output out=resd_data p=preds rstudent=jackknife cookd=cooks h=leverage;
run;quit;

proc univariate data=resd_data normal plots;
var jackknife;
run;

proc gplot data=resd_data;
plot jackknife*preds / vref=0 vref=-2 vref=2;
run;

data jcl_data (keep= like preds jackknife violated_j cooks violated_c leverage violated_h);
length violated_j $1 violated_c $1 violated_h $1;
set resd_data;
k = 6;
n = 276;
h_0 = 2*(k+1)/n;
if abs(jackknife) > 2 then violated_j = 1;
else violated_j = 0;
if cooks > 1 then violated_c = 1;
else violated_c = 0;
if leverage > h_0 then violated_h = 1;
else violated_h = 0;
if abs(jackknife) > 2 | cooks > 1 | leverage > h_0 then output;
run;

proc print data=jcl_data;
run; 

proc reg data=center;
main: model like =  Attractive Intelligent_c Fun SharedInterests Sincere  intg_3_c same_race
             / vif collinoint;
test_race: test same_race;
run;quit;

proc reg data=center;
main: model like =  Attractive Intelligent_c Fun SharedInterests Sincere  intg_3_c age_close
             / vif collinoint;
test_age: test age_close;
run;quit;

/*
same_race:
t statistic= 0.65 f statistic = 0.43  p-value= 0.5146   Not significant

age_close:
t statistic= 1.09 f statistic = 1.18 p-value=0.2787   Not significant
*/


/* Reliability */



data split;
set center;
ran_nbr = ranuni(7997);
run;

proc sort data=split;
by ran_nbr;
run;

data holdout training;
set split;
counter=_n_;
if counter le 92 then output holdout;
else output training;
run;

data holdout;
set holdout;
Yhat = 0.75163  + 0.37456*Attractive + 0.31237*Intelligent_c + 0.16530*Fun + 0.13722*SharedInterests  
                + 0.18839*Sincere - 0.02639*intg_3_c;
/* R^2=0.6938 MSE=0.56174 */
run;

proc corr data=holdout;
var like Yhat;
run;

/* 
Yhat = 0.51861  + 0.37108*Attractive + 0.27720*Intelligent_c + 0.19714*Fun + 0.13192*SharedInterests  
                + 0.19622*Sincere - 0.02553*intg_3_c
r = 0.86384
R^2 = 0.7462

0.7462 - 0.6859 = 0.0603
*/
/*
Yhat = 0.75163  + 0.37456*Attractive + 0.31237*Intelligent_c + 0.16530*Fun + 0.13722*SharedInterests  
                + 0.18839*Sincere - 0.02639*intg_3_c
r = 0.86152
R^2 = 0.7422

0.7422 - 0.6938 = 0.0484
*/
 ods rtf close;

 

 proc glm data=holdout;
 model like = yhat same_race yhat*same_race;
 run;

 proc glm data=holdout;
 model like = yhat age_close yhat*age_close;
 run;
