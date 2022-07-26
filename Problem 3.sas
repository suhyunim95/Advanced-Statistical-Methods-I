/* #1 */
FILENAME data "/home/*/data.csv";

DATA C;
INFILE data DSD FIRSTOBS=2;
INPUT URIC DIA HDL CHOLES TRIG ALCO;
RUN;

/* Fit a full model for predicting uric acid levels using all other explanatory variables. */
PROC REG DATA=C; 
MODEL URIC=DIA HDL CHOLES TRIG ALCO;
RUN;

/* Find the best model(s) using adjusted R square criterion. */
PROC REG DATA=C; 
MODEL URIC=DIA HDL CHOLES TRIG ALCO / SELECTION=ADJRSQ RSQUARE SSE MSE CP; /* selection is based on Adj R^2 */
RUN;

PROC REG DATA=C; 
MODEL URIC=DIA CHOLES TRIG ALCO;
RUN;

/* Check all assumptions upon the best model chosen */
/* 1. Linearity test: Lack of fit test */
PROC REG DATA=C;
MODEL URIC=DIA CHOLES TRIG ALCO / LACKFIT;
OUTPUT OUT=D RSTUDENT=R PREDICTED=P; 
RUN; 
/* H0: There is no lack of fit. The model is linear.
   p-value = 0.8994 > 0.05  
   We accept H0 and conclude that there is no lack of fit and the model is linear. */

/* 2. Homogeneity of variance: Breusch-Pagan test, residual plot */
PROC MODEL DATA=C;
PARMS b0 b1 b2 b3 b4;
URIC=b0 + b1*DIA + b2*CHOLES + b3*TRIG + b4*ALCO;
FIT URIC /WHITE BREUSCH=(DIA CHOLES TRIG ALCO);
RUN;
/* H0: Homogeneity of variance holds.
   Breusch-Pagan test: p-value <.0001, so we reject H0 and conclude that the homogeneity of variance doesn't hold. */

/* Absolute residual vs. fitted values plot */
DATA D; SET D;
ABSR = ABS(R); 
RUN;

PROC PLOT DATA=D HPERCENT=50 VPERCENT=50;
PLOT ABSR*P; 
RUN;
/* From the absolute residuals vs fitted values plot, we can see the residuals increase along with the fitted values.  */

/* 3. Normality of errors */
PROC UNIVARIATE DATA=D NORMAL PLOT; /* Check normality of Studentized residuals */
VAR R;
RUN;
/* H0: Normality holds.
   Since p value < 0.0001, we reject H0 and conclude that the normality doesn't hold. */

/* Detect outliers using Bonferroni method. */
PROC REG DATA=D;
MODEL URIC=DIA CHOLES TRIG ALCO / INFLUENCE R;
ODS OUTPUT OUTPUTSTATISTICS=RESULTS;
RUN;

DATA RESULTS; SET RESULTS; 
TVALUE=tinv(1-0.01/(2*998), 998-6-1);
IF (ABS(RSTUDENT)) > TVALUE THEN OUTLIER=1;
ELSE OUTLIER=0;
RUN;

PROC PRINT DATA=RESULTS;
WHERE OUTLIER=1;
VAR RSTUDENT;
RUN;
/* Cases 267, 477, 483 are outliers by using Bonferroni method. */

/* Detect outliers, influential points */
DATA RESULTS; SET RESULTS;
IF HATDIAGONAL > 2*(6/998) THEN HILEV=1;
ELSE HILEV=0;
IF (ABS(DFFITS) > 2*SQRT(6/998)) THEN DFFLAG=1; 
ELSE DFFLAG=0;
FPERCENT = 100*PROBF(COOKSD, 6, 998-6); 
IF (ABS(DFB_DIA) > 2/SQRT(998)) THEN B1FLAG=1; 
ELSE B1FLAG=0;
IF (ABS(DFB_CHOLES) > 2/SQRT(998)) THEN B2FLAG=1;
ELSE B2FLAG=0;
IF (ABS(DFB_TRIG) > 2/SQRT(998)) THEN B3FLAG=1;
ELSE B3FLAG=0;
IF (ABS(DFB_ALCO) > 2/SQRT(998)) THEN B4FLAG=1;
ELSE B4FLAG=0;
RUN;

PROC PRINT DATA=RESULTS;
WHERE HILEV=1 or DFFLAG=1 or FPERCENT>20 or B1FLAG=1 or B2FLAG=1 or B3FLAG=1;
VAR HATDIAGONAL HILEV DFFITS DFFLAG COOKSD FPERCENT DFB_DIA B1FLAG DFB_CHOLES B2FLAG DFB_TRIG B3FLAG DFB_ALCO B4FLAG;
RUN;
/* Cases 267, 477, 483 are outliers and influential. */

/* Check for collinearity */
PROC REG DATA=D; 
MODEL URIC=DIA CHOLES TRIG ALCO / COLLIN TOL VIF; 
RUN; 
/* None of VIF exceeds 10, mean VIF is around 1, none of condition indices exceeds 30 => There is no collinearity. */

/* #3 */
/* Fit a WLS model */
PROC REG DATA=D;
MODEL URIC=DIA CHOLES TRIG ALCO /R CLB;
OUTPUT OUT=RESULTS R=RESIDUAL;
RUN;

DATA STEP2;
SET RESULTS;
ABSRESID = ABS(RESIDUAL);
RUN;

PROC REG DATA=STEP2;
MODEL ABSRESID=DIA CHOLES TRIG ALCO /P;
OUTPUT OUT=STEP3 P=YHAT;
RUN;

DATA STEP3;
SET STEP3;
WT = 1/(YHAT**2);
RUN;

PROC REG DATA=STEP3; 
MODEL URIC=DIA CHOLES TRIG ALCO /R CLB;
WEIGHT WT;
OUTPUT OUT=ITERATION2 R=RESIDUAL2;
RUN;

/* Reiterate the process for WLS */
DATA ITERATION2;
SET ITERATION2;
ABSRESID2 = ABS(RESIDUAL2);
RUN;

PROC REG DATA=ITERATION2;
MODEL ABSRESID2=DIA CHOLES TRIG ALCO /P; 
OUTPUT OUT=RESULTS2 P=YHAT2;
RUN;

DATA RESULTS2;
SET RESULTS2;
WT2=1/(YHAT2**2);
RUN;

/* Weighted least squares regression */
PROC REG DATA=RESULTS2; 
MODEL URIC=DIA CHOLES TRIG ALCO /R CLB;
WEIGHT WT2;
OUTPUT OUT=ITERATION3 R=RESIDUAL3;
RUN;

/* #4 */
/* Iteration 1 */
PROC REG DATA=D;
MODEL URIC=DIA CHOLES TRIG ALCO /R;
OUTPUT OUT=RESULTS R=RESIDUAL;
RUN;

PROC UNIVARIATE DATA=RESULTS NOPRINT;
VAR RESIDUAL;
OUTPUT OUT=MED1 MEDIAN=SCALE;

PROC PRINT DATA=MED1;
VAR SCALE;
RUN;
/* MEDIAN1 = -7.67431 */

DATA STEP2;
SET RESULTS;
MAD1 = RESIDUAL/(-7.67431/0.6745);
RUN;

PROC REG DATA=STEP2;
MODEL MAD1=DIA CHOLES TRIG ALCO /P;
OUTPUT OUT=STEP3 P=YHAT;
RUN;

DATA STEP3;
SET STEP3;
WT = (1-(MAD1/4.685)**2)**2;
RUN;

PROC REG DATA=STEP3; 
MODEL URIC=DIA CHOLES TRIG ALCO /R CLB;
WEIGHT WT;
OUTPUT OUT=ITERATION2 R=RESIDUAL2;
RUN;

/* Iteration 2 */
PROC UNIVARIATE DATA=ITERATION2 NOPRINT;
VAR RESIDUAL2;
OUTPUT OUT=MED2 MEDIAN=SCALE;

PROC PRINT DATA=MED2;
VAR SCALE;
RUN;
/* MEDIAN1 = -417.165 */

DATA STEP3;
SET RESULTS;
MAD2 = RESIDUAL/(-417.165/0.6745);
RUN;

PROC REG DATA=STEP3;
MODEL MAD2=DIA CHOLES TRIG ALCO /P;
OUTPUT OUT=STEP4 P=YHAT;
RUN;

DATA STEP4;
SET STEP4;
WT2 = (1-(MAD2/4.685)**2)**2;
RUN;

PROC REG DATA=STEP4; 
MODEL URIC=DIA CHOLES TRIG ALCO /R CLB;
WEIGHT WT2;
OUTPUT OUT=ITERATION3 R=RESIDUAL3;
RUN;

/* Iteration 3 */
PROC UNIVARIATE DATA=ITERATION3 NOPRINT;
VAR RESIDUAL3;
OUTPUT OUT=MED3 MEDIAN=SCALE;

PROC PRINT DATA=MED3;
VAR SCALE;
RUN;
/* MEDIAN1 = -7.57341 */

DATA STEP5;
SET RESULTS;
MAD3 = RESIDUAL/(-7.57341/0.6745);
RUN;

PROC REG DATA=STEP5;
MODEL MAD3=DIA CHOLES TRIG ALCO /P;
OUTPUT OUT=STEP6 P=YHAT;
RUN;

DATA STEP6;
SET STEP6;
WT3 = (1-(MAD3/4.685)**2)**2;
RUN;

PROC REG DATA=STEP6; 
MODEL URIC=DIA CHOLES TRIG ALCO /R CLB;
WEIGHT WT3;
OUTPUT OUT=ITERATION4 R=RESIDUAL4;
RUN;

/* #5 */
PROC MEANS DATA=C MEDIAN; 
VAR URIC;
RUN;

PROC SURVEYSELECT DATA=C NOPRINT SEED=123 
			      OUT=BOOT(RENAME=(REPLICATE=SAMPLEID))
                  METHOD=URS
                  SAMPRATE=1 
                  REPS=1000;
RUN;

PROC MEANS DATA=BOOT NOPRINT;
BY SAMPLEID;
FREQ NUMBERHITS;
VAR URIC;
OUTPUT OUT=BOOT MEDIAN=MEDIANHAT;
RUN;

/* histogram and Q-Q plot of the bootstrap distribution of theta hat */
TITLE "Bootstrap Distribution";
PROC UNIVARIATE DATA=BOOT;
VAR MEDIANHAT;
HISTOGRAM;
QQPLOT;
RUN;

/* Standard error theta hat */
PROC MEANS DATA=BOOT NOLABELS N STDERR;
VAR MEDIANHAT;
RUN;

PROC PRINT DATA=BOOT (OBS=10);
RUN;

/* Bias of theta hat */
DATA BOOT;
SET BOOT;
BIAS=MEDIAN-MEDIANHAT;
RUN;

PROC MEANS DATA=BOOT;
VAR BIAS;
RUN;

/* 2.5th and 97.5th percentiles of the sampling distribution of theta hat */
PROC UNIVARIATE DATA=BOOT NOPRINT;
VAR MEDIANHAT;
OUTPUT OUT=PCTL PCTLPRE=CI95_ PCTLPTS=2.5  97.5 PCTLNAME=Lower Upper;
RUN;

PROC PRINT DATA=PCTL NOOBS;
RUN;

/* 2.5th and 97.5th percentiles of the sampling distribution of (theta hat-theta) */
PROC UNIVARIATE DATA=BOOT NOPRINT;
VAR BIAS;
OUTPUT OUT=BPCTL PCTLPRE=CI95_ PCTLPTS=2.5  97.5 PCTLNAME=Lower Upper;
RUN;

PROC PRINT DATA=BPCTL NOOBS;
RUN;

/* 95% confidence interval for θ using three bootstrap methods */
/* normal approximation */
PROC MEANS DATA=BOOT NOPRINT ;
VAR MEDIANHAT ;
OUTPUT OUT=CI N=n MEAN=mean
STDERR=stderr LCLM=lclm Uclm=uclm ;
RUN;

PROC PRINT DATA=CI;
RUN;

/* basic bootstrap */
PROC UNIVARIATE DATA=BOOT NOPRINT;
VAR MEDIANHAT;
OUTPUT OUT=BPCTL PCTLPRE=CI95_ PCTLPTS=2.5  97.5 PCTLNAME=Lower Upper;
RUN;

PROC PRINT DATA=PCTL NOOBS;
RUN;

/* percentile bootstrap */
PROC UNIVARIATE DATA=BOOT NOPRINT;
VAR MEDIANHAT;
OUTPUT OUT=PCTL PCTLPRE=CI95_ PCTLPTS=2.5  97.5 PCTLNAME=Lower Upper;
RUN;

PROC PRINT DATA=PCTL NOOBS;
RUN;

/* #6 */
PROC IMPORT DATAFILE='/home/u59240733/breast_tumor.xlsx'
OUT=TUMOR
DBMS=XLSX
REPLACE;
RUN;

PROC Logistic Data = TUMOR descending; /* descending to model P(Y=1) instead of P(Y=0) */
MODEL CLASS =  Clump_Thickness SIZE_Uniformity SHAPE_Uniformity marginal_adhesion epithelial_size bare_Nucleoli Bland_Chromatin Normal_Nucleoli mitoses;  
RUN;

PROC Logistic Data = TUMOR descending; /* descending to model P(Y=1) instead of P(Y=0) */
MODEL CLASS =  Clump_Thickness marginal_adhesion bare_Nucleoli Bland_Chromatin Normal_Nucleoli;  
RUN;
