filename FHS "/home/*/data.csv";

/* Import data */
DATA A;
INFILE FHS DSD FIRSTOBS = 2;
INPUT AGE TOTALCHOL SBP DBP BMI CIGSPERDAY GLUCOSE HEARTRATE CVD HYPERTENSION;
RUN;

/* Save k3 and sample size in dataset B */
PROC UNIVARIATE DATA=A NOPRINT;     
VAR SBP;
OUTPUT OUT=B SKEW=k3obs N=N;       
RUN;

/* Adds Nruns = number of MC runs to dataset B */
DATA B; SET B; Nruns = 10000;        
RUN;

/* Adds title to dataset B */
PROC PRINT DATA=B; TITLE 'Data set B. Observed statistic';
RUN;

/* Create dataset MC using dataset B */
DATA MC; SET B; 
RETAIN SEED 83859;
DO MCrun=1 TO Nruns;
  DO j=1 TO N;          
    CALL RANNOR(SEED, X); 
    OUTPUT;
  END; 
END; 
RUN;
 
/* Compute k3 for each sample */ 
PROC UNIVARIATE DATA=MC NOPRINT;
VAR X;
BY MCrun;                        
OUTPUT OUT=C SKEW=k3;              
RUN;

/* Null distribution of k3 = skewness */
PROC UNIVARIATE DATA=C;
VAR k3;
HISTOGRAM; 
RUN;

/* Extending dataset B to the dimension as C: Repeating content of data B Nruns times */
DATA B; SET B;                      
DO i=1 TO Nruns; 
OUTPUT; 
END;   
RUN;

/* The indicator is 1 if k3 <= k3obs and 0 if k3 > k3obs */     
DATA D; MERGE B C;                  
indicator = (k3 <= k3obs);             
RUN;

/* The probability of k3 < k3obs  */
PROC MEANS DATA=D NOPRINT;          
VAR indicator;                      
OUTPUT OUT=E MEAN=Pvalue;           
RUN;

/* Report the p-value */
PROC PRINT DATA=E; TITLE 'Estimated p-value';
VAR Pvalue;                      
RUN;
/* p-value = 1 
   we do not reject H0
   => Skewness is not equal to 0
   => we can conclude that SBP is not normally distributed. */

/* Draw new sample from N(0,6/N) as the distribution of k3 */
DATA F;
SEED = 1660519;
DO j=1 TO 4434; 
	X = sqrt(6/4434) * RANNOR(SEED);
	OUTPUT;
END;
RUN;

/* The indicator is 1 if X <= 1.1479005625 and 0 if X > 1.1479005625 */     
DATA G; MERGE F;                  
indicator = (X <= 1.1479005625);             
RUN;

/* The probability of X <= 1.1479005625 */
PROC MEANS DATA=G NOPRINT;          
VAR indicator;                      
OUTPUT OUT=H MEAN=Pvalue;           
RUN;

/* Report the p-value */
PROC PRINT DATA=H; TITLE 'Estimated p-value';
VAR Pvalue;                      
RUN;
/* p-value = 1 
   we do not reject H0
   => Skewness is not equal to 0
   => we can conclude that SBP is not normally distributed. */

/* #2(a) */ 
%LET N = 20;                             
%LET MC = 1000;                 

/* Simulate samples from N(10,2) */
DATA X1 (KEEP=MCruns x1);
SEED = 83859;
DO MCruns = 1 to &MC;             
   DO i = 1 to &N;                      
      x1 = rand("Normal", 10, 2);                   
      OUTPUT;
   END;
END;

DROP SEED;
RUN;

DATA X2 (KEEP=MCruns x2);
SEED = 83860;
DO MCruns = 1 to &MC;             
   DO i = 1 to &N;                      
      x2 = rand("Normal", 10, 2);                   
      OUTPUT;
   END;
END;

DROP SEED;
RUN;

/* Create A with a new column of X1-X2 */
DATA A; 
	MERGE X1 X2;
	BY MCruns;
	diff = X1-X2;
RUN;

/* Obtain both CIs */
PROC TTEST DATA=A sides=2 alpha=0.05 h0=0;
VAR diff;
RUN;

/* Create B containing lower&upper bound of CIs */
PROC MEANS DATA=A NOPRINT;
BY MCruns;
VAR diff;
OUTPUT OUT=B MEAN=mean LCLM=min UCLM=max;
RUN;

/* Count how many CIs include parameter */
DATA B; SET B;
    indicator = (min < 0 & max > 0);          
RUN;
 
/* Estimate coverage probability */
PROC MEANS DATA=B NOPRINT;          
VAR indicator;                      
OUTPUT OUT=C MEAN=covprob;           
RUN;

PROC PRINT DATA=C; TITLE 'Estimated coverage probability';
VAR covprob;                      
RUN;

/* #2(b) */ 
%LET N = 20;                             
%LET MC = 1000;                 

/* Simulate samples from N(10,2) */
DATA X1 (KEEP=MCruns x1);
SEED = 83859;
DO MCruns = 1 to &MC;             
   DO i = 1 to &N;                      
      x1 = rand("Normal", 10, 2);                   
      OUTPUT;
   END;
END;

DROP SEED;
RUN;

/* Simulate samples from N(10,4) */
DATA X2 (KEEP=MCruns x2);
SEED = 83860;
DO MCruns = 1 to &MC;             
   DO i = 1 to &N;                      
      x2 = rand("Normal", 10, 4);                   
      OUTPUT;
   END;
END;

DROP SEED;
RUN;

/* Create A with a new column of X1-X2 */
DATA A; MERGE X1 X2;
BY MCruns;
diff=X1-X2;
RUN;

/* Obtain both CIs */
PROC TTEST DATA=A sides=2 alpha=0.05 h0=0;
VAR diff;
RUN;

PROC MEANS DATA=A noprint;
   BY MCruns;
   VAR diff;
   OUTPUT OUT=B MEAN=mean LCLM=min UCLM=max;
RUN;

/* Count how many CIs include parameter */
DATA B; SET B;
   indicator = (min < 0 & max > 0);          
RUN;
 
/* Estimate coverage probability */
PROC MEANS DATA=B NOPRINT;          
VAR indicator;                      
OUTPUT OUT=C MEAN=covprob;           
RUN;

PROC PRINT DATA=C; TITLE 'Estimated coverage probability';
VAR covprob;                      
RUN;

/* #2(c) */ 
%LET N1 = 20; 
%LET N2 = 10;                             
%LET MC = 1000;                 

/* Simulate samples from N(10,2) */
DATA X1 (KEEP=MCruns x1);
SEED = 83859;
DO MCruns = 1 to &MC;             
   DO i = 1 to &N1;                      
      x1 = rand("Normal", 10, 2);                   
      OUTPUT;
   END;
END;

DROP SEED;
RUN;

/* Simulate samples from N(10,2) */
DATA X2 (KEEP=MCruns x2);
SEED = 83860;
DO MCruns = 1 to &MC;             
   DO i = 1 to &N2;                      
      x2 = rand("Normal", 10, 2);                   
      OUTPUT;
   END;
END;

DROP SEED;
RUN;

DATA A; MERGE X1 X2;
BY MCruns;
diff=X1-X2;
RUN;

PROC MEANS DATA=A noprint;
   BY MCruns;
   VAR diff;
   OUTPUT OUT=B MEAN=mean LCLM=min UCLM=max;
RUN;

/* Count how many CIs include parameter */
DATA B; SET B;
   indicator = (min < 0 & max > 0);          
RUN;
 
/* Estimate coverage probability */
PROC MEANS DATA=B NOPRINT;          
VAR indicator;                      
OUTPUT OUT=C MEAN=covprob;           
RUN;

PROC PRINT DATA=C; TITLE 'Estimated coverage probability';
VAR covprob;                      
RUN;

/* #2(d) */ 
%LET N1 = 20; 
%LET N2 = 10;                             
%LET MC = 1000;                 

/* Simulate samples from N(10,2) */
DATA X1 (KEEP=MCruns x1);
SEED = 83859;
DO MCruns = 1 to &MC;             
   DO i = 1 to &N1;                      
      x1 = rand("Normal", 10, 2);                   
      OUTPUT;
   END;
END;

DROP SEED;
RUN;

/* Simulate samples from N(10,4) */
DATA X2 (KEEP=MCruns x2);
SEED = 83860;
DO MCruns = 1 to &MC;             
   DO i = 1 to &N2;                      
      x2 = rand("Normal", 10, 4);                   
      OUTPUT;
   END;
END;

DROP SEED;
RUN;

DATA A; MERGE X1 X2;
BY MCruns;
diff=X1-X2;
RUN;

PROC MEANS DATA=A noprint;
   BY MCruns;
   VAR diff;
   OUTPUT OUT=B MEAN=mean LCLM=min UCLM=max;
RUN;

/* Count how many CIs include parameter */
DATA B; SET B;
   indicator = (min < 0 & max > 0);          
RUN;
 
/* Estimate coverage probability */
PROC MEANS DATA=B NOPRINT;          
VAR indicator;                      
OUTPUT OUT=C MEAN=covprob;           
RUN;

PROC PRINT DATA=C; TITLE 'Estimated coverage probability';
VAR covprob;                      
RUN;

/* #2(e) */
%LET N1 = 10; 
%LET N2 = 20;                             
%LET MC = 1000;                 

/* Simulate samples from N(10,2) */
DATA X1 (KEEP=MCruns x1);
SEED = 83859;
DO MCruns = 1 to &MC;             
   DO i = 1 to &N1;                      
      x1 = rand("Normal", 10, 2);                   
      OUTPUT;
   END;
END;

DROP SEED;
RUN;

/* Simulate samples from N(10,4) */
DATA X2 (KEEP=MCruns x2);
SEED = 83860;
DO MCruns = 1 to &MC;             
   DO i = 1 to &N2;                      
      x2 = rand("Normal", 10, 4);                   
      OUTPUT;
   END;
END;

DROP SEED;
RUN;

DATA A; MERGE X1 X2;
BY MCruns;
diff=X1-X2;
RUN;

PROC MEANS DATA=A noprint;
   BY MCruns;
   VAR diff;
   OUTPUT OUT=B MEAN=mean LCLM=min UCLM=max;
RUN;

/* Count how many CIs include parameter */
DATA B; SET B;
   indicator = (min < 0 & max > 0);          
RUN;
 
/* Estimate coverage probability */
PROC MEANS DATA=B NOPRINT;          
VAR indicator;                      
OUTPUT OUT=C MEAN=covprob;           
RUN;

PROC PRINT DATA=C; TITLE 'Estimated coverage probability';
VAR covprob;                      
RUN;
