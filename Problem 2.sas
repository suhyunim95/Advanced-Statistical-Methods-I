FILENAME data "/home/*/data.csv";

DATA C;
INFILE data DSD FIRSTOBS = 2;
INPUT Y X1 X2 X3 X4;
RUN;
/* X1: Doctor Availability 
   X2: Hospital Availability 
   X3: Capital Income 
   x4: Popular Density */

/* Fit a multiple regression model using all available predictors */
PROC REG DATA = C; 
MODEL Y=X1 X2 X3 X4;
RUN;

/* Lack of fit test and detect the outlier */
PROC REG DATA=C;
MODEL Y=X1 X2 X3 X4 / lackfit;
OUTPUT OUT=D RSTUDENT=R PREDICTED=P; 
RUN; 
/* There is no p-value. 
   H0: There is no lack of fit. The model is linear. 
   We conclude that there is a lack of fit. */
/* slightly left-skewed */
/* There is outliers in the Rstudent vs. fitted value plot which |Rstudent| > 2 */

DATA D; SET D;
absR = ABS(R); /* save absolute value of residuals */
RUN;

/* Absolute residuals vs Fitted values to check homogeneity assumption */
PROC PLOT DATA=D HPERCENT=50 VPERCENT=50;
PLOT absR*P; 
RUN;
/* The absolute residual somewhat increase along with the fitted values, but not exactly. */

/* Homogeneity test using Breusch-Pagan */
PROC MODEL DATA=C; 
PARMS b0 b1 b2 b3 b4;
Y = b0 + b1*X1 + b2*X2 + b3*X3 + b4*X4;
FIT Y /WHITE BREUSCH=(X1 X2 X3 X4);
FIT Y /BREUSCH=(X1);
FIT Y /BREUSCH=(X2);
FIT Y /BREUSCH=(X3);
FIT Y /BREUSCH=(X4);
RUN;
/* p-value = 0.4255 > 0.05, we accept H0.
   H0: The variance of error is constant.
   So we conclude that the variance of error is constant.  */

/* Normality test */
PROC UNIVARIATE DATA=D NORMAL PLOT; /* Check normality of Studentized residuals */
VAR R;
RUN;
/* p-value for Shapiro-Wilk test is 0.0007 < 0.05, we reject H0.
   H0: The errors are normally distributed.
   So we conclude that The errors are not normally distributed */

/* #2 */
/* Fit the model without the outliers */
PROC REG LINEPRINTER; /* With Lineprinter option, points can be marked with specified symbols  */
MODEL Y = X1 X2 X3 X4 / R ; /* R option asks for analysis of residuals */
PAINT RSTUDENT. >2 OR RSTUDENT.<-2 / SYMBOL='*'; /* paint potential outliers by "*" in the next plot */
PLOT Y*(X1 X2 X3 X4) RSTUDENT.*PREDICTED.; 
REWEIGHT RSTUDENT.>2 OR RSTUDENT.<-2 / WEIGHT = 0.25; /* give reduced weight to outliers; weight specified here is arbitrary */
REFIT; 
PRINT;
OUTPUT OUT=D RSTUDENT=R; /* Create dataset D with RSTUDENT values */
RUN;

PROC TRANSREG Data=D;
MODEL BoxCox(Y) = identity(X1 X2 X3 X4); 
RUN;
/* Since our best lambda is 2.25, a square transformation is needed */

/* Since we found that normality of error assumption does not hold, Y needs to be transformed. */
DATA D; SET D;
sqY = Y**2;
RUN;

PROC REG Data=D;
MODEL sqY = X1 X2 X3 X4 / lackfit; 
REWEIGHT RSTUDENT.>2 OR RSTUDENT.<-2 / WEIGHT = 0.25; /* give reduced weight to outliers; weight specified here is arbitrary */
REFIT; 
PRINT;
OUTPUT OUT=E RSTUDENT=sqR PREDICTED=P; 
RUN;
/* In the residual quantile plot, we can see it has improved. */

DATA E; SET E;
abssqR = abs(sqR); /* save absolute value of residuals */
RUN;

PROC PLOT DATA = E HPERCENT=50 VPERCENT=50;
PLOT abssqR*P; /* absolute residuals vs fitted values to check homogeneity assumption */
RUN;

PROC MODEL DATA=D;
PARMS b0 b1 b2 b3 b4;
sqY = b0 + b1*X1 + b2*X2 + b3*X3 + b4*X4;
fit sqY /WHITE BREUSCH=(X1 X2 X3 X4);
fit sqY /BREUSCH=(X1);
fit sqY /BREUSCH=(X2);
fit sqY /BREUSCH=(X3);
fit sqY /BREUSCH=(X4);
RUN;
/* p-value = 0.1329 > 0.05, we accept H0.
   H0: The varince of error is not a constant.
   So we conclude that the varince of error is not a constant. 
   -> same conclusion */
   
PROC UNIVARIATE DATA=E NORMAL PLOT; /* Check normality of Studentized residuals */
VAR sqR;
RUN;
/* p-value for Shapiro-Wilk test is 0.3291 > 0.05, we accept H0.
   H0: The errors are normally distributed.
   So we conclude that the errors are normally distributed. 
   -> improvement for the conclusion */
  
/* #3 */
/* Test if X1(Doctor Availabilty) and X2(Hospital Availability) can be dropped from the model jointly */
PROC REG DATA=D; 
MODEL sqY = X1 X2 X3 X4; 
X1X2: test X1 = X2 = 0; 
RUN;
/* H0: X1 and X2 can be dropped jointly. 
   Extra sum of squares = MSR * DF = 1090.87424 * 2 = 2181.74848
   Test statistic = 1.45
   p-value = 0.2449 > 0.05, so we accept H0.
   We can conclude that X1(Doctor Availability) and X2(Hospital Availability) can be dropped from the model jointly. */

/* #4 */
/* Find the best model using adjR^2, Cp, and BIC criterion */
PROC REG DATA=D; 
MODEL sqY = X1 X2 X3 X4 / SELECTION = ADJRSQ CP BIC; /* selection is based on Adj R^2 */
RUN;
/* The model with X1 X3 X4 */

/* Stepwise Method */
PROC REG DATA=D; 
MODEL sqY = X1 X2 X3 X4 / SELECTION = STEPWISE; /* stepwise selection */
RUN;
/* The model with X4 */

/* Forward Selection Method */
PROC REG DATA=D; 
MODEL sqY = X1 X2 X3 X4 / SELECTION = F; /* stepwise selection */
RUN;
/* The model with X4 X3 X1 */
  
/* Backward Selection Method */
PROC REG DATA=D; 
MODEL sqY = X1 X2 X3 X4 / SELECTION = B; /* stepwise selection */
RUN;
/* The model with X4 */
/* The final model is the one with X4 X3 X1 */

/* #5 */
PROC GLM DATA=D; 
MODEL sqY = X4 X3 X1; 
RUN;
/* Coefficient of multiple determination = SSR/SST = 7555.06638/43992.25224 = 0.17173629435
   Coefficient of multiple correlation = sqrt(SSR/SST) = sqrt(7555.06638/43992.25224) = 0.41441077972 */

PROC CORR DATA=D;
VAR sqY X4; 
PARTIAL X1 X3;
RUN;
PROC CORR DATA=D;
VAR sqY X1; 
PARTIAL X3 X4;
RUN;
PROC CORR DATA=D;
VAR sqY X3; 
PARTIAL X1 X4;
RUN;
/* Coefficient of partial correlation
   - Ry4|1,3 = -0.27667
   - Ry1|3,4 = 0.22149
   - Ry3|1,4 = -0.27294
   Coefficient of partial determination
   - R^2y4|1,3 = (-0.27667)^2 = 0.0765462889
   - R^2y1|3,4 = (0.22149)^2 = 0.0490578201
   - R^2y3|1,4 = (-0.27294)^2 = 0.0744962436
   
/* #6 */
/* Show that the largest coefficient of partial determination = R^2y4|1,3 = (-0.27667)^2 = 0.0765462889
   is also equal to corr(Y,X4|X1,X3)^2 */
PROC GLM DATA=D; 
MODEL sqY = X1 X3; 
RUN;
/* SSE(X1,X3) = 39457.49444 */
PROC GLM DATA=D; 
MODEL sqY = X4 X3 X1; 
RUN;
/* SSR(X4|X1,X3) = 3020.308577 */
/* SSR(X4|X1,X3)/SSE(X1,X3) = 3020.308577/39457.49444 = 0.07654587854 = R^2y4|1,3 */

/* Similarly, show that the alternative interpretation of the coefficient of multiple determination holds */
/* SSR/SST = 7555.06638/43992.25224 = 0.17173629435 = R^2 */

/* #7 */
PROC REG DATA = D;
MODEL sqY = X4 X3 X1/ CLB CLM;
PLOT sqY*(X4 X3 X1)='+' p.*sqY='*' / OVERLAY;
OUTPUT OUT=NEW P=PRED;

PROC GPLOT DATA=NEW;
PLOT sqY*X4 PRED*sqY/ OVERLAY;
RUN;

PROC GPLOT DATA=NEW;
PLOT sqY*X3 PRED*sqY/ OVERLAY;
RUN;

PROC GPLOT DATA=NEW;
PLOT sqY*X1 PRED*sqY/ OVERLAY;
RUN;
