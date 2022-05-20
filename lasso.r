##########################LASSO ANALYSIS########################################
################################################################################

# LASSO Code taken from Section 1 (Instructor Provided Code), adapted to our project results.

#Load data set, and values
rm(list=ls())
data <- read.csv("jeju_data2model_pud_4WWU.csv",header <- TRUE)
attach(data)

# Use log-transformed y
y <- usdyav
y1 <- y+1
logy1 <- log(y1)

# Load variables
x1 <- commercial_km2
x2 <- culthistpl_km2
x3 <- naturalmon_km2
x4 <- road_moct_kms_100s
x5 <- sandbeach_km2
x6 <- seacliff_km2
x7 <- viewpoint
x8 <- airdist_kms_100s
x9 <- land_km2_10s
x10 <- forest_km2_10s
x11 <- athletic_km2_10s
x12 <- industrial_km2
x13 <- trails_osm_kms_100s
x14 <- nearroad_kms

#Use transformed variables
x1dummy = as.integer(x1 > 0)
x2dummy = as.integer(x2 > 0)
x3dummy = as.integer(x3 > 0)
x4sqrt = sqrt(x4)
x5dummy = as.integer(x5 > 0)
x6dummy = as.integer(x6 > 0)
x7dummy = as.integer(x7 > 0)
x8 # no transformation was foudn to be needed
x9dummy = as.integer(x9 > 0.2598)
cubertx10 = x10^(1/3)
x11dummy = as.integer(x11 > 0)
x12dummy = as.integer(x12 > 0)
x13dummy = as.integer(x13 > 0)
x14dummy = as.integer(x14 > 0)

#Multiple linear regression model 
model = lm(logy1 ~ x1dummy + x2dummy + x3dummy + x4sqrt + x5dummy + x6dummy + 
             x7dummy + x8 + x9dummy + cubertx10 + x11dummy + x12dummy + x13dummy + x14dummy ) 
summary(model)

#Perform variable selection (to be covered in Chapter 11)

#Run regression diagnostics by checking the residuals

# Here, the histogram shows a bell-like curve, centered around 0
#   Indication that we can assume normality. Neat.
resid <- model$residuals
hist(resid, freq = FALSE, main = "Model Residuals",
     xlab = "Residuals")

#Advanced topic (Lasso regression, a type of penalized least squares)
#The lasso regression performs variable selection and model estimation simultaneously.

#install.packages("glmnet")
library("glmnet")
#Using lasso
alpha = 1

#Save all the explanatory variables in data frame.
xmat <- data.frame(x1dummy = x1dummy, x2dummy = x2dummy, x3dummy = x3dummy,  x4sqrt = x4sqrt,  x5dummy = x5dummy,
                   x6dummy = x6dummy,  x7dummy = x7dummy,  x8 = x8, x9dummy = x9dummy,
                   cubertx10 = cubertx10, x11dummy = x11dummy,  x12dummy =  x12dummy, x13dummy = x13dummy,  x14dummy = x14dummy)
# Call:
#   lm(formula = logy1 ~ x1dummy + x2dummy + x3dummy + x4sqrt + x5dummy + 
#        x6dummy + x7dummy + x8 + x9dummy + cubertx10 + x11dummy + 
#        x12dummy + x13dummy + x14dummy)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.98321 -0.20411 -0.05434  0.14841  1.88853 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.13332    0.05716   2.332  0.01993 *  
#   x1dummy      0.32132    0.05806   5.534 4.22e-08 ***
#   x2dummy      0.02285    0.04057   0.563  0.57344    
# x3dummy      0.09165    0.03261   2.810  0.00507 ** 
#   x4sqrt       1.19198    0.18526   6.434 2.13e-10 ***
#   x5dummy      0.40964    0.06465   6.336 3.91e-10 ***
#   x6dummy      0.64575    0.07148   9.034  < 2e-16 ***
#   x7dummy      0.26154    0.04291   6.095 1.69e-09 ***
#   x8          -1.36031    0.15749  -8.637  < 2e-16 ***
#   x9dummy     -0.06652    0.05830  -1.141  0.25422    
# cubertx10    0.09826    0.06922   1.420  0.15610    
# x11dummy    -0.02504    0.03497  -0.716  0.47425    
# x12dummy    -0.03734    0.03270  -1.142  0.25373    
# x13dummy     0.23056    0.03026   7.620 7.08e-14 ***
#   x14dummy     0.22148    0.04879   4.540 6.49e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3552 on 809 degrees of freedom
# Multiple R-squared:  0.5004,	Adjusted R-squared:  0.4917 
# F-statistic: 57.87 on 14 and 809 DF,  p-value: < 2.2e-16

#Convert the data frame into a matrix.
xmat <- as.matrix(xmat)

#Fit the model
cvfit = cv.glmnet(xmat, logy1, alpha=alpha)

#lambda based on the 1se criterion
lambda_1se = cvfit$lambda.1se
lambda_1se # 0.03370218
# Saved lambda for result replication, May 19th 10:35 PM
lambda_1se = 0.03370218

#The ones with dots mean dropped variables. 
#The ones without dots mean estimated coefficients.
coef.lambda.1se <- coef(cvfit, s = lambda_1se)
coef.lambda.1se

# 15 x 1 sparse Matrix of class "dgCMatrix"
# s1
# (Intercept)  0.36221224
# x1dummy      0.35293730
# x2dummy      .         
# x3dummy      0.04298106
# x4sqrt       0.27607536
# x5dummy      0.29749123
# x6dummy      0.48906747
# x7dummy      0.20177644
# x8          -1.12142004
# x9dummy      .         
# cubertx10    .         
# x11dummy     .         
# x12dummy     .         
# x13dummy     0.21020464
# x14dummy     .      

# Here, we drop x2, x9, x10, x11, x12, 14 for our model, indicated by LASSO
# This leads to the following least-squares equation: 
model2 = lm(logy1 ~ x1dummy + x3dummy + x4sqrt + x5dummy + x6dummy + x7dummy + x8  + x13dummy )
summary(model2)

# Call:
#   lm(formula = logy1 ~ x1dummy + x3dummy + x4sqrt + x5dummy + x6dummy + 
#        x7dummy + x8 + x13dummy)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.02820 -0.21702 -0.05332  0.14626  1.90236 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.32668    0.03433   9.515  < 2e-16 ***
#   x1dummy      0.37815    0.05598   6.756 2.70e-11 ***
#   x3dummy      0.11197    0.03061   3.658 0.000271 ***
#   x4sqrt       0.49319    0.10619   4.644 3.97e-06 ***
#   x5dummy      0.38986    0.06513   5.986 3.22e-09 ***
#   x6dummy      0.62365    0.07169   8.700  < 2e-16 ***
#   x7dummy      0.25523    0.04324   5.903 5.25e-09 ***
#   x8          -1.41850    0.15769  -8.996  < 2e-16 ***
#   x13dummy     0.22647    0.03015   7.511 1.55e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3593 on 815 degrees of freedom
# Multiple R-squared:  0.4849,	Adjusted R-squared:  0.4799 
# F-statistic: 95.92 on 8 and 815 DF,  p-value: < 2.2e-16



#Predicted values of y (y.hat) based on the x values in the data using lambda_1se.
fit = glmnet(xmat, logy1, alpha = alpha, nlambda = 20)
# head(fit)
y.hat = predict(fit,newx=xmat,s=lambda_1se)
# head(y.hat)

#Residuals (run the regression diagnostics based on this)
resid.lasso = y-y.hat
resid.lasso

# Histogram of Residuals. Here, we can see the residuals are skewed
hist(resid.lasso)

# The QQ-Plot also shows right-skewness.
# In the QQ-Plot, we see some hints of normality, as for a large part of QQ-Plot, we have
#   that our data points follow the line. And then at the end, the points start to deviate
#   from the line. 
qqnorm(resid.lasso, pch = 1, frame = FALSE)
qqline(resid.lasso, col = "steelblue", lwd = 2)

# Residual Plot of Model
# Note how the residual plot, too has a skewness to it 
plot(fitted(model2), resid.lasso)
abline(0,0)

# Studentized Residuals
# We see that there are quite a few points above the line y=3
# Also note a lack of outliers below the line y = -3
# These two observations can explain the right-skewness of the model
# y = b0 + b1x1 + b3x3 + b4x4 + b5x5 + b6x6 + b7x7 + b8x8 + b13x3 + error
library(MASS)
stud_resid = studres(model2)
plot(x1dummy + x3dummy + x4sqrt + x5dummy + x6dummy + x7dummy + x8  + x13dummy, stud_resid,  ylab='Studentized Resid', xlab='x1dummy + x3dummy + x4sqrt + x5dummy + x6dummy + x7dummy + x8  + x13dummy') 
abline(0,0)
abline(3,0)
abline(-3,0)

# Density Plot of Residuals
# We can also note the right skewness in this plot as well, which may be explained by the outliers seen in the studentized residuals
plot(density(resid.lasso))

# Residuals vs Fitted (Cook's Distance)
# 4th-plot : Residuals vs Fitted
#   We can notice the lack of change in spread as leverage increases
plot(model2, which = 4)
plot(model2, which = 5)

##################################Interpretation of Coefficients################


################################# Possible Improvement of Model ################
# 1) We can utilize other model selection schemes to perhaps get a model that gives better residual plot diagnosis
# 2) As seen from the Residuals vs fitted plot (with Cook's Distance), we can investigate observations #748, #749, 610
#       as they have a much larger Cook's Distance than the other observations
# 3) Check for multicollinearity with VIF
