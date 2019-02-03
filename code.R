
library(MASS)
library(tidyr)
library(dplyr)
# install.packages('corrplot')
library(corrplot)
library(ggplot2)
library(ROCR)
library(tidyverse)
library(plotly)
# install.packages('GGally')
library(GGally)
# install.packages('fastDummies')
library(fastDummies)
# install.packages('ggridges')
library(ggridges)
library(viridis)
# install.packages('hrbrthemes')
library(hrbrthemes)
library(data.table)
library(gridExtra)
# install.packages('olsrr')
# install.packages('rlang')
library(olsrr)
library(car)
library(leaps)


# Set current working directory to folder containing data

################################################################
################################################################
################### EXPLORATORY DATA ANALYSIS ################## ----
################################################################
################################################################

df <- read.csv("Raw Data.csv")

#Serial number does not effect chance of admission
df$Serial.No. = NULL

# Analysing variable 'Research'
summary(factor(df$Research))
df$Research <- as.factor(df$Research)

# Selecting all Numeric Variables
df_Numeric_Variable <- select_if(df, is.numeric)
#df_Numeric_Variable

data_table <- as.data.table(df)
write.csv(data_table, 'Data.csv', row.names = FALSE)

# attach to appendix

# Summary of Data
#summary(df)
data.frame(unclass(summary(df)), check.names = FALSE, stringsAsFactors = FALSE)

# Check correlation between variables
cor(df_Numeric_Variable)

# Check correlation between variables - correlation plot
g <- ggpairs(df, mapping = aes(color = Research), columns = c(1:6,8) ,lower = list(continuous = wrap("points", alpha = 0.5,size = 0.1)),
             upper = list(continuous = wrap("cor", size = 2.5)))

g1 = g + theme(strip.placement = "outside", text = element_text(size = 5 ,margin = margin()))
g1
ggsave("p1.jpeg")

g = ggplot(df, aes(x=Research, y=Chance.of.Admit, fill=Research )) + ggtitle("Research vs Enrollment Probability") + geom_boxplot() 
g1 = g + theme (plot.title = element_text(size=7, face="bold")) + labs(x="Research", y= "Enrollment Probability (x100) %")

g1
ggsave("p2.jpeg", width = 4, height = 4)

# underlying nature of data , dist of data
g = ggplot(df,aes(x=University.Rating, fill=Research)) + ggtitle("Distribution of data based on University Ratings") +
  geom_density(color="#e9ecef", alpha=0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() + theme(plot.title = element_text(size=10, face="bold")) +
  labs(fill="") + xlab("University Rating (1-5)") + ylab("Distribution of data (x100) %")
g1 = g + guides(colour = guide_legend("Research"))

g1
ggsave("p3.jpeg", width = 4, height = 4)

#in our data, enrollment prob is higher for higher uni rting which could be an artifice of the underlying nature of data since it could be skewed towards
# students performing v well...
# reserch wih 0 are unis rated lower and 1 ar rated higher

g = ggplot(df, aes(y=as.factor(SOP), x=Chance.of.Admit,  fill=as.factor(SOP))) +
  ggtitle("Role of SOP in Enrollment Probability") + geom_density_ridges() +
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none",strip.text.x = element_text(size = 8), plot.title = element_text(size=10, face="bold")) +
  xlab("Enrollment Probability") + ylab("SOP Score")

g
ggsave("p4.jpeg", width = 4, height = 4)

# higher SOP implies higher prob of enrollment..though there are some outliers for SOP rating 5 which do have lower enrollment success
# sop 3-4 more distributed prob


g = df  %>% 
  mutate(University.Rating=as.factor(University.Rating)) %>%
  ggplot(aes(x=University.Rating, y=Chance.of.Admit, fill=as.factor(LOR))) + ggtitle("Role of LOR and Universiy Rankings in Enrollment Probability") +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_viridis(discrete=TRUE, name="") +
  theme_ipsum() + theme(plot.title = element_text(size=8, face="bold")) + 
  ylab("Enrollment Probability") +
  xlab("University Rating")

g
ggsave("p5.jpeg", width = 6, height = 6)
# for unis ranked 1 and 2 moe variation in data for enrollment probs
# thse with lor >=3 have only app to uni rating > 3
# LOR ratng has more effect on enrollmen with ower uni ratings..as the higher ting less variation in lor across enrollment success

# Multicollinearity between CGPA,GRE and TOEFL
g1 = ggplot(df, aes(x=CGPA, y=GRE.Score)) +  ggtitle("Multicollinearity amongst variables - GRE and CGPA") +
  geom_point(shape=1,aes(colour = Research)) +  geom_smooth(method=lm , color="red", se=TRUE) +
  theme(plot.title = element_text(size=8, face="bold")) 

g2 = ggplot(df, aes(x=CGPA, y=TOEFL.Score)) +  ggtitle("Multicollinearity amongst variables - TOEFL and CGPA") +
  geom_point(shape=1,aes(colour = Research)) +  geom_smooth(method=lm , color="blue", se=TRUE) +
  theme(plot.title = element_text(size=8, face="bold")) 

g1
ggsave("p6.jpeg", width = 5, height = 5)
g2
ggsave("p7.jpeg", width = 5, height = 5)


g = ggplot(df, aes(GRE.Score, Chance.of.Admit)) + ggtitle("Effect of GRE Score on Enrollment Probability") + 
  geom_point(aes(size = University.Rating,colour = University.Rating,alpha = 0.2)) + 
  geom_smooth(method="lm", color = "black", se=TRUE) +
  theme(plot.title = element_text(size=8, face="bold")) +
  ylab("Enrollment Probability") +
  xlab("GRE Score") + scale_alpha(guide = 'none') 
g1 = g + guides(colour = guide_legend("University Rating"), size = guide_legend("University Rating"), shape = guide_legend("University Rating"))+
  scale_color_gradient(low="blue", high="red")

g1
ggsave("p8.jpeg", width = 6, height = 6)

g = ggplot(df, aes(TOEFL.Score, Chance.of.Admit)) + ggtitle("Effect of TOEFL Score on Enrollment Probability") + 
  geom_point(aes(size = University.Rating,colour = University.Rating,alpha = 0.2)) + 
  geom_smooth(method="lm", color = "black", se=TRUE) +
  theme(plot.title = element_text(size=8, face="bold")) +
  ylab("Enrollment Probability") +
  xlab("TOEFL Score") + scale_alpha(guide = 'none')
g1 = g + guides(colour = guide_legend("University Rating"), size = guide_legend("University Rating"), shape = guide_legend("University Rating")) +
  scale_color_gradient(low="yellow", high="brown")

g1
ggsave("p9.jpeg", width = 6, height = 6)

g = ggplot(df, aes(CGPA, Chance.of.Admit)) + ggtitle("Effect of CGPA on Enrollment Probability") + 
  geom_point(colour = "lightpink4", size = 3,alpha = 0.5) +
  geom_smooth(method="lm", color = "black", se=TRUE) +
  theme(plot.title = element_text(size=8, face="bold")) +
  ylab("Enrollment Probability") +
  xlab("CGPA") + scale_alpha(guide = 'none')
g1 = g + scale_color_gradient(low="yellow", high="brown")

g1
ggsave("p10.jpeg", width = 6, height = 6)


################################################################
################################################################
######################## MODEL BUILDING ######################## ----
################################################################
################################################################

admission <- read.csv('Raw data.csv')

#Finding columns for which there are missing values
t(apply(admission, 2, function(x) any(is.na(x))))
#No missing values

#Range of TOEFL and GRE scores
range(admission$GRE.Score)
range(admission$TOEFL.Score)

length(unique(admission$Serial.No.))
#[1] 400

#Removing serial number as this doesnt
admission <- admission[2: ncol(admission)]

admission$Research <- as.factor(admission$Research)

#Doing step-wise selection
lm.null <- lm(Chance.of.Admit ~ 1, data = admission)
lm.full <- lm(Chance.of.Admit ~ ., data = admission)                   
step(lm.null, scope=list(lower=lm.null, upper= lm.full), direction="both")

#After running the above function we get the best fit model
linear.step <- lm(Chance.of.Admit ~ CGPA + GRE.Score + LOR + Research + TOEFL.Score, data = admission)
summary(linear.step)
"
Call:
lm(formula = Chance.of.Admit ~ CGPA + GRE.Score + LOR + Research + 
TOEFL.Score, data = admission)

Coefficients:
(Intercept)         CGPA    GRE.Score          LOR  
-1.298464     0.121004     0.001782     0.022776  
Research1  TOEFL.Score  
0.024577     0.003032 
"
linear.model <- lm(formula = Chance.of.Admit ~ CGPA + GRE.Score + LOR + Research + 
                     TOEFL.Score, data = admission)

#Significance of regression test-
#Null hypothesis-
#All the regression coefficients are zero-
n <- nrow(admission)
p <- length(coefficients(linear.model))
k <- p - 1
beta.hats <- as.vector(coefficients(linear.model))

ssr <- sum(anova(linear.model)$`Sum Sq`[1:k])
ms.r <- ssr/ k

ms.res <- anova(linear.model)$`Mean Sq`[p]

f0.val <- ms.r/ ms.res
f0.val
#[1] 320.6369

alpha <- 0.05
f.crit <- qf(alpha, df1 = k, df2 = n - p, lower.tail=FALSE)
f.crit
#[1] 2.236895

abs(f0.val) > f.crit
#[1] TRUE
p.val <- pf(f0.val, df1 = k, df2 = n - p, lower.tail=FALSE)
p.val
#[1] 0
p.val < alpha
#[1] TRUE
#Hence, the model is significant and we can reject the null hypothesis.

x0.matrix <- matrix(data = c(rep(1, n), admission$CGPA, admission$GRE.Score, admission$LOR, admission$Research, admission$TOEFL.Score), ncol = p)

c.matrix <- solve(t(x0.matrix) %*% x0.matrix)
t.crit <- qt(alpha/2, df = n - p, lower.tail=FALSE)
t.crit
#[1] 1.966003

#Performing t-tests
#T-tests failing index vactors
t.fail.vectors <- c()

for (i in 1: p) {
  t.val <- beta.hats[i] / sqrt(ms.res * c.matrix[i,i])
  if (abs(t.val) <= t.crit) {
    t.fail.vectors <- c(t.fail.vectors, i)
  }
}
t.fail.vectors
#NULL
#Selection on information criterion- AIC(Pg 336)
#Hence, no regression coefficient fails the t-tests. So, we can reject the null hypothesis of the coefficients being zero.

# the resultant model here consist of 5 variables..
# we will now use regsubsets to obtain sets of models and compare them 
# across the board to see the ideal models we get, and then compare it to
# the models we have using stepwise selection

n <- dim(admission)[1]
k <- dim(admission)[2] - 1
p <- k + 1
reg.subs <- regsubsets(Chance.of.Admit ~ ., data = admission, nbest=5)
summary(reg.subs)
R2.seq <- summary(reg.subs)$rsq
R2.seq

adj.R2.seq <- summary(reg.subs)$adjr2
adj.R2.seq

Cp.seq <- summary(reg.subs)$cp
Cp.seq

BIC.seq <- summary(reg.subs)$bic
BIC.seq

summary.reg.subs <- summary(reg.subs)

df <- cbind(data.frame('obs.num' = seq(1,31)), data.frame('var.numbers' = ceiling(seq(1,31)/5)), as.data.frame(R2.seq), as.data.frame(adj.R2.seq), as.data.frame(Cp.seq), as.data.frame(BIC.seq))

df %>% arrange(desc(R2.seq))
df %>% arrange(desc(adj.R2.seq))
df %>% arrange(Cp.seq)
df %>% arrange(desc(BIC.seq))

"
If we compare all the subsets of models based on
statistical measures such as Cp,Rsq,BIC, there are 4 models that particularly stand out compared to the rest.
Amongst the 4 models obs 21,26,27,31 -
model number 26 seems the most ideal because of relatively
high Rsq ~ 80%, and has the lowest Cp . This model
matches what we get using the stepwise selection function
"

###########################################
###########################################
#########Cross-Validation##################
###########################################
###########################################

orthnorm.GS <- function(X0.mat) {
  n <- dim(X0.mat)[[1]]
  k <- dim(X0.mat)[[2]]
  ones <- rep(x=1, times=n)
  X.mat <- as.matrix(cbind(ones, X0.mat))
  p <- dim(X.mat)[[2]]
  U.mat <- as.matrix(ones)
  for (j in 1:k) {
    y.vect <- X0.mat[,j]
    UpU.mat <- t(U.mat) %*% U.mat
    UpU.inv <- solve(UpU.mat)
    Upy.mat <- t(U.mat) %*% y.vect
    b.hat <- UpU.inv %*% Upy.mat
    res <- y.vect - U.mat %*% b.hat
    U.mat <- as.matrix(cbind(U.mat, res / sqrt(sum(res^2))))
  }
  U0.mat <- U.mat[,2:p]
  return(U0.mat)
}

'
(Intercept)         CGPA    GRE.Score          LOR  
-1.298464     0.121004     0.001782     0.022776  
Research1  TOEFL.Score  
0.024577     0.003032 

'

X0.mat.mod <- as.matrix(admission[, c(6, 1, 5, 2)])

U0.mat.GS <- orthnorm.GS(X0.mat.mod)
round(colSums(U0.mat.GS), digits=3)
U0pU0.mat <- t(U0.mat.GS) %*% U0.mat.GS
round(U0pU0.mat, digits=3)
U0.mat <- U0.mat.GS

DUPLEX.alg <- function(U0.mat) {
  n <- dim(U0.mat)[[1]]
  #k <- dim(U0.mat)[[2]]
  mode <- "est"
  est.idx.list <- numeric(length=0)
  pred.idx.list <- numeric(length=0)
  rem.idx.list <- 1:n
  nREM <- length(rem.idx.list)
  while (nREM > 1) {
    max.dist <- -Inf
    for (i in 1:(nREM-1)) {
      for (j in (i+1):nREM) {
        diff <- U0.mat[rem.idx.list[i],] - U0.mat[rem.idx.list[j],]
        dist <- sqrt(sum(diff^2))
        if (dist > max.dist) {
          max.dist <- dist
          idx.pair <- rem.idx.list[c(i,j)]
        }
      }
    }
    if (mode == "est") {
      est.idx.list <- c(est.idx.list, idx.pair)
      mode <- "pred"
    } else if (mode == "pred") {
      pred.idx.list <- c(pred.idx.list, idx.pair)
      mode <- "est"
    }
    rem.idx.list <- setdiff(rem.idx.list, idx.pair)
    nREM <- length(rem.idx.list)
  }
  if (nREM == 1) {
    if (mode == "est") {
      est.idx.list <- c(est.idx.list, rem.idx.list)
    } else if (mode == "pred") {
      pred.idx.list <- c(pred.idx.list, rem.idx.list)
    }
  }
  out.list <- list(est=est.idx.list, pred=pred.idx.list)
  return(out.list)
}

valid <- DUPLEX.alg(U0.mat)
valid$est

valid$pred


#The modified Snee's score is 
U0.est <- U0.mat[valid$est,]
U0pU0.est <- t(U0.est) %*% U0.est
U0pU0.det.est <- det(U0pU0.est)
U0pU0.det.est
#[1] 0.06244188

U0.pred <- U0.mat[valid$pred,]
U0pU0.pred <- t(U0.pred) %*% U0.pred
U0pU0.det.pred <- det(U0pU0.pred)
U0pU0.det.pred
#[1] 0.05648999

Snee.score <- (U0pU0.det.est / U0pU0.det.pred)^(1/k)
Snee.score
#[1] 1.020237
#Based on the Snee score valid$pred is the prediction set and valid$est is the train set.

#Doing CV on the split-
linear.model.train <- lm(formula = Chance.of.Admit ~ CGPA + GRE.Score + LOR + Research + TOEFL.Score, data = admission[valid$est,])
summary_train <- summary(linear.model.train)
summary_train

y.hat <- predict(linear.model.train, data = admission[valid$pred,])
mean.squared.error <- mean((y.hat - admission[valid$pred,]$Chance.of.Admit) ^ 2)
mean.squared.error
#[1] 0.03599495

mean.absolute.error <- mean(abs(y.hat - admission[valid$pred,]$Chance.of.Admit))
mean.absolute.error
#[1] 0.1519133

#Doing CV on the split without research in the model-
linear.model.train <- lm(formula = Chance.of.Admit ~ CGPA + GRE.Score + LOR + TOEFL.Score, data = admission[valid$est,])
summary_train <- summary(linear.model.train)
summary_train

y.hat <- predict(linear.model.train, data = admission[valid$pred,])
mean.squared.error <- mean((y.hat - admission[valid$pred,]$Chance.of.Admit) ^ 2)
mean.squared.error
#[1] 0.03592315

mean.absolute.error <- mean(abs(y.hat - admission[valid$pred,]$Chance.of.Admit))
mean.absolute.error
#[1] 0.1521245
#Use the whole set for the model adequacy checks.

pairs(admission)
#There appears to be a linear relation between GRE score and Chance.
#There appears to be a linear relation between TOEFL score and Chance.
#There appears to be a linear relation between CGPA and Chance.
#There appears to be a linear relation between LOR and Chance.
#There appears to be a linear relation between SOP and Chance.

#GRE score and TOEFL score seem to be linear in relation.
#Hypothesis - They are multicollinear
cor(matrix(data = c(admission$GRE.Score, admission$TOEFL.Score), ncol = 2))
#It is 0.8359768 indicating multicollinearity. Will decide later which one to take.

#GRE score and CGPA seem to be linear in relation.
#Hypothesis - They are multicollinear
cor(matrix(data = c(admission$GRE.Score, admission$CGPA, admission$TOEFL.Score, admission$SOP, admission$LOR), ncol = 5))
#Multicollinearity is indicated between TOEFL, GRE and CGPA

################################################################
################################################################
################### MODEL ADEQUACY CHECKING #################### ----
################################################################
################################################################

## We keep only the selected variables and the outcome in our dataframe

adm_selected <- admission[, c("Chance.of.Admit", "CGPA", "GRE.Score", "LOR", "TOEFL.Score","Research")]

## convert Research to a type numeric column, as it is already one hot encoded.
adm_selected$Research <- as.numeric(adm_selected$Research)

n <- dim(adm_selected)[[1]]
y.vect <- as.matrix(adm_selected[,1])
colnames(y.vect)[1] <- "Chance.of.Admit"
X0.mat <- as.matrix(adm_selected[,2:6])
k <- dim(X0.mat)[[2]]
X.mat <- cbind(rep(x=1, times=n), X0.mat)
colnames(X.mat)[1] <- "intercept"
p <- dim(X.mat)[[2]]
XpX.mat <- t(X.mat) %*% X.mat
Xpy.mat <- t(X.mat) %*% y.vect
XpX.inv <- solve(XpX.mat)
b.hat <- XpX.inv %*% Xpy.mat
y.hat <- X.mat %*% b.hat
SS.Res <- as.numeric(t(y.vect) %*% y.vect) - as.numeric(t(y.hat) %*% y.vect)
df.Res <- n - p
MS.Res <- SS.Res / df.Res
linear.model.full <- lm(Chance.of.Admit ~ CGPA + GRE.Score + LOR + TOEFL.Score + Research, data=adm_selected)

## Getting the residuals
res <- y.vect - y.hat
res

## The standardized residuals are                         

std.res <- res / sqrt(MS.Res)
std.res

## The hat diagonals are                                  |

h.val <- hatvalues(linear.model.full)

## Studentized residuals:
stud.res <- rstandard(linear.model.full)

## PRESS Residuals:
PRESS.res <- res / (1-h.val)
PRESS.res

## R Student Residuals:
R.stud.res <- rstudent(linear.model.full)

admission.res <- data.frame(res, std.res, stud.res, PRESS.res, R.stud.res)
names(admission.res) <- c("e", "d", "r", "PRESS.e", "t")
admission.res

### PLOTS

## Normal Probability plot of residuals:
qqnorm(admission.res$t, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals")
qqline(admission.res$t)
## Shows heavy tailed, but almost all points do fall on straight line

## Residual-by-fitted-value plot:
plot(y.hat, admission.res$t, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot")
lines(c(min(y.hat), max(y.hat)), c(0,0), type="l", lty=1, lwd=3)

# Shows double bow pattern
# Refer Page 139 of book
# The double - bow pattern in panel c often occurs
# when y is a proportion between zero and 1

## Residual-by-regressor plots
par(mfrow = c(2, 2))
plot(adm_selected$CGPA, admission.res$t, pch=16, cex=1, xlab="CGPA", ylab="R-student residual", main = "Residual-by-regressor plot:CGPA")
lines(c(min(adm_selected$CGPA), max(adm_selected$CGPA)), c(0,0), type="l", lty=1, lwd=3)

plot(adm_selected$GRE.Score, admission.res$t, pch=16, cex=1, xlab="GRE.Score", ylab="R-student residual", main = "Residual-by-regressor plot:GRE.Score")
lines(c(min(adm_selected$GRE.Score), max(adm_selected$GRE.Score)), c(0,0), type="l", lty=1, lwd=3)

plot(adm_selected$LOR, admission.res$t, pch=16, cex=1, xlab="LOR", ylab="R-student residual", main = "Residual-by-regressor plot:LOR")
lines(c(min(adm_selected$LOR), max(adm_selected$LOR)), c(0,0), type="l", lty=1, lwd=3)

plot(adm_selected$TOEFL.Score, admission.res$t, pch=16, cex=1, xlab="TOEFL.Score", ylab="R-student residual", main = "Residual-by-regressor plot:TOEFL.Score")
lines(c(min(adm_selected$TOEFL.Score), max(adm_selected$TOEFL.Score)), c(0,0), type="l", lty=1, lwd=3)

## Same as the Residual vs Fitted value plot

## Partial regression plots:

y.x2x3x4x5 <- resid(lm(Chance.of.Admit ~ GRE.Score + LOR + TOEFL.Score + Research, data=adm_selected))
x1.x2x3x4x5 <- resid(lm(CGPA ~ GRE.Score + LOR + TOEFL.Score + Research, data=adm_selected))

y.x1x3x4x5 <- resid(lm(Chance.of.Admit ~ CGPA + LOR + TOEFL.Score + Research, data=adm_selected))
x2.x1x3x4x5 <- resid(lm(GRE.Score ~ CGPA + LOR + TOEFL.Score + Research, data=adm_selected))

y.x1x2x4x5 <- resid(lm(Chance.of.Admit ~ CGPA + GRE.Score + TOEFL.Score + Research, data=adm_selected))
x3.x1x2x4x5 <- resid(lm(LOR ~ CGPA + GRE.Score + TOEFL.Score + Research, data=adm_selected))

y.x1x2x3x5 <- resid(lm(Chance.of.Admit ~ CGPA + GRE.Score + LOR + Research, data=adm_selected))
x4.x1x2x3x5 <- resid(lm(TOEFL.Score ~ CGPA + GRE.Score + LOR + Research, data=adm_selected))

y.x1x2x3x4 <- resid(lm(Chance.of.Admit ~ CGPA + GRE.Score + LOR + TOEFL.Score, data=adm_selected))
x5.x1x2x3x4 <- resid(lm(Research ~ CGPA + GRE.Score + LOR + TOEFL.Score, data=adm_selected))


# mai=c(0.7,0.7,0.2,0.1)
par(mfrow = c(2, 2)) # mai=c(bottom, left, top, right)
# plot(delivery.data$cases, delivery.data$time, pch=16, cex=1, xlab="cases", ylab="time", main = "raw data")
# plot(delivery.data$distance, delivery.data$time, pch=16, cex=1, xlab="distance", ylab="time", main = "raw data")
plot(x1.x2x3x4x5, y.x2x3x4x5, pch=16, cex=1, xlab="CGPA", ylab="Chance.of.Admit", main = "partial regression plot:CGPA")
plot(x2.x1x3x4x5, y.x1x3x4x5, pch=16, cex=1, xlab="GRE.Score", ylab="Chance.of.Admit", main = "partial regression plot:GRE.Score")
plot(x3.x1x2x4x5, y.x1x2x4x5, pch=16, cex=1, xlab="LOR", ylab="Chance.of.Admit", main = "partial regression plot:LOR")
plot(x4.x1x2x3x5, y.x1x2x3x5, pch=16, cex=1, xlab="TOEFL.Score", ylab="Chance.of.Admit", main = "partial regression plot:TOEFL.Score")
plot(x5.x1x2x3x4, y.x1x2x3x4, pch=16, cex=1, xlab="Research", ylab="Chance.of.Admit", main = "partial regression plot:Research")
par(mfrow = c(1, 1))

## No discernible linear relationships can be seen between the regressors and the outcome

## The PRESS Statistic:
PRESS.stat <- sum(PRESS.res^2)
PRESS.stat

## Contribution of each point to the PRESS statistic
PRESS.contrib = round((100*PRESS.res^2 / PRESS.stat), 4)

PRESS.contrib.gt1 <- PRESS.contrib[PRESS.contrib > 1]
PRESS.points <- which(PRESS.contrib > 1)

cbind(PRESS.points,PRESS.contrib.gt1)
#       PRESS.points PRESS.contrib.gt1
#  [1,]           10            4.3151
#  [2,]           11            2.8849
#  [3,]           41            2.1237
#  [4,]           42            1.7301
#  [5,]           43            1.4370
#  [6,]           60            2.5004
#  [7,]           62            1.3121
#  [8,]           64            1.3680
#  [9,]           65            3.2803
# [10,]           66            3.2608
# [11,]           67            2.0043
# [12,]           68            1.7632
# [13,]           69            1.4131
# [14,]           81            1.2075
# [15,]           92            1.5069
# [16,]           93            3.3162
# [17,]           94            1.1965
# [18,]           95            1.7149
# [19,]           96            1.0875
# [20,]          104            1.0103
# [21,]          116            1.1800
# [22,]          328            1.6131
# [23,]          359            1.4481
# [24,]          360            1.5251
# [25,]          375            1.9362
# [26,]          376            1.4729
# [27,]          377            1.0086
# [28,]          387            1.0351

## LACK OF FIT TEST

library(alr3)
pureErrorAnova(linear.model.full)
# Analysis of Variance Table
# 
# Response: Chance.of.Admit
#               Df Sum Sq Mean Sq   F value    Pr(>F)    
# CGPA          1 6.1885  6.1885 1490.5591 < 2.2e-16 ***
# GRE.Score     1 0.1496  0.1496   36.0303 4.402e-09 ***
# LOR           1 0.1059  0.1059   25.5104 6.736e-07 ***
# TOEFL.Score   1 0.0307  0.0307    7.3887  0.006853 ** 
# Residuals   395 1.6400  0.0042                        
# ---
#         Signif. codes:  0 â***â 0.001 â**â 0.01 â*â 0.05 â.â 0.1 â â 1


## Variance Stabilizing Transformations:
## Since outcome y here is a proportion, we will use the recommended transformation to stabilize the variance
## y* = arcsin(sqrt(y))

y.new <- as.matrix(asin(sqrt(adm_selected$Chance.of.Admit)))

adm_new <- cbind(y.new, adm_selected)

linear.model.full.new <- lm(y.new ~ CGPA + GRE.Score + LOR + TOEFL.Score + Research, data=adm_new[valid$est,])

# Residual-by-fitted-value plot:

y.hat.new <- round(predict(linear.model.full.new, newdata = adm_new[valid$pred, ]), 2)
y.hat.trans <- (sin(y.hat.new))^2
ms.res.new <- mean((y.hat.trans - adm_new$Chance.of.Admit[valid$est])^2)
ms.res.new
# [1] 0.03284787
R.stud.res.new <- rstudent(linear.model.full.new)

par(mfrow=c(1,1))
plot(y.hat.new, R.stud.res.new, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot")
abline(h=0, lty=1, lwd=3)

qqnorm(R.stud.res.new, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals")
qqline(R.stud.res.new)
# Lack of fit test on the new model

pureErrorAnova(linear.model.full.new)
# 
# Response: Chance.of.Admit
#              Df Sum Sq Mean Sq   F value    Pr(>F)    
# CGPA          1 6.1885  6.1885 1523.1205 < 2.2e-16 ***
# GRE.Score     1 0.1496  0.1496   36.8174 3.048e-09 ***
# LOR           1 0.1059  0.1059   26.0677 5.142e-07 ***
# TOEFL.Score   1 0.0307  0.0307    7.5501  0.006276 ** 
# Research      1 0.0391  0.0391    9.6288  0.002054 ** 
# Residuals   394 1.6008  0.0041                        
# ---
#         Signif. codes:  0 â***â 0.001 â**â 0.01 â*â 0.05 â.â 0.1 â â 1

## Applying Ridge Regression to this data

# -------------------------------------------------------+
# R function for ridge regression                        |
# -------------------------------------------------------+

ridge.reg.coefficients <- function(y.vect, X0.mat, plot=TRUE, grid.size=25, grid.st=0.001, grid.fn=0.5) {
  # Collect parameters
  n <- dim(X0.mat)[1]
  k <- dim(X0.mat)[2]
  p <- k + 1
  # Unit-length scaling
  y.bar <- mean(y.vect)
  y.cent <- y.vect - y.bar
  SS.T <- sum(y.cent^2)
  y.vect.scl <- y.vect / sqrt(SS.T)
  X.mat.scl <- matrix(data=NA, nrow=n, ncol=k)
  x.bar.list <- numeric(length=k)
  css.list <- numeric(length=k)
  for (j in 1:k) {
    x.bar.list[j] <- mean(X0.mat[,j])
    xj.cent <- X0.mat[,j] - x.bar.list[j]
    css.list[j] <- sum(xj.cent^2)
    X.mat.scl[,j] <- xj.cent / sqrt(css.list[j])
  }
  # Calculate ridge trace diagram
  ridge.k.grid <- exp(seq(from=log(grid.st), to=log(grid.fn), length.out=grid.size))
  b.hat.R.scl.list <- matrix(data=0, nrow=k, ncol=grid.size)
  y.vect.aug <- rbind(y.vect.scl, matrix(data=0, nrow=k, ncol=1))
  for (iVAL in 1:grid.size) {
    ridge.k.val <- ridge.k.grid[iVAL]
    X.mat.aug <- rbind(X.mat.scl, sqrt(ridge.k.val)*diag(k))
    XpX.mat.aug <- t(X.mat.aug) %*% X.mat.aug
    Xpy.mat.aug <- t(X.mat.aug) %*% y.vect.aug
    XpX.inv.aug <- solve(XpX.mat.aug)
    b.hat.R.scl.list[,iVAL] <- XpX.inv.aug %*% Xpy.mat.aug
  }
  if (plot) {
    plot(ridge.k.grid, rep(x=0, times=grid.size), pch=3, cex=1, ylim=c(min(b.hat.R.scl.list), max(b.hat.R.scl.list)), xlab="ridge constant, k", ylab="fitted ridge regression coefficient", main = "Ridge trace diagram")
    abline(h=0, lty=1, lwd=1)
    for (j in 1:k) {
      lines(ridge.k.grid, b.hat.R.scl.list[j,], type="l", lty=1, lwd=3)
    }
  }
  # Convert to the original scale and calculate MS.Res and R2.
  X.mat <- as.matrix(cbind(rep(x=1, times=n), X0.mat))
  b.hat.R.list <- matrix(data=0, nrow=p, ncol=grid.size)
  SS.Res.list <- numeric(length=grid.size)
  R2.list <- numeric(length=grid.size)
  for (iVAL in 1:grid.size) {
    b.hat.R.list[1,iVAL] <- y.bar
    for (j in 1:k) {
      b.hat.R.list[j+1,iVAL] <- b.hat.R.scl.list[j,iVAL] / sqrt(css.list[j] / SS.T)
      b.hat.R.list[1,iVAL] <- b.hat.R.list[1,iVAL] - b.hat.R.list[j+1,iVAL]*x.bar.list[j]
    }
    SS.Res.list[iVAL] <- sum((y.vect - X.mat %*% b.hat.R.list[,iVAL])^2)
    R2.list[iVAL] <- 1 - SS.Res.list[iVAL] / SS.T
  }
  MS.Res.list <- SS.Res.list / (n-p)
  out.list <- list(ridge.k.grid=ridge.k.grid, b.hat.R.list=b.hat.R.list, MS.Res.list=MS.Res.list, R2.list=R2.list)
  return(out.list)
}

# -------------------------------------------------------+
# R function for principle components regression         |
# -------------------------------------------------------+

prin.comp.coefficients <- function(y.vect, X0.mat) {
  # Collect parameters
  n <- dim(X0.mat)[1]
  k <- dim(X0.mat)[2]
  p <- k + 1
  # Unit-length scaling
  y.bar <- mean(y.vect)
  y.cent <- y.vect - y.bar
  SS.T <- sum(y.cent^2)
  y.vect.scl <- y.vect / sqrt(SS.T)
  X.mat.scl <- matrix(data=NA, nrow=n, ncol=k)
  x.bar.list <- numeric(length=k)
  css.list <- numeric(length=k)
  for (j in 1:k) {
    x.bar.list[j] <- mean(X0.mat[,j])
    xj.cent <- X0.mat[,j] - x.bar.list[j]
    css.list[j] <- sum(xj.cent^2)
    X.mat.scl[,j] <- xj.cent / sqrt(css.list[j])
  }
  # Calculate principal components and coefficient estimates
  XpX.mat.scl <- t(X.mat.scl) %*% X.mat.scl
  eig.out <- eigen(XpX.mat.scl)
  Lambda.mat <- diag(eig.out$values)
  Lambda.inv <- diag(1/eig.out$values)
  T.mat <- eig.out$vectors
  Z.mat <- X.mat.scl %*% T.mat
  Zpy.mat <- t(Z.mat) %*% y.vect.scl
  a.hat.scl <- Lambda.inv %*% Zpy.mat[1:j,]
  a.hat.PC.scl.list <- matrix(data=0, nrow=k, ncol=k)
  b.hat.PC.scl.list <- matrix(data=0, nrow=k, ncol=k)
  for (j in 1:k) {
    a.hat.PC.scl.list[1:j,j] <- a.hat.scl[1:j,]
    b.hat.PC.scl.list[,j] <- T.mat %*% a.hat.PC.scl.list[,j]
  }
  # Convert to the original scale and calculate MS.Res and R2.
  X.mat <- as.matrix(cbind(rep(x=1, times=n), X0.mat))
  grid.size <- dim(b.hat.PC.scl.list)[2]
  b.hat.PC.list <- matrix(data=0, nrow=p, ncol=grid.size)
  SS.Res.list <- numeric(length=grid.size)
  R2.list <- numeric(length=grid.size)
  for (iVAL in 1:grid.size) {
    b.hat.PC.list[1,iVAL] <- y.bar
    for (j in 1:k) {
      b.hat.PC.list[j+1,iVAL] <- b.hat.PC.scl.list[j,iVAL] / sqrt(css.list[j] / SS.T)
      b.hat.PC.list[1,iVAL] <- b.hat.PC.list[1,iVAL] - b.hat.PC.list[j+1,iVAL]*x.bar.list[j]
    }
    SS.Res.list[iVAL] <- sum((y.vect - X.mat %*% b.hat.PC.list[,iVAL])^2)
    R2.list[iVAL] <- 1 - SS.Res.list[iVAL] / SS.T
  }
  MS.Res.list <- SS.Res.list / (n-p)
  out.list <- list(b.hat.PC.list=b.hat.PC.list, MS.Res.list=MS.Res.list, R2.list=R2.list)
  return(out.list)
}


ridge.lm <- ridge.reg.coefficients(as.matrix(y.new[valid$est]), X0.mat[valid$est,])

iVAL <- 22
b.hat.R <- ridge.lm$b.hat.R.list[,iVAL]
b.hat.R
# [1] -1.447804794  0.093192869  0.002914575  0.029032324  0.005745357
# [6]  0.028768139

MS.Res.R <- ridge.lm$MS.Res.list[iVAL]
MS.Res.R
# [1] 0.004334246

R2.R <- ridge.lm$R2.list[iVAL]
R2.R
# [1] 0.8589865

## Predicting on the validation set
X0.mat.predict <- cbind(rep(1, length(valid$pred)), X0.mat[valid$pred,])

y.predict <- X0.mat.predict %*% as.matrix(b.hat.R)
y.predict.trans <- round((sin(y.predict))^2, 2)

#Mean square error of this model is 
ms.res.new.ridge <- mean((y.predict.trans - adm_new$Chance.of.Admit[valid$est])^2)
ms.res.new.ridge
# [1] 0.031698
R.stud.res.new <- rstudent(linear.model.full.new)


## Fitting on the whole dataset
ridge.lm.full <- ridge.reg.coefficients(as.matrix(y.new), X0.mat)
ridge.lm.full

iVAL <- 22
b.hat.R.full <- ridge.lm.full$b.hat.R.list[,iVAL]
b.hat.R.full
# [1] -1.402721079  0.096499350  0.002804666  0.030303789  0.005235441
# [6]  0.032763695

MS.Res.R.full <- ridge.lm.full$MS.Res.list[iVAL]
MS.Res.R.full
# [1] 0.005301699

R2.R.full <- ridge.lm.full$R2.list[iVAL]
R2.R.full
# [1] 0.8186113

y.vect.new <- as.matrix(y.new)
X0.mat.intercept <- cbind(rep(1, length(dim(X0.mat)[1])), X0.mat)
SS.Res.ridge <- t(y.vect.new) %*% (y.vect.new) - t(b.hat.R.full) %*% t(X0.mat.intercept) %*% y.vect.new
SS.Res.ridge
# 2.672771

MSRes.ridge <- as.numeric(SS.Res.ridge)/(n-p)
# [1] 0.006783684

SS.T.ridge <- sum(y.new^2) - (sum(y.new))^2/n
SS.T.ridge
# [1] 11.51599

SS.R.ridge <- SS.T.ridge - SS.Res.ridge
SS.R.ridge
# [1,] 8.843215

MS.R.ridge <- as.numeric(SS.R.ridge/k)
MS.R.ridge
# [1] 1.768643

F0.ridge <- MS.R.ridge/MSRes.ridge
F0.ridge
# [1] 260.7201

p.val.ridge <- pf(F0.ridge, 1, n-2, lower.tail=FALSE)
p.val.ridge
# [1] 1.805475e-45

## As this value is less than 0.05, this passes the siginificance of regression test.
alpha
c.matrix <- solve(t(X0.mat.intercept) %*% X0.mat.intercept)
t.crit.ridge <- qt(alpha/2, df = n - p, lower.tail=FALSE)
t.crit.ridge
# [1] 1.966003

t.fail.vectors <- c()

for (i in 1: p) {
  t.val <- b.hat.R.full[i] / sqrt(MSRes.ridge * c.matrix[i,i])
  if (abs(t.val) <= t.crit) {
    t.fail.vectors <- c(t.fail.vectors, i)
  }
}
t.fail.vectors
# NULL

X0.mat.predict <- cbind(rep(1, length(valid$pred)), X0.mat[valid$pred,])
C.mat.predict <- solve(t(X0.mat.predict) %*% X0.mat.predict)

lower.conf <- numeric(length(y.predict))
upper.conf <- numeric(length(y.predict))
max.err.diff <- 0
min.err.diff <- 1000
mean.err.diff <- 0

for (i in 1 : length(y.predict)) {
  row.matrix <- as.matrix(as.numeric(X0.mat.predict[i,]))
  err.margin <- t.crit.ridge * sqrt(MS.R.ridge *(1 + t(row.matrix) %*% C.mat.predict %*% row.matrix))
  
  lower.conf[i] <- y.new[i] - err.margin
  upper.conf[i] <- y.new[i] + err.margin
  
  max.err.diff <- max(2 * err.margin, max.err.diff)
  min.err.diff <- min(2 * err.margin, min.err.diff)
  mean.err.diff <- mean.err.diff + 2 * err.margin
}

#Mean prediction band length
mean.err.diff <- mean.err.diff/ length(y.predict)
as.numeric(mean.err.diff)
#[1] 5.306915

#Minimum prediction band length
as.numeric(min.err.diff)
#[1] 5.255512

#Maximum prediction band length
as.numeric(max.err.diff)
#[1] 5.444683

conf.table <- data.frame('lower.prediction.interval' = lower.conf, 'predicted.vals' = y.predict, 'upper.prediction.interval' = upper.conf)
conf.table <- head(conf.table)
# export to image
png("table_conf.png", height = 50*nrow(conf.table), width = 200*ncol(conf.table))
grid.table(conf.table)
dev.off()



################################################################
################################################################
############ DIAGNOSTICS FOR LEVERAGE AND INFLUENCE ############ ----
################################################################
################################################################

# data with final variables
adm_selected <- admission[, c("Chance.of.Admit", "CGPA", "GRE.Score", "LOR", "TOEFL.Score", "Research")]
# data with transformed response
adm_selected$Chance.of.Admit <- asin(sqrt(adm_selected$Chance.of.Admit))
# transformed linear model
linear.model.full <- lm(Chance.of.Admit ~ CGPA + GRE.Score + LOR + TOEFL.Score + Research, data=adm_selected)

################################################################

# get influence measures
infl.measures <- influence.measures(linear.model.full)
infl.measures <- data.frame(infl.measures$infmat)

# data dimensions
n <- nrow(adm_selected)
p <- ncol(adm_selected)

# leverage cutoff
hat.cutoff <- 2 * p/n

# determine leverage observations based on hat diagonal values
hat.lvrge <- data.frame('h.ii' = infl.measures$hat, 'leverage' = ifelse(infl.measures$hat > hat.cutoff, '*', '.'))

# create table
pts_selected <- which(hat.lvrge$leverage == '*')
leverage_points <- data.frame(adm_selected[pts_selected, ], 'Leverage' = infl.measures$hat[pts_selected])

# export to image
png("leverage.png", height = 50*nrow(leverage_points), width = 200*ncol(leverage_points))
grid.table(leverage_points)
dev.off()

# leverage points that are influential
leverage <- infl.measures$hat
r.student <- rstudent(linear.model.full)
title <- "R-Student vs Leverage to Determine Influence points"
label <- paste("Threshold:", hat.cutoff)
ggplot(adm_selected, aes(leverage, r.student)) + 
  geom_point(shape = 16) + 
  xlab("Leverage") + 
  ylab("R-Student") + 
  ggtitle(title) + 
  geom_hline(yintercept = 2, colour = "maroon") +
  geom_vline(xintercept = hat.cutoff, colour = "maroon") +  
  annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2, family = "serif", fontface = "italic", colour = "darkred", label = label)

# removing leverage points and building the model
data_ <- adm_selected[-pts_selected,]
lm_lvrg <- lm(Chance.of.Admit ~ CGPA + GRE.Score + LOR + TOEFL.Score + Research, data=data_)
anova_full <- anova(linear.model.full)
anova_lvrg <- anova(lm_lvrg)
table_ <- data.frame(cbind(t(coefficients(linear.model.full)), anova_full$`Mean Sq`[6]))
table_ <- rbind(table_, data.frame(cbind(t(coefficients(lm_lvrg)), anova_lvrg$`Mean Sq`[6])))

# display models
colnames(table_) <- c('b.Intercept', 'b.CGPA', 'b.GRE.Score', 'b.LOR', 'b.TOEFL.Score', 'b.Research', 'MS.Res')
rownames(table_) <- c('full model', 'model without leverage points')

# export to image
png("table_lvrg.png", height = 50*nrow(table_), width = 200*ncol(table_))
grid.table(table_)
dev.off()

################################################################
# cooks.d cutoff
cooks.d.cutoff <- 1

# determine influencial observations based on cooks.d values
cooks.d.infl <- data.frame('cooks.d' = infl.measures$cook.d, 'infl' = ifelse(infl.measures$cook.d > cooks.d.cutoff, '*', '.'))

# cooks.d bar plot
ggplot(adm_selected, aes(x = c(1:nrow(adm_selected)), y = infl.measures$cook.d)) + geom_bar(width = 0.5, stat = "identity")  + 
  ylab("Cook's D") + xlab("Observation") + ggtitle("Cook's D Bar Plot") 


################################################################
# dffits cutoff
dffits.cutoff <- 2 * sqrt(p/n)

# determine influencial observations based on dffits values
dffits.infl <- data.frame('dffits' = infl.measures$dffit, 'infl' = ifelse(abs(infl.measures$dffit) > dffits.cutoff, '*', '.'))

# plot
dffits(linear.model.full)

# create table
pts_selected <- which(dffits.infl$infl == '*')
infl.dffits_points <- data.frame(adm_selected[pts_selected, ], 'DFFITS Influence' = infl.measures$dffit[pts_selected])

# export to image
png("dffits.influence.png", height = 50*nrow(infl.dffits_points), width = 200*ncol(infl.dffits_points))
grid.table(infl.dffits_points)
dev.off()

# removing leverage points and building the model
data_ <- adm_selected[-pts_selected,]
lm_dffits_infl <- lm(Chance.of.Admit ~ CGPA + GRE.Score + LOR + TOEFL.Score + Research, data=data_)
anova_ <- anova(lm_dffits_infl)
table_ <- data.frame(cbind(t(coefficients(linear.model.full)), anova_full$`Mean Sq`[6]))
table_ <- rbind(table_, data.frame(cbind(t(coefficients(lm_dffits_infl)), anova_$`Mean Sq`[6])))

# display models
colnames(table_) <- c('b.Intercept', 'b.CGPA', 'b.GRE.Score', 'b.LOR', 'b.TOEFL.Score', 'b.Research', 'MS.Res')
rownames(table_) <- c('full model', 'model without influence points')

# export to image
png("table_dffits.png", height = 50*nrow(table_), width = 200*ncol(table_))
grid.table(table_)
dev.off()

################################################################
# dfbetas cutoff
dfbetas.cutoff <- 2 / sqrt(n)

# determine influencial observations based on dfbetas values
dfbetas.infl <- data.frame(infl.measures[1:5], ifelse(abs(dfbetas(linear.model.full)) > dfbetas.cutoff, '*', '.'))
colnames(dfbetas.infl) <- c("dfb.Intercept", "dfb.CGPA", "dfb.GRE", "dfb.LOR", "dfb.TOEF", "infl.Intercept", "infl.CGPA", "infl.GRE.Score", "infl.LOR", "infl.TOEFL.Score", "infl.Research")

# plot
dfbeta(linear.model.full)

# create table
pts_selected <- which(apply(dfbetas.infl, 1, function(r) any(r == '*')))
infl.dfbetas_points <- data.frame(adm_selected[pts_selected, ], infl.measures[pts_selected, c(1:6)])

# export to image
png("dfbetas.influence.png", height = 50*nrow(infl.dfbetas_points), width = 200*ncol(infl.dfbetas_points))
grid.table(infl.dfbetas_points)
dev.off()

################################################################
# covration cutoff
covratio.cutoff.lo <- 1 - 3*p/n
covratio.cutoff.hi <- 1 + 3*p/n

# determine influencial observations based on covratio values
cov.ratio <- covratio(linear.model.full)
cov.infl <- data.frame('cov.ratio' = cov.ratio, 'infl' = ifelse(((cov.ratio < covratio.cutoff.lo) | (cov.ratio > covratio.cutoff.hi)), '*', '.'))

# create table
pts_selected <- which(cov.infl$infl == '*')
infl.cov_points <- data.frame(adm_selected[pts_selected, ], 'COVRATIO' = cov.infl$cov.ratio[pts_selected])

# sort table by covratio
infl.cov_points <- infl.cov_points[order(infl.cov_points$COVRATIO), ]

# export to image
png("cov.infl.png", height = 50*nrow(infl.cov_points), width = 200*ncol(infl.cov_points))
grid.table(infl.cov_points)
dev.off()

# removing leverage points and building the model
degrading_points <- which(infl.cov_points$COVRATIO < covratio.cutoff.lo)
data_ <- adm_selected[-degrading_points,]
lm_cov_infl <- lm(Chance.of.Admit ~ CGPA + GRE.Score + LOR + TOEFL.Score + Research, data=data_)
anova_ <- anova(lm_cov_infl)
table_ <- data.frame(cbind(t(coefficients(linear.model.full)), anova_full$`Mean Sq`[6]))
table_ <- rbind(table_, data.frame(cbind(t(coefficients(lm_dffits_infl)), anova_$`Mean Sq`[6])))

enhancing_points <- which(infl.cov_points$COVRATIO > covratio.cutoff.hi)
data_ <- adm_selected[-enhancing_points,]
lm_cov_infl <- lm(Chance.of.Admit ~ CGPA + GRE.Score + LOR + TOEFL.Score + Research, data=data_)
anova_ <- anova(lm_cov_infl)
table_ <- rbind(table_, data.frame(cbind(t(coefficients(lm_cov_infl)), anova_$`Mean Sq`[6])))

data_ <- adm_selected[-pts_selected,]
lm_cov_infl <- lm(Chance.of.Admit ~ CGPA + GRE.Score + LOR + TOEFL.Score + Research, data=data_)
anova_ <- anova(lm_cov_infl)
table_ <- rbind(table_, data.frame(cbind(t(coefficients(lm_cov_infl)), anova_$`Mean Sq`[6])))

# display models
colnames(table_) <- c('b.Intercept', 'b.CGPA', 'b.GRE.Score', 'b.LOR', 'b.TOEFL.Score', 'b.Research', 'MS.Res')
rownames(table_) <- c('full model', 'model without degrading points', 'model without enhancing points', 'model without influence points')

# export to image
png("table_cov.png", height = 50*nrow(table_), width = 200*ncol(table_))
grid.table(table_)
dev.off()

################################################################

