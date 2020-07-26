library(tidyverse)
library(readxl)
library(psych)
library(dplyr)
library(summarytools)
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

#### B2. UNDERSTANDING THE DATASET ####

#### B.2.1 data gathering #### 
df <- read_excel('Factor Analysis.xls', skip = 1)
df <- df[,15:25]

#### B.2.2. EDA ####
dim(df)
str(df)
descr(df)

# box plot 
boxplot(df, main = 'Box Plot')

# correlation plot
library(corrplot)
# correlation matrix cor.matrix
cor.ma <- cor(df)
corrplot(cor.ma, tl.col = 'black', method = "number", type = "upper")



#### B.3. Checking conditions/assumptions for factor analysis ####
#### B.3.2.KMO Statistics ####
KMO(cor.ma)

#### B.3.3. Matrix Determinant #### 
det(cor.ma)

#### B.3.4. Bartlett's Spherecity Test ####
cortest.bartlett(df, n=20)



#### B.4. EFA ####
#### B.4.1. Choosing the number of factors
# fitting full model factors, i.e. 23
pc1 <- principal(df, nfactors=11, rotate="none")
pc1
fa.diagram(pc1)
# h2 (communalities) = all equal to 1
# u2 (unique variance) = all equal to 0

# plotting eigenvalues
plot(pc1$values, type="b", 
     xlab = 'No. Variables',
     ylab = 'Eigenvalues',
     main = 'Scree plot') 
abline(h = 1)
# COMMENT: the requirement for the number of factors is 2
# --> choosing 2 factors for the FA model 


#### B.4.2. factoring without rotation ####
pc2 <- principal(df, nfactors=2, rotate="none")
pc2
fa.diagram(pc2)
factor.plot(pc2, labels=rownames(pc2$loadings), 
            xlim = c(0,1),
            ylim = c(-0.8,0.8))


#### B.4.3. EFA with orthogonal (varimax) rotation ####
pc3 <- principal(df, nfactors = 2, rotate="varimax")
pc3
fa.diagram(pc3)
factor.plot(pc3, labels=rownames(pc3$loadings),
            xlim = c(-0.2, 1),
            ylim = c(-0.2, 1),
            cex = 0.9)
print.psych(pc3, cut = 0.51, sort = TRUE)
#How do we interpret these?

#### B.4.4. EFA with oblique rotation ####
pc4 <- principal(df, nfactors = 2, rotate = "promax", scores = TRUE)
pc4
fa.diagram(pc4)
print.psych(pc4, cut = 0.42, sort = TRUE)


#### B.4.5. Reliability Analysis #### 
Group1 <- df[,c(1,4,8,9,10)]
Group2 <- df[,c(2,3,5,6,7,11)]

alpha(Group1)
alpha(Group2)
