# Part 1: Load data from source and pre-process.

housing <- read.csv("housing.csv",
                    stringsAsFactors = TRUE)
View(housing)
summary(housing)

# Since the "ocean_proximity" column is categorical, convert to numerical.
unique(housing$ocean_proximity)
housing$ocean_proximity <- as.numeric(as.factor(housing$ocean_proximity))

# Since "total_bedrooms" column has 207 missing entries (NA's), 
#   remove these instances from dataset.
housing <- na.omit(housing)

View(housing)
summary(housing)


# Part 2: Further analysis of data after pre-processing.

# Correlation between features.
require(corrplot)
hc <- cor(housing, use="pairwise.complete.obs")
corrplot(hc, method = "circle")

cor_hc <- as.data.frame(corrplot(hc, method = "number"))
names(cor_hc) <- names(housing)
View(abs(cor_hc[,9, drop=FALSE]))


# Part 3: Linear Models

# Build model to predict median_housing_value for every other column.
d = NULL
par(mfrow=c(3,3))
for (attr in colnames(housing)) {
  if (attr == "median_house_value"){
    next
  }
  lr <- lm(housing[,"median_house_value"]~housing[,attr])
  plot(housing[,attr], housing$median_house_value,
       xlab = attr, ylab = "median_house_value")
  abline(lr, lwd=3, col="red")
  
  sum <- summary(lr)
  coeff <- paste(sum$coefficients[1], sum$coefficients[2], sep = ", ")
  std_err <- paste(sum$coefficients[3], sum$coefficients[4], sep = ", ")
  t <- paste(sum$coefficients[5], sum$coefficients[6], sep = ", ")
  p <- paste(sum$coefficients[7], sum$coefficients[8], sep = ", ")
  r2 <- sum$r.squared
  adj_r2 <- sum$adj.r.squared
  f <- sum$fstatistic
  
  d = rbind(d, data.frame(attr, coeff, std_err, t, p, r2, adj_r2, f))
}
d = d[c(1,4,7,10,13,16,19,22,25),]
row.names(d) <- NULL
colnames(d) <- c("Attribute", "Coefficients(intercept, attribute)", 
                 "Standard_Error(intercept, attribute)", 
                 "T-Value(intercept, attribute)",
                 "P-Value(intercept, attribute)",
                 "R_Squared", "Adjusted_R_Squared", "F-Statistic")
View(d)
View(d[,c("Attribute","R_Squared", "Adjusted_R_Squared", "F-Statistic")])


# Build model using median_income and latitude.
lm.fit.2 = lm(median_house_value~ median_income + latitude, data=housing)
summary(lm.fit.2)
plot(lm.fit.2)

# Build model using median_income, latitude, and total rooms.
lm.fit.3 = lm(median_house_value~ median_income + latitude + total_rooms, data=housing)
summary(lm.fit.3)
plot(lm.fit.3)

# Build model using all attributes.
lm.fit.all = lm(median_house_value~., data=housing)
summary(lm.fit.all)
plot(lm.fit.all)