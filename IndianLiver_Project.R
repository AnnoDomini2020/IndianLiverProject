##### Load Packages and Dataset #####
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(utils)) install.packages("utils", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(splines)) install.packages("splines", repos = "http://cran.us.r-project.org")
if(!require(foreach)) install.packages("foreach", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(pander)) install.packages("pander", repos = "http://cran.us.r-project.org")

data <- unzip(zipfile = "archive.zip",                  # Unzip ZIP file for dataset. If file is not
              files = "indian_liver_patient.csv")       # in working directory, use setwd() to set 
                                                        # file directory as working directory.
il_data <- read.csv(file = data, header = TRUE,              # Dataset is a comma-separated values text
                    sep = ",", stringsAsFactors = FALSE)     # file.
                                                        
rm(data)                                                # Remove temporary file.

##### Examine and Clean Dataset #####
str(il_data)      # Dataset consists of 583 observations for 11 variables. Each observation pertains to a patient.
head(il_data)     # The variables include age, gender, eight chemical variables, and diagnosis. Diagnosis is named
colnames(il_data) # "dataset." Dataset = 1 if the patient is diagnosed with liver disease and 2 if otherwise.

sum(il_data$Dataset == 1)   # 416 liver disease patients.
sum(il_data$Dataset == 2)   # 167 healthy patients.
sum(is.na(il_data))         # Four entries contain missing data.
which(is.na(il_data),       # Identify horizontal and vertical indices of missing entries.
      arr.ind = TRUE)
colnames(il_data)[10]       # Four patients are missing ambulin:globulin ratio data.
il_data <- na.omit(il_data) # Remove them from the dataset.

v_data <- il_data %>%       # Convert diagnosis status to character strings for visualization.
  mutate(Diagnosis = ifelse(Dataset == 1, "Liver Disease", "No Disease"))

# Graphic 1: Diagnostic demographics of patients.
v_data %>%
  group_by(Diagnosis) %>%
  summarise(Percentage = 100*n()/579) %>%
  ggplot(aes(Diagnosis, Percentage, fill = Diagnosis)) +
  geom_bar(stat = "identity") +
  ggtitle("Figure 1: Percentage of Patients by Diagnosis")

class(il_data$Gender)            # Gender is included as a character variable.
range(il_data$Gender)            # "Female", "Male"
mean(il_data$Gender == "Female") # Proportion female.
mean(il_data$Gender == "Male")   # Proportion male.

il_data <- il_data %>%           # Convert it to a factor variable for analytical purposes.
  mutate(Gender = as.integer(as.factor(Gender)))
mean(il_data$Gender == 1)        # Proportion female.
mean(il_data$Gender == 2)        # Proportion male.

# Graphic 2: Gender demographics of patients.
v_data %>%
  group_by(Gender) %>%
  summarise(Percentage = 100*n()/579) %>%
  ggplot(aes(Gender, Percentage, fill = Gender)) +
  geom_bar(stat = "identity") +
  ggtitle("Figure 2: Percentage of Patients by Gender")

# Graphic 3: Liver disease rate by gender.
v_data %>%
  ggplot(aes(Gender, fill = Diagnosis)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion") +
  ggtitle("Figure 3: Liver Disease Rate by Gender")

# Graphic 4: Gender composition of diagnostic group.
v_data %>%
  ggplot(aes(Diagnosis, fill = Gender)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion") +
  ggtitle("Figure 4: Gender Demographics of Diagnostic Groups")

##### Examine and Clean Dataset (Cont.) #####
# Graphic 5: Average patient age by diagnosis.
v_data %>%
  group_by(Diagnosis) %>%
  summarize(`Mean Age` = mean(Age)) %>%
  ggplot(aes(Diagnosis, `Mean Age`, fill = Diagnosis)) +
  geom_bar(stat = "identity") +
  ggtitle("Figure 5: Median Patient Age by Diagnosis")

# Graphic 6: Average patient age by gender and diagnosis
v_data %>%
  group_by(Gender, Diagnosis) %>%
  summarize(`Mean Age` = mean(Age)) %>%
  ggplot(aes(Gender, `Mean Age`, fill = Diagnosis)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Figure 6: Median Patient Age by Diagnosis and Gender")

mean(il_data$Age)           # Average patient age.
median(il_data$Age)         # Median patient age.
ua <- unique(il_data$Age)   # Non-duplicate patient ages.
ua[which.max(tabulate(      # Count the number of patients for each age.
  match(il_data$Age, ua)))] # Mode patient age.
range(il_data$Age)          # Patient age range.
sd(il_data$Age)             # Patient age standard deviation.

l_data <- il_data %>%
  filter(Dataset == 1)      # Liver patients only.
range(l_data$Age)           # Liver patient age range.
sd(l_data$Age)              # Liver patient age standard deviation.
mean(l_data$Age)            # Average liver patient age.
median(l_data$Age)          # Median liver patient age.
ual <- unique(l_data$Age)   # Mode liver patient age:
ual[which.max(tabulate(match(l_data$Age, ual)))]

h_data <- il_data %>%
  filter(Dataset == 2)      # Healthy patients only.
range(h_data$Age)           # Healthy patient age range.
sd(h_data$Age)              # Healthy patient age standard deviation.
mean(h_data$Age)            # Average healthy patient age.
median(h_data$Age)          # Median healthy patient age.
uah <- unique(h_data$Age)   # Mode healthy patient age:
uah[which.max(tabulate(match(h_data$Age, uah)))]

# Table 1: Summary Statistics of Patient Ages.
summary_stats <- data.frame("Diagnosis" = c("All Patients", "Liver Patients", "Healthy Patients"),
                            "Mean" = round(c(mean(il_data$Age), mean(l_data$Age), mean(h_data$Age)), 2), 
                            "Median" = c(median(il_data$Age), median(l_data$Age), median(h_data$Age)),
                            "Mode" = c(ua[which.max(tabulate(match(il_data$Age, ua)))], 
                                       ual[which.max(tabulate(match(l_data$Age, ual)))], 
                                       uah[which.max(tabulate(match(h_data$Age, uah)))]
                            ),
                            "Min." = c(min(il_data$Age), min(l_data$Age), min(h_data$Age)),
                            "Max." = c(max(il_data$Age), max(l_data$Age), max(h_data$Age)),
                            "S.D." = round(c(sd(il_data$Age), sd(l_data$Age), sd(h_data$Age)), 2)
)
kable(summary_stats,        # Display for PDF output in RMD.
      caption = "Summary Statistics of Patient Ages in Years")

# Graphic 7: Density plot of patient ages.
il_data %>%
  ggplot(aes(Age)) +        # Note, patients aged older than 90 are listed as 90 years old.
  xlab("Patient Age") +     # We see a spike bewtween 50 and 63.
  ylab("Density (Proportion Within Age Group)") +
  ggtitle("Figure 7: Patient Age Distribution") +
  geom_density(bw = 5, fill = "grey")

# Graphic 8: Density plot of patient ages, differentiated by diagnosis.
v_data %>%
  mutate(group = Diagnosis) %>%
  ggplot(aes(Age, fill = Diagnosis)) +
  geom_density(alpha = 0.5, bw = 5) +          # Transparent curves, some more smoothing for legibility.
  ggtitle("Figure 8: Patient Age Distribution by Diagnosis") +
  xlab("Patient Age") +
  ylab("Density (Proportion Within Age Group)")

##### Matrix Creation and Standardization #####
x <- as.matrix(il_data[,1:10])           # Create matrix from dataset (excluding diagnoses). (Note lower-case x.)
X <- sweep(x, 2, colMeans(x), FUN = "-") # Standardize matrix variables. (Note upper-case X.)
X <- sweep(X, 2, colSds(x), FUN = "/")   # Exploratory analysis will demonstrate its necessity.
colnames(X) <- colnames(il_data)[1:10]   # Restore variable names.

range(colSds(X))   # Standardization sets the standard deviation of each matrix column to 1
range(colMeans(X)) # and average value of each column to a constant order of magnitude.
range(colSds(x))   # Note that both statistics range across four orders of magnitude in the
range(colMeans(x)) # unstandardized matrix.
colMins(x)         # Unstandardized minimum values.
colMins(X)         # Standardized minimum values.
colMaxs(x)         # Unstandardized maximum values.
colMaxs(X)         # Standardized maximum values.

# Table 2: Summary statistics for each dataset variable.
data.frame(Correlation = cor(il_data, il_data$Dataset),  # Correlation w/ datset.
           Min. = sapply(il_data, min),                  # Minimum value.
           Max. = sapply(il_data, max),                  # Maximum value.
           Mean = sapply(il_data, mean),                 # Mean value.
           Median = sapply(il_data, median),             # Median value.
           S.D. = sapply(il_data, sd)) %>%               # Standard deviation.
  kable(caption = "Variable Correlation w/ Dataset and Summary Statistics.")

# Table 3: Correlations between standardized predictors and outcomes.
t3 <- cor(X, il_data$Dataset)
colnames(t3) <- "Diagnostic Correlation"
t3 %>% kable(caption = "Correlations Between Standardized Predictors and Outcomes.")

data.frame(Minimum = sapply(il_data, min),
           Maximum = sapply(il_data, max)) %>%
  kable(caption = "il_data Variable Ranges")

d <- as.matrix(dist(X))                  # Matrix of distances between all patients.
str(d)                                   # 579x579 matrix.
unique(diag(d))                          # All diagonals equal 0.
mean(d[1, which(il_data$Dataset == 1)])  # Average distance between liver patients.
mean(d[1, which(il_data$Dataset == 2)])  # Average distance between healthy patients.
d2 <- as.matrix(dist(t(X)))              # Transposed (10x10) distance matrix.

##### Unsupervised Machine Learning #####
# Graphic 9: Hierarchical clustering.
tX <- t(X)
rownames(tX) <- c("Age", "TP", "TB", "DB", "Albumin",   # Abbreviate variable names
                  "Gender", "AGR", "ALP", "ALT", "AST") # for visualization.
plot(hclust(dist(tX)),
     main = "Figure 9: Patient Feature Dendrogram", 
     xlab = NA, sub = NA)

# Graphic 10: Heatmap.
heatmap(as.matrix(dist(tX)), col = RColorBrewer::brewer.pal(11, "RdBu"),
        main = "Figure 10: 10x10 Standardized Feature Matrix Heatmap")

# Graphic 11: Bad heatmap. (This is why we use standardization!)
tx <- t(x)      # Transposed unstandardized matrix.
rownames(tx) <- c("Age", "TP", "TB", "DB", "Albumin", 
                  "Gender", "AGR", "ALP", "ALT", "AST")
heatmap(as.matrix(dist(tx)),
        col = RColorBrewer::brewer.pal(11, "RdBu"),
        main = "Figure 11: 10x10 Unstandardized Feature Matrix Heatmap")

# Principal component analysis:
pca <- prcomp(X) # Principal component data summarizes the variance of the matrix in fewer dimensions.
summary(pca)     # The first seven principal componnents explain ~96% of the variance.

# Table 4: Present summary results for R Markdown.
pander(summary(pca)$importance,
       caption = "Principal Component Analysis Summary",
       summary = TRUE)

# Graphic 12: Plot points in first two principal components, differentiate by diagnosis.
pcs <- data.frame(pca$x, Status = v_data$Diagnosis) # Encode principal components for visualization.
pcs %>%
  ggplot(aes(PC1, PC2, col = Status)) +             # Color by diagnosis.
  geom_point() +
  ggtitle("Figure 12: Principal Components 1, 2 and Diagnosis")

# Graphic 13: Plot only the first seven principal components, differentiate by diagnosis.
data.frame(PC = pcs[, 1:7],
           Status = v_data$Diagnosis) %>%
  gather(key = "PC", 
         value = "Value", -Status) %>%
  ggplot(aes(PC, Value, fill = Status)) +
  geom_boxplot() +
  ggtitle("Figure 13: Principal Components 1-7 and Diagnosis")

# Graphic 14: Plot last three principal components, differentiate by diagnosis.
data.frame(PC = pcs[, 8:10],
           Status = v_data$Diagnosis) %>%
  gather(key = "PC", 
         value = "Value", -Status) %>%
  ggplot(aes(PC, Value, fill = Status)) +
  geom_boxplot() +
  ggtitle("Figure 14: Principal Components 8-10 and Diagnosis")

##### Set Splitting #####
props <- seq(0.1, 0.5, 0.01)                           # Candidate proportions for the test set.
set.seed(1, sample.kind = "Rounding")                  # Set seed to obtain consistent results for report.
disp2 <- sapply(props, function(p){                    # Calculate under-representation of a diagnosis status in test data.
  disp <- replicate(1000, {                            # Results vary randomly with each partition. Replicate yields stability.
    ind <- createDataPartition(il_data$Dataset,
                               times = 1, p = p, 
                               list = FALSE)
    trainY <- as.factor(il_data$Dataset[-ind])
    testY <- as.factor(il_data$Dataset[ind])
    abs(mean(trainY == 1) - mean(testY == 1))          # Different partitions may under-represent either status.
  })                                                   # Absolute value presents consistent relationship.
  return(mean(2*disp))                                 # Under-representation of one diagnosis status necessitates over-
})                                                     # representation of the other. Doubling each disparity gives full picture.

# Graphic 15:  Full disparities plotted for each test set proportion. 
ggplot(data.frame(Proportions = props,
                  Disparities = disp2),
       aes(Proportions, Disparities)) + 
  geom_smooth() +
  ggtitle("Figure 15: Diagnostic Disparity Between Training and Test Sets, by Test Set Proportion")

# Partition:
set.seed(1, sample.kind = "Rounding")                  # Set seed to obtain consistent results for report.
ind <- createDataPartition(il_data$Dataset, times = 1, # Partition 20% of data into test set,
                           p = 0.2, list = FALSE)      # 80% of data into training set.
trainX <- X[-ind,]                                     # Matrix training set.
testX <- X[ind,]                                       # Matrix testing set
trainY <- as.factor(il_data$Dataset[-ind])             # Diagnoses for training data.
testY <- as.factor(il_data$Dataset[ind])               # Diagnoses for testing data.
ldi <- which(as.numeric(testY) == 1)                   # Indices of liver patients. (For sensitivity tests.)
ndi <- which(as.numeric(testY) == 2)                   # Indices of healthy patients. (For specificity tests.)

##### k-means #####
predict_kmeans <- function(x, k){ # Predict the k-means clusters that would fit each patient.
  c <- k$centers                  # Cluster centers identified by a k-means test.
  d <- sapply(1:nrow(x), function(i){
    apply(c, 1, function(y) dist(rbind(x[i,], y)))
  })                              # Distance to cluster centers.
  max.col(-t(d))                  # Cluster with minimal distance from center.
}

set.seed(1, sample.kind = "Rounding")         # Set seed to achieve consistent partition.
sub_ind <- sample(nrow(trainX),               # Create a subset of training set for validation.
                  round(0.25*nrow(trainX)))   # Partition 25% of training set into validation set.
set.seed(1, sample.kind = "Rounding")         # Re-set seed to achieve consistent k-means computation.
k <- kmeans(trainX[-sub_ind,], centers = 2,   # Derive two clusters to fit training subset.
            nstart = 1)                       # Choose one random starting point for deriving clusters.
p <- predict_kmeans(trainX[sub_ind,], k)      # Predict the clusters for the validation set.
100*mean(p == as.numeric(trainY[sub_ind]))    # Test percent accuracy of k-means training.

set.seed(1, sample.kind = "Rounding")
Accuracies <- sapply(1:25, function(i){       # Testing accuracies for different numbers of random starting points.
  k <- kmeans(trainX[-sub_ind,], centers = 2, # For each i, kmeans will execute i iterations, each starting from
              nstart = i)                     # random points.
  p <- predict_kmeans(trainX[sub_ind,], k)
  100*mean(p == as.numeric(trainY[sub_ind]))  # Report the predictive accuracy for each number i of iterations.
})

# Graphic 16: k-Means Accuracy vs. nstart.
ggplot(data.frame(Iteration_Count = 1:25, 
                  Accuracy = Accuracies),
       aes(Iteration_Count, Accuracy)) + 
  geom_point() +
  xlab("Iteration Count") +
  ggtitle("Figure 16: k-Means Accuracy by Number of Random Starting Points")

set.seed(1, sample.kind = "Rounding")
k <- kmeans(trainX, centers = 2,      # Use entire training set.
            nstart = 25)              # Iterate from 25 random starting points.
p_km <- predict_kmeans(testX, k)      # Predictions for test set.
acc_km <- mean(p_km == testY)         # Accuracy of test set predictions.
sen_km <- mean(p_km[ldi] == 1)        # Sensitivity of test set predictions.
spe_km <- mean(p_km[ndi] == 2)        # Specificity of test set predictions.

##### Logistic Regression #####
# Bootstrap:
set.seed(1, sample.kind = "Rounding")
b_fit_glm <- train(trainX, trainY, method = "glm")
100*b_fit_glm$results$Accuracy               # Percent accuracy of training set fits.
b_p_glm <- predict(b_fit_glm, testX)         # Test set predictions.
b_acc_glm <- confusionMatrix(b_p_glm,        # Accuracy of test set predictions.
                testY)$overall["Accuracy"]   # Same as mean(b_p_glm == testY).
b_sen_glm <- mean(b_p_glm[ldi] == 1)         # Sensitivity of test set predictions.
b_spe_glm <- mean(b_p_glm[ndi] == 2)         # Specificity of test set predictions.

# K-fold:
set.seed(1, sample.kind = "Rounding")
k_fit_glm <- train(trainX, trainY, 
                   method = "glm",           # Use 25-fold cross validation instead of bootstrapping. 
                   trControl = trainControl(method = "cv"))
100*k_fit_glm$results$Accuracy
k_p_glm <- predict(k_fit_glm, testX)
k_acc_glm <- confusionMatrix(k_p_glm, testY)$overall["Accuracy"]
k_sen_glm <- mean(k_p_glm[ldi] == 1)
k_spe_glm <- mean(k_p_glm[ndi] == 2)

##### LDA #####
# Bootstrap:
set.seed(1, sample.kind = "Rounding")
b_fit_lda <- train(trainX, trainY, method = "lda")
100*b_fit_lda$results$Accuracy
b_p_lda <- predict(b_fit_lda, testX)
b_acc_lda <- confusionMatrix(b_p_lda, testY)$overall["Accuracy"]
b_sen_lda <- mean(b_p_lda[ldi] == 1) # Sensitivity
b_spe_lda <- mean(b_p_lda[ndi] == 2) # Specificity

# K-fold:
set.seed(1, sample.kind = "Rounding")
k_fit_lda <- train(trainX, trainY, method = "lda", trControl = trainControl(method = "cv"))
100*k_fit_lda$results$Accuracy
k_p_lda <- predict(k_fit_lda, testX)
k_acc_lda <- confusionMatrix(k_p_lda, testY)$overall["Accuracy"]
k_sen_lda <- mean(k_p_lda[ldi] == 1)
k_spe_lda <- mean(k_p_lda[ndi] == 2)

##### QDA #####
# Bootstrap:
set.seed(1, sample.kind = "Rounding")
b_fit_qda <- train(trainX, trainY, method = "qda")
100*b_fit_qda$results$Accuracy
b_p_qda <- predict(b_fit_qda, testX)
b_acc_qda <- confusionMatrix(b_p_qda, testY)$overall["Accuracy"]
b_sen_qda <- mean(b_p_qda[ldi] == 1) # Sensitivity
b_spe_qda <- mean(b_p_qda[ndi] == 2) # Specificity

# K-fold:
set.seed(1, sample.kind = "Rounding")
k_fit_qda <- train(trainX, trainY, method = "qda", trControl = trainControl(method = "cv"))
100*k_fit_qda$results$Accuracy
k_p_qda <- predict(k_fit_qda, testX)
k_acc_qda <- confusionMatrix(k_p_qda, testY)$overall["Accuracy"]
k_sen_qda <- mean(k_p_qda[ldi] == 1)
k_spe_qda <- mean(k_p_qda[ndi] == 2)

##### Locally Estimated Scatterplot Smoothing #####
# Bootstrap:
s <- seq(0.45, 0.55, 0.01)                # Independently vary span. Originally tested from 0.45 to 0.65.
set.seed(1, sample.kind = "Rounding")
b_fit_loess <- train(trainX, trainY,
                   method = "gamLoess",   # Degree must be constant. Attempts to adjust it caused system failure.
                   tuneGrid = data.frame(span = s, degree = 1))
b_fit_loess$bestTune                      # Best span value.
100*b_fit_loess$results$Accuracy[8]       # Percent accuracy of best span value.
b_p_loess <- predict(b_fit_loess, testX)  # Test set predictions.
b_acc_loess <- confusionMatrix(b_p_loess, # Accuracy of test set predictions
                             testY)$overall["Accuracy"]
b_sen_loess <- mean(b_p_loess[ldi] == 1)  # Sensitivity of test set predictions.
b_spe_loess <- mean(b_p_loess[ndi] == 2)  # Specificity of test set predictions.

# Graphic 16: Plot bootstrap accuracies with respect to span.
ggplot(data.frame(Span = s,
                  Accuracy = b_fit_loess$results$Accuracy),
       aes(Span, Accuracy)) + 
  geom_point() + 
  geom_line() +
  ggtitle("Figure 16: Loess Fit Accuracy by Span (Bootstrap)")

# K-fold:
s <- seq(0.35, 0.45, 0.01)                # The previous sequence yielded a peak at 0.45 for K-fold. Originally tested
set.seed(1, sample.kind = "Rounding")     # from 0.25 to 0.45.
k_fit_loess <- train(trainX, trainY, method = "gamLoess",
                     tuneGrid = data.frame(span = s, degree = 1),
                     trControl = trainControl(method = "cv"))
k_fit_loess$bestTune
100*k_fit_loess$results$Accuracy[11]
k_p_loess <- predict(k_fit_loess, testX)
k_acc_loess <- confusionMatrix(k_p_loess, testY)$overall["Accuracy"]
k_sen_loess <- mean(k_p_loess[ldi] == 1)
k_spe_loess <- mean(k_p_loess[ndi] == 2)

# Graphic 17: Plot K-fold accuracies with respect to span.
ggplot(data.frame(Span = s,
                  Accuracy = k_fit_loess$results$Accuracy),
       aes(Span, Accuracy)) + 
  geom_point() + 
  geom_line() +
  ggtitle("Figure 17: Loess Fit Accuracy by Span (K-fold)")

##### k-Nearest Neighbors #####
# Bootstrap:
k <- seq(1, 100, 1)                        # Independently varied numbers of nearst neighbors.
set.seed(1, sample.kind = "Rounding")
b_fit_knn <- train(trainX, trainY, method = "knn",
                 tuneGrid = data.frame(k = k))

b_fit_knn$bestTune                         # The most accurate value is 87, but its low kappa measure indicates
100*b_fit_knn$results$Accuracy[87]         # that it may not be the most appropriate value.
b_p_knn_87 <- predict(b_fit_knn, testX)    # Predictions generated on k = 87 will later demonstrate why 87 is
b_acc_knn_87 <- mean(b_p_knn_87 == testY)  # not the right choice.
b_sen_knn_87 <- mean(b_p_knn_87[ldi] == 1) # Sensitivity for k = 87.
b_spe_knn_87 <- mean(b_p_knn_87[ndi] == 2) # Specificity for k = 87.
b_fit_knn$results$Kappa[87]                # Kappa value for k = 87

# Graphic 18: kNN Fit Results by k. (Bootstrap)
b_knn_results <- data.frame(k, Kappa = b_fit_knn$results$Kappa,  # Plot Cohen's kappa statistic alongside accuracy
                          Accuracy = b_fit_knn$results$Accuracy) # results for kNN fits by k.
ggplot(b_knn_results, aes(x = k)) +                              # Note, high values of k yield diminishing returns
  geom_point(aes(y = Accuracy), color = "blue") +                # in accuracy and declines in kappa.
  geom_line(aes(y = Accuracy), color = "blue") +                 # Kappa indicates the likelihood that a given result
  geom_point(aes(y = Kappa), color = "red") +                    # (accuracy) is not due to chance. In a sense, it
  geom_line(aes(y = Kappa), color = "red") +                     # functions as the inverse of a p-value.
  xlab("k") +
  ylab("Kappa and Accuracy") +
  theme(legend.position = "top") +
  ggtitle("Figure 18: kNN Fit Results by k (Bootstrap)") +
  geom_text(aes(x = 0, y = 0.70), label = "Accuracy", color = "blue") +
  geom_text(aes(x = 0, y = 0.25), label = "Kappa", color = "red")

# Bootstrap: (without over-training)
100*b_fit_knn$results$Accuracy[27]         # 27 is nearly as accurate and yields the highest kappa value before
b_fit_knn <- train(trainX, trainY,         # kappa values constantly trend toward zero. k = 27 will be chosen to
                   method = "knn",           # officially predict the test set values.
                   tuneGrid = data.frame(k = 27))
b_p_knn <- predict(b_fit_knn, testX)       # Test set predictions for k = 27.
b_acc_knn <- mean(b_p_knn == testY)        # Accuracy for k = 27.
b_sen_knn <- mean(b_p_knn[ldi] == 1)       # Sensitivity for k = 27.
b_spe_knn <- mean(b_p_knn[ndi] == 2)       # Specificity for k = 27.
b_fit_knn$results$Kappa[27]                # Kappa value for k = 27

# K-fold:
set.seed(1, sample.kind = "Rounding")
k_fit_knn <- train(trainX, trainY, method = "knn",
                 tuneGrid = data.frame(k = k),
                 trControl = trainControl(method = "cv"))

k_fit_knn$bestTune                      # k = 34 doesn't yield the highest kappa value, but there's a significant spike there,
100*k_fit_knn$results$Accuracy[34]      # it's comparable to the kappa value for k = 27 in bootstrapping, and it yields the
k_fit_knn$results$Kappa[34]             # highest accuracy in the domain, hence it is used in the final model.
k_p_knn <- predict(k_fit_knn, testX)
k_acc_knn <- mean(k_p_knn == testY)
k_sen_knn <- mean(k_p_knn[ldi] == 1)
k_spe_knn <- mean(k_p_knn[ndi] == 2)

# Graphic 19: kNN Fit Results by k. (K-fold)
k_knn_results <- data.frame(k, Kappa = k_fit_knn$results$Kappa,
                          Accuracy = k_fit_knn$results$Accuracy)
ggplot(k_knn_results, aes(x = k)) +
  geom_point(aes(y = Accuracy), color = "blue") +
  geom_line(aes(y = Accuracy), color = "blue") +
  geom_point(aes(y = Kappa), color = "red") +
  geom_line(aes(y = Kappa), color = "red") +
  xlab("k") +
  ylab("Kappa and Accuracy") +
  theme(legend.position = "top") +
  ggtitle("Figure 19: kNN Fit Results by k (K-fold)") +
  geom_text(aes(x = 0, y = 0.70), label = "Accuracy", color = "blue") +
  geom_text(aes(x = 0, y = 0.25), label = "Kappa", color = "red")

##### Random Forest #####
# Bootstrap:
m <- seq(1, 5, 1)                   # Originally tested from 1 to 17.
set.seed(1, sample.kind = "Rounding")
b_fit_rf <- train(trainX, trainY, method = "rf",
                tuneGrid = data.frame(mtry = m),
                importance = TRUE)  # Assess importance of predictors.
b_fit_rf$bestTune
100*b_fit_rf$results$Accuracy[1]
b_p_rf <- predict(b_fit_rf, testX)  # Test set predictions for Random Forest.
b_acc_rf <- confusionMatrix(b_p_rf, testY)$overall["Accuracy"]
b_sen_rf <- mean(b_p_rf[ldi] == 1)  # Sensitivity
b_spe_rf <- mean(b_p_rf[ndi] == 2)  # Specificity

# Graphic 20: Random Forest Accuracy by mtry.
ggplot(data.frame(mtry = m, Accuracy = b_fit_rf$results$Accuracy),
       aes(mtry, Accuracy)) + 
  geom_point() + 
  geom_line() +
  ggtitle("Figure 20: Random Forest Accuracy by mtry (Bootstrap)")

# Table 5:
bimp <- varImp(b_fit_rf)$importance[1]
colnames(bimp) <- "Importance"
arrange(bimp, desc(Importance)) %>% 
  kable(caption = "Bootstrap Random Forest Variable Importance")

# K-fold:
set.seed(1, sample.kind = "Rounding")
k_fit_rf <- train(trainX, trainY, method = "rf",
                  tuneGrid = data.frame(mtry = m),
                  trControl = trainControl(method = "cv"),
                  importance = TRUE)
k_fit_rf$bestTune
100*k_fit_rf$results$Accuracy[1]
k_p_rf <- predict(k_fit_rf, testX)
k_acc_rf <- confusionMatrix(k_p_rf, testY)$overall["Accuracy"]
k_sen_rf <- mean(k_p_rf[ldi] == 1)
k_spe_rf <- mean(k_p_rf[ndi] == 2)

# Graphic 21: Random Forest Accuracy by mtry.
ggplot(data.frame(mtry = m, Accuracy = k_fit_rf$results$Accuracy),
       aes(mtry, Accuracy)) + 
  geom_point() + 
  geom_line() +
  ggtitle("Figure 21: Random Forest Accuracy by mtry (K-fold)")

# Table 6:
kimp <- varImp(k_fit_rf)$importance[1]
colnames(kimp) <- "Importance"
arrange(kimp, desc(Importance)) %>% 
  kable(caption = "K-fold Random Forest Variable Importance")

##### Ensemble #####
# Bootstrap:
b_ensemble <- cbind(b_p_glm, b_p_qda, b_p_lda,   # Compile the results of all algorithms trained via bootstrap
                    b_p_loess, b_p_knn, b_p_rf)  # cross validation.
b_vote <- sapply(1:nrow(testX), function(i){     # Predict liver disease if majority of algorithms
  ifelse(mean(b_ensemble[i,] == 1) >= 0.5, 1, 2) # predict liver disease for a given patient.
})                                               # Save all ensemble test set predictions.
b_acc_ens <- mean(b_vote == testY)      # Accuracy of bootstrap ensemble predictions.
b_sen_ens <- mean(b_vote[ldi] == 1)     # Sensitivity of bootstrap ensemble predictions.
b_spe_ens <- mean(b_vote[ndi] == 2)     # Specificity of bootstrap ensemble predictions.

# K-fold:
k_ensemble <- cbind(k_p_glm, k_p_qda, k_p_lda,   # Compile the results of all algorithms trained via K-fold
                    k_p_loess, k_p_knn, k_p_rf)  # cross validation.
k_vote <- sapply(1:nrow(testX), function(i){
  ifelse(mean(k_ensemble[i,] == 1) >= 0.5, 1, 2)
})
k_acc_ens <- mean(k_vote == testY)      # Accuracy of K-fold ensemble predictions.
k_sen_ens <- mean(k_vote[ldi] == 1)     # Sensitivity of K-fold ensemble predictions.
k_spe_ens <- mean(k_vote[ndi] == 2)     # Specificity of K-fold ensemble predictions.

# Complete ensemble:
c_ensemble <- cbind(k_p_glm, k_p_qda, k_p_lda, k_p_loess, # Include k-means, bootstrap, and K-fold algorithms.
                    k_p_knn, k_p_rf, b_p_glm, b_p_qda,    # Note, kNN results for k = 87 are excluded.
                    b_p_lda, b_p_loess, b_p_knn, b_p_rf, p_km)
c_vote <- sapply(1:nrow(testX), function(i){
  ifelse(mean(c_ensemble[i,] == 1) >= 0.5, 1, 2)
})
c_acc_ens <- mean(c_vote == testY)      # Accuracy of complete ensemble predictions.
c_sen_ens <- mean(c_vote[ldi] == 1)     # Sensitivity of complete ensemble predictions.
c_spe_ens <- mean(c_vote[ndi] == 2)     # Specificity of complete ensemble predictions.

##### Results #####
# Display all proportions as percentages:
results <- data.frame(Method = c("k-means", "Logistic Regression (Bootstrap)", "LDA (Bootstrap)", 
                                 "QDA (Bootstrap)", "LOESS (Bootstrap)", "kNN (Bootstrap)", 
                                 "Random Forest (Bootstrap)", "Logistic Regression (K-fold)", 
                                 "LDA (K-fold)", "QDA (K-fold)", "LOESS (K-fold)", "kNN (K-fold)", 
                                 "Random Forest (K-fold)", "Ensemble (Bootstrap)", "Ensemble (K-fold)", 
                                 "Ensemble (Complete)", "kNN (k = 87)"
                                 ),
                      Accuracy = round(100*c(acc_km, b_acc_glm, b_acc_lda, b_acc_qda, b_acc_loess,
                                             b_acc_knn, b_acc_rf, k_acc_glm, k_acc_lda, k_acc_qda, 
                                             k_acc_loess, k_acc_knn, k_acc_rf, b_acc_ens, k_acc_ens, 
                                             c_acc_ens, b_acc_knn_87), 
                                       2),
                      Sensitivity = round(100*c(sen_km, b_sen_glm, b_sen_lda, b_sen_qda, b_sen_loess,
                                                b_sen_knn, b_sen_rf, k_sen_glm, k_sen_lda, k_sen_qda, 
                                                k_sen_loess, k_sen_knn, k_sen_rf, b_sen_ens, k_sen_ens, 
                                                c_sen_ens, b_sen_knn_87), 
                                          2),
                      Specificity = round(100*c(spe_km, b_spe_glm, b_spe_lda, b_spe_qda, b_spe_loess,
                                                b_spe_knn, b_spe_rf, k_spe_glm, k_spe_lda, k_spe_qda, 
                                                k_spe_loess, k_spe_knn, k_spe_rf, b_spe_ens, k_spe_ens, 
                                                c_spe_ens, b_spe_knn_87), 
                                          2),
                      Balance = round(100*c((spe_km + sen_km)/2, (b_spe_glm + b_sen_glm)/2, (b_spe_lda + b_sen_lda)/2,
                                            (b_spe_qda + b_sen_qda)/2, (b_spe_loess + b_sen_loess)/2, (b_spe_knn + b_sen_knn)/2,
                                            (b_spe_rf + b_sen_rf)/2, (k_spe_glm + k_sen_glm)/2, (k_spe_lda + k_sen_lda)/2, 
                                            (k_spe_qda + k_sen_qda)/2, (k_spe_loess + k_sen_loess)/2, (k_spe_knn + k_sen_knn)/2, 
                                            (k_spe_rf + k_sen_rf)/2, (b_spe_ens + b_sen_ens)/2, (k_spe_ens + k_sen_ens)/2, 
                                            (c_spe_ens + c_sen_ens)/2, (b_spe_knn_87 + b_sen_knn_87)/2), 
                                      2)
                      )

# Final table:
kable(results, caption = "Model Results")