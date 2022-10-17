#------------------------------------------------PART 1 -------------------------------------------------------#

library(tidyverse)
library(tidymodels) 
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(GGally)
library(repr) 
library(psych)
library(naniar)
library(ggcorrplot)
library(caTools)
#------------------------------------------------PART 2 --------------------------------------------------------#


# READING DATASET AND VIEWING

df <- read.csv("D:/Advanced Statistics/Project/heart_cleveland_upload.csv")
df

View(df)

str(df)

describe(df)

#-------------------------------------------------PART 3 ------------------------------------------------------#


#  CHECKING FOR MISSING VALUES
df  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col()

#-------------------------------------------------PART 4 -----------------------------------------------------#


# Plotting the correlation of the variables minus the outcome variable

ggcorr(df[,-14], name = "corr", label = TRUE)+
  
  theme(legend.position="none")+
  
  labs(title="Correlation Plot")+
  
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


# INFERENCE FROM CORRELATION PLOT
      # From Above, we can see that the pair oldpeak and slope are the only ones that have a correlation
      # of above 50% i.e 60%. The rest have correlation of below 50%.


#-------------------------------------------------PART 5 ------------------------------------------------------#


# Making afew changes to some of the variables that seem categorical in  nature for the purpose of visualization
# Here we change the numerical value i,e 0 or 1 to naming variable or to their name 
# for example in sex 
#sex column 
df$sex[df$sex == 0] = "female"
df$sex[df$sex == 1] = "male"

#cp 
df$cp[df$cp==0] = "typical angina"
df$cp[df$cp==1] = "atypical angina"
df$cp[df$cp==2] = "non-anginal pain"
df$cp[df$cp==3] = "asymptomatic"

#fbs
df$fbs[df$fbs==0] = "false"
df$fbs[df$fbs==1] = "true"

#exang
df$exang[df$exang == 1] = "yes"
df$exang[df$exang == 0] = "no"

#restecg
df$restecg[df$restecg==0] = "Normal"
df$restecg[df$restecg==1] = "ST-T Wave abnormality"
df$restecg[df$restecg==2] = " Probable or definite left ventricular hypertrophy"

#slope
df$slope[df$slope == 0] = "upsloping"
df$slope[df$slope == 1] = "flat"
df$slope[df$slope == 2] = "downsloping"

#thal
df$thal[df$thal == 0] = "normal"
df$thal[df$thal == 1] = "fixed defect"
df$thal[df$thal == 2] = "reversible defect"

#condition
df$condition[df$condition == 0] = 'No disease'
df$condition[df$condition == 1] = 'Has disease'



converted_df <- head(df, n = 6)
View(converted_df)


#-------------------------------------------------PART 6 ------------------------------------------------------#


# CONTIGENCY ANALYSIS / CHI-SQUARE TEST OF INDEPENDENCE
# creating a 2-way contingency table( between the predictive variables & the outcome variable, condition)

# A hypothesis test called contingency analysis is used to determine whether two categorical variables
# are independent or not. To put it another way, we're asking, "Can we foretell the value of one variable 
# if we know the value of the other?" If the answer is true, we can conclude that the variables under examination 
# are not independent. If the response is no, the variables under investigation are said to be independent.
#The exam is called as 'Contingency Analysis' since it makes use of contingency tables. 
# Because the test statistic follows a chi-square distribution, it's also known as the 'Chi-square test of independence.'
# The test used to determine if two categorical variables are independent or not. The test's null hypothesis states 
# that the two variables are independent, whereas the alternative hypothesis states that they are not.

# We are going to use the built-in chisq.test() function to perform Chi-square test of independence. 
# Firstly, we create a 2-way contingency table.

table_1 <- table(df$sex, df$condition)
table_1
chisq.test(table_1)

# cp & condition
table_2 <- table(df$cp, df$condition)
table_2
chisq.test(table_2)

# fbs & condition
table_3 <- table(df$fbs, df$condition)
table_3
chisq.test(table_3)

# restecg & condition
table_4 <- table(df$restecg, df$condition)
table_4
chisq.test(table_4)

# exang & condition
table_5 <- table(df$exang, df$condition)
table_5
chisq.test(table_5)

# slope & condition
table_6 <- table(df$slope, df$condition)
table_6
chisq.test(table_6)

# ca & condition
table_7 <- table(df$ca, df$condition)
table_7
chisq.test(table_7)

# thal & condition
table_8 <- table(df$thal, df$condition)
table_8
chisq.test(table_8)

# Interpretations from the above contingency Analysis

   #1.  From the above result, we can see that p-value is less than the significance level (0.05) for all the tables
       #except table 3. Thus, we can reject the null hypothesis and conclude that the two variables (sex & condition
       #(i.e having the disease or not)) are not independent that is there exists an association between the 2 variables.
       #The same can be said with the variables chest pain, resting electrocardiogram, exercise induced angina, the slope 
       #of the peak exercise ST segment, ca and thal all having a p-value of less than 0.05 hence having an association 
       #with the outcome variable condition.

    #2. We can see a warning present itself that is, "Chi-squared approximation may be incorrect", when tried to
        #determine whether the two variable fasting blood sugar and condition (table_3) are independent or 
        #not. chisq.test function throws the above warning whenever one of the expected counts is lower than 5. 
        #In this case, we will add the option “correct = FALSE” as the second argument in the chisq.test() function 
        #to tell R to not do a Yate’s correction, which can be overly conservative. Let us see what the output will be
        #when we make this change.


chisq.test(table_3, correct=FALSE)

# Interpretation from above
    #The previous warning is no longer present and we can see that the p-value is greater than the
    #level of significance 0.05, thus we do not reject the null hypothesis that states that the two 
    #variables are independent that is there is no association between fasting blood sugar and the outcome 
    #variable condition.



#-----------------------------------------------------PART 7 ----------------------------------------------------------#


# converting categorical variable variables from character datatype  to factor datatype

cols <- c('sex', 'cp', 'fbs', 'restecg', 'exang', 'slope', 'ca', 'thal', 'condition')

for (col in cols) {
  df[, col] <- as.factor(df[, col])
}

str(df)


#----------------------------------------------------PART 8 -----------------------------------------------------------#

                  #-----------------------------PLOT 1 ----------------------------------#

# We will start to visualize & explore categorical variables

df %>% group_by(condition) %>% summarise(n = n()) %>% ggplot(aes(condition, n, fill = condition)) + geom_col() + 
  geom_text(aes(label = n), position = position_dodge(width = 1), vjust = 0.25) + theme_economist()

# Interpretations
    # Based on the plot above, it is clear that the dataset is NOT experiencing imbalanced classification 
    # which is a supervised learning problem where one class outnumbers other class by a large proportion. 
    # This problem is faced more frequently in binary classification problems than multi-level classification problems. 
    # The term imbalanced refer to the disparity encountered in the dependent (response) variable (in our case the
    # response variable, condition). Thus, an imbalanced classification problem is one in which the response
    # variable has imbalanced proportion of classes. In other words, a data set that exhibits an unequal 
    # distribution between its classes is considered to be imbalanced.



                #-----------------------------PLOT 2 -----------------------------------#


df %>% group_by(fbs, condition) %>% summarise(n = n()) %>% ggplot(aes(x = reorder(fbs, -n), y = n, fill = condition)) +
  geom_col(position = 'dodge') + geom_label_repel(aes(label = n)) + labs(x = "fasting blood sugar") + theme_economist()

df %>% group_by(restecg, condition) %>% summarise(n = n()) %>% ggplot(aes(x = reorder(restecg, -n), y = n, fill =  condition)) + 
  geom_col(position = 'dodge') + geom_label_repel(aes(label = n)) + labs(x = "resting electrocardiographic results")  + theme_economist() 

df %>% group_by(exang, condition) %>% summarise(n = n()) %>% ggplot(aes(x = reorder(exang, -n), y = n, fill =  condition))+
  geom_col(position = 'dodge') + geom_label_repel(aes(label = n)) + labs(x = "exercise induced angina") + theme_economist()

df %>% group_by(slope, condition) %>% summarise(n = n()) %>% ggplot(aes(x = reorder(slope, -n), y = n, fill =  condition)) +
  geom_col(position = 'dodge') + geom_label_repel(aes(label = n)) + labs(x = "the slope of the peak exercise ST segment") +
  theme_economist()

df %>% group_by(ca, condition) %>% summarise(n = n()) %>% ggplot(aes(x = reorder(ca, -n), y = n, fill =  condition)) +
  geom_col(position = 'dodge') + geom_label_repel(aes(label = n)) + 
  labs(x = "number of major vessels (0-3) colored by flourosopy") + theme_economist()

df %>% group_by(thal, condition) %>% summarise(n = n()) %>% ggplot(aes(x = reorder(thal, -n), y = n, fill =  condition)) +
  geom_col(position = 'dodge') + geom_label_repel(aes(label = n)) + labs(x = "thalium stress result") + theme_economist()


df %>% group_by(sex, condition) %>% summarise(n = n()) %>% ggplot(aes(x = reorder(sex, -n), y = n, fill = condition)) + 
  geom_col(position = 'dodge') + geom_label_repel(aes(label = n)) + labs(x = "Gender") + theme_economist()


# Interpretation of the visualizations above

    # There are less people who have heart disease in the dataset.
    # From the looks of it, there are less people who have fasting blood sugar > 120 mg/dL
    # i.e(majority is false) than those who have fasting blood sugar > 120 mg/dL (likely to be diabetic).
    # People who are showing probable or definite left ventricular hypertrophy by Estes' criteria as their
    # resting electrocardiogram results take the lead when it comes to having heart disease.
    # Followed by the category of normal resting electrocardiogram results. Then those having ST-T wave 
    # abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV) having 3 individuals
    # with the disease. etc


      #-------------------------------------- PLOT 3 ---------------------------------------------#

# visualizing and exploring the numeric data

#visualizing the numerical variables 
#Histogram with a density plot


df %>% 
  keep(is.numeric) %>%  # keeping only numerical column
  gather() %>%          # Convert to key-value pair
  ggplot(aes(value)) +    # plot the values
  facet_wrap(~key, scale = "free") + # in separate panel
  geom_histogram(aes(y = ..density..), color = "darkblue", fill = "lightblue") +
  geom_density(alpha = 0.2) + theme_economist()


     #------------------------------------ PLOT 4 -----------------------------------------------#


#boxplot


df %>% 
  keep(is.numeric) %>%  # keeping only numerical column
  gather() %>%          # Convert to key-value pair
  ggplot(aes(value)) +    # plot the values
  facet_wrap(~key, scale = "free") + # in separate panel
  geom_boxplot() + theme_economist()


    #-----------------------------------------PLOT 5 -----------------------------------------------#


gg <- ggplot(df, aes(x=df$chol, y=df$trestbps)) + 
  geom_point(aes(col=df$condition))
gg


   #----------------------------------------------PLOT 6 -------------------------------------------#

databox=data.frame(df$age, df$trestbps,df$chol,df$thalach)
boxplot(databox)


    #----------------------------------------------PLOT 7 ----------------------------------------#


gg1 <- ggplot(df, aes(x=df$chol, y=df$trestbps)) + 
  geom_point(aes(col=df$condition, size=df$oldpeak)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(100, 430)) + 
  ylim(c(75, 200)) + 
  labs(subtitle="trestbps Vs chol", 
       y="trestbps", 
       x="chol", 
       title="Scatterplot", 
       caption = "Source: midwest", 
       bins = 30)
plot(gg1)


#-----------------------------------------------LOGISTIC REGRESSION--------------------------------------------------#

      #--------------------------------SPLITTING THE DATA--------------------------------------------------#
set.seed(123)
split=sample.split(df$condition, SplitRatio = 0.75)
split

qualityTrain=subset(df,split == TRUE)
qualityTest=subset(df,split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)

# 223 data are used to train
# 74 datas are used to test


      #-----------------------------BUILDING THE MODEL----------------------------------------------------#


#The dependent variable used iscondition, for the independent variable is age, trestbps, chol, fbs, 
#restecg, thalach, exang, oldpeak, slope, ca, and thal.

#logistic regression model

datasetlog=glm(df$condition ~ df$condition+df$age+df$trestbps+df$chol+df$fbs+df$restecg+df$thalach+df$exang+df$oldpeak+df$slope+df$ca+df$thal,data=qualityTrain,family = binomial)
summary(datasetlog)

# A lot of variables are not significant.
# Now we will removing Variables based on Significance Level using the backward method

datasetlog2=glm(df$condition ~ df$age+df$sex+df$cp+df$trestbps+df$chol+df$fbs+df$thalach+df$exang+df$oldpeak+df$slope+df$ca+df$thal,data=qualityTrain,family = binomial)
summary(datasetlog2)

datasetlog3=glm(df$condition ~ df$age+df$sex+df$cp+df$chol+df$fbs+df$thalach+df$exang+df$oldpeak+df$slope+df$ca+df$thal,data=qualityTrain,family = binomial)
summary(datasetlog3)

datasetlog4=glm(df$condition ~ df$age+df$sex+df$cp+df$chol+df$fbs+df$thalach+df$exang+df$oldpeak+df$ca+df$thal,data=qualityTrain,family = binomial)
summary(datasetlog4)

datasetlog5=glm(df$condition ~ df$age+df$sex+df$cp+df$chol+df$thalach+df$exang+df$oldpeak+df$ca+df$thal,data=qualityTrain,family = binomial)
summary(datasetlog5)


datasetlog6=glm(df$condition ~ df$age+df$sex+df$cp+df$chol+df$thalach+df$oldpeak+df$ca+df$thal,data=qualityTrain,family = binomial)
summary(datasetlog6)


datasetlog7=glm(df$condition ~ df$sex+df$cp+df$trestbps+df$chol+df$thalach+df$oldpeak+df$ca+df$thal,data=qualityTrain,family = binomial)
summary(datasetlog7)

# Applying Model after removing least significant Variables

# A general rule in machine learning is that the more features you have, the more likely your model 
#will suffer from overfitting

#Making predictions on training sets using datasetlog7

#presiksi data aktual menggunakan model datasetlog7

predictTrain=predict(datasetlog7,type="response")
predictTrain

#penentuan thresold


#---------------------------------Plotting ROCR curve----------------------------------------------------------#

qualityTrain
library(ROCR)
ROCRpred <- prediction(predictTrain, df$condition)
ROCRperf<- performance(ROCRpred,'tpr','fpr')

plot(ROCRperf)
plot(ROCRperf,colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1),
     text.adj=c(-0.2,1.7))


#From ROCR curve threshold of 0.7 seems to be okay so that true positives are maximised such that 
#maximum number of patients with heart disease are not identified as healthy.

#then we can see the value of AUC. Higher the AUC, better the model is at distinguishing between
#patients with disease and no disease.


#-----------------------------Area under the curve-----------------------------------------------------------#


auc = as.numeric(performance(ROCRpred, 'auc')@y.values)
auc

# AUC value is 0.92 that means our model is able to distinguish between patients with the disease
#and no disease with a probability of 0.92. So it is a good value.


#--------------------------------Accuracy------------------------------------------------------------------#

#Accuracy using a threshold of 0.7
predictTest=predict(datasetlog7, newdata = qualityTest,type = "response")
table(df$condition,predictTest >=0.7)

#accuracy
(39+13)/74

#Logistic regression model with all the variables and logistic regression model after removing less significant 
#attributes performed best with an accuracy of testing 70%


#----------------------------------BUILDING RANDOM FOREST MODEL --------------------------------------#


#Coverting the outcome variable to a factor
# Using the duplicate dataframe called heart
df[, 'condition'] <- as.factor(df[, 'condition'])
class(df$condition)

#Set seed for reproducibility
set.seed(9876)


# Creating the balanced data split
heart_split <- initial_split(df, prop = 0.70, strata = condition)

# Extracting the training and test set
heart_train <- training(heart_split)
heart_test  <- testing(heart_split)


#Specifying a random forest
RF_spec <- rand_forest(mtry = 4, trees = tune(), min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine('ranger', importance = "impurity")




# Creating the tuning grid
tunegrid_RF <- grid_regular(parameters(RF_spec), 
                            levels = 12)

tunegrid_RF


options(repr.plot.width = 10, repr.plot.height = 10)

# enabling parallel processing on 6 threads to speed up tuning
library(doParallel)
doParallel::registerDoParallel(cores = 6)

# Setting seed for reproducibility
set.seed(2000)

# Creating CV folds of training data
Folds <- vfold_cv(heart_train, v = 6)

# Tuning along the grid
library(ranger)
Tune_results <- tune_grid(RF_spec,
                          condition ~ .,
                          resamples = Folds,
                          grid = tunegrid_RF,
                          metrics = metric_set(roc_auc))
# Checking the result of tuning hyperparameters
Tune_results %>% collect_metrics() %>%
  head(n= 20)



# Ploting the results
autoplot(Tune_results)


#Once this computation is complete (which can take a while even with parallel processing 
#enabled via registerDoParallel()), we can look at the best performing hyperparameters 
#w.r.t. a metric we specified earlier i.e roc_auc.

# Showing the best combination of hyperparameters 
show_best_params <- show_best(Tune_results, n =1)

show_best_params
# Selecting the final hyperparameters
Best_params <- select_best(Tune_results)

Best_params


# Finalizing the specification
Final_spec <- finalize_model(RF_spec, Best_params)

Final_spec


# Training the final model on the full training data
final_RF_model <- Final_spec %>% fit(condition ~ ., heart_train)
final_RF_model


# Set seed for reproducibility
set.seed(51)

# Predicting on test set and add to training set
library(janitor)
Predictions_1 <- predict(final_RF_model,
                         new_data = heart_test, 
                         type = "prob") %>%  janitor::clean_names() %>%
  mutate(true_class = heart_test$condition)



# Creating and plot the ROC curve
roc_curve(Predictions_1,
          estimate = pred_has_disease,
          truth = true_class) %>% autoplot()


# Calculating the AUC
roc_auc(Predictions_1,
        estimate = pred_has_disease, 
        truth = true_class)


#-------------------------------Confusion Matrix------------------------------------------#


#Predicting outcome categories and then Combining with the test so as to construct the confusion matrix

prediction_2 <- predict(final_RF_model,
                        new_data = heart_test, 
                        type = "class") %>%  janitor::clean_names() %>%
  mutate(true_class = heart_test$condition)


conf_mat(prediction_2,
         truth = true_class,
         estimate = pred_class) %>% 
  # Create a heat map
  autoplot(type = 'heatmap')

#----------------------------Checking other metrics-----------------------------------------#

# Using the confusion matrix to view other metrics like precision, recall etc by passing the summary() function
metrics <- conf_mat(prediction_2,
                    truth = true_class,
                    estimate = pred_class) %>% 
  # Passing to the summary() function
  summary()

metrics

#--------------------------Variable importance--------------------------------------------#

# Plot the variable importance
library(vip)
vip::vip(final_RF_model, aesthetics = list(color = 'blue'))

