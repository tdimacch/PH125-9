#' ---
#' title: \textcolor{orange}{M9 - Capstone - Voting Party Predictor and Influencer Identification}
#' author: "Tim Dimacchia"
#' date: "`r Sys.Date()` "
#' output:
#'   pdf_document: 
#'      toc: true
#'      toc_depth: 3
#'      number_sections: true
#'      fig_width: 6
#'      fig_height: 4
#'      extra_dependencies: ["float"]
#'      fig_caption: true
#'   word_document: default
#' header-includes:
#'   - \usepackage{pdfpages}
#'   - \usepackage{xcolor}
#'   - \usepackage{hyperref}
#'   - \hypersetup{colorlinks=true, urlcolor=blue, linkcolor=blue}
#' ---
#' 
Sys.Date()
## ----setup_knitr, include=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, background="red")
options(knitr.table.format= "latex")
knitr::opts_knit$set(eval.after = "fig.cap")
#knitr::opts_chunk$set(background = c(.1, 0, 0))

# knitr::opts_chunk$set( echo = TRUE, message=FALSE, warning=FALSE, fig.width=8 )
#knitr::purl("PH125-9-ElectionProject.Rmd", output="PH125-9-ElectionProject.R", documentation = 2)

#' 
#' 
#' \newpage
#' 
#' # Preface - About the Author
#'             Author: Tim DiMacchia
#'               Date: September 20, 2020
#'              Email: tdimacch@mac.com
#'           [LinkedIn:](www.linkedin.com/in/timdimacchia)
#'           [More About The Author - click for URL](https://docs.google.com/presentation/d/1q9buIIOHkAzYgvzcKOwQIxNMoPGS7wWAmmpHDLDgDxo/edit?usp=sharing)
#'   
#' # Project Background  
#' The following project is required as part of the HarvardX (edX) PH125.9 Data Science: Capstone course.  It is required to complete the 9 module series to receive a Professional Certificate in DataScience. 
#' 
#' As part of this project, we were strongly discouraged from using well-known datasets, particularly ones that have been used as examples in previous courses or are similar to them (such as the iris, titanic, mnist, or movielens datasets, among others).  Providing an opoportunity to learn and use new datasets.
#' 
#' Recommended data source locations were the UCI Machine Learning Repository and Kaggle. The required dataset must be automatically downloaded in your code or included with your submission.
#' 
#' The foundation for this project came from the Kaggle competion - "Can we predict voting outcomes?"
#' This competition may be found at: [Kaggle Competition - Predicting Election Results](www.kaggle.com/c/can-we-predict-voting-outcomes/data).  Project competition was only open to students of [15.07x - The Analytics Edge](https://www.edx.org/course/analytics-edge-mitx-15-071x-2)
#' 
#' Data for this project was provided by Show of Hands, an informal voting polling platform for use on mobile devices and the web.  Show of Hands specilizes in seeing what aspects and characteristics of people's lives predict how they will be voting for presidential elections. 
#' 
#' Show of Hands has been downloaded over 300,000 times across Apple and Android app stores, and users have cast more than 75 million votes. 
#' 
#' Other project ideas that were considered were: 
#' 
#' - Anayizing Rider Share data for a particular city (San Francisco, Reno, Las Vegas, etc.)
#' - Job Offer Salary Prediction for a canidate
#' - City Housing Prices (California, Reno, etc.)
#' - Olympic Race Walker Result prediction 
#' - Predicting Fake News
#' - Predicting Car Prices
#' 
#' I have choosen this project, because we are approximately 50 days from our next presidential election and building a predictor was considered a 'fun' exercise to compare against the actual results. 
#' 
#' The following appendixes have been created: 
#' 
#' - Appendix-A   Package Installations 
#' - Appendix-B   Dataset Inspections
#' - Appendix-C   Demographic Charts
#' - Appendix-D   Correlation Matrixes and Heat Maps
#' - Appendix-E   References
#' - Appendix-F   Peer Assignment Grading Requirements
#' - Appendix-G   Survey Questions
#' - Appendix-H   List of Tables
#' - Appendix-J   List of Figures
#' 
#' \newpage
#' # Project Goal 
#' The goal of this project is to apply machine learning techniques that go beyond standard linear regression modeling using a publicly available dataset of our choice.   As mentioned in the Project Background section, we will be using the election survey results provided by Show of Hands which consists of thousands of users and one hundred different questions to see which responses predict voting outcomes.
#' 
#' We have created two goals for this project which are modifications of the Kaggle compeition goal: 
#' 
#' - Goal #1:  Predict which candidate platform will win the election
#' - Goal #2:  Identify which questions have the largest influence on Goal #1.
#' 
#' Special Note:  This class is considered to be an **introductory class** in both Machine Language and the programming language R.  Conversly the Kaggle competition was for an **advanced class**.  
#' 
#' 
#' # Project Grading
#' Appendix - F (Peer Assignment Grading Requirements) contains the requirements and point allocation for each requirements of this project. 
#' 
#' Project submission must include: 
#' 
#' - A report in the form of a PDF document 
#' - The resulting Rmd file 
#' - The R source code / script that performs the machine learning task. 
#' 
#' Addiitionally, access to the datasete must also be made either through automatic download or inclusion in a GitHub repository. 
#' 
#' We will be providing our datasets via project submission attachment in the form of the .zip file. 
#' * Note: The user must expand this .zip file into their current working directory.*
#' 
#' \newpage
#' 
#' 
## ----global_options, warning = FALSE, message = FALSE,  echo = FALSE---------------------------------------------------------------------------------------------------------------------------------
# This secion defines global environment options
options(digits = 5)                            # limiting significant digits to 3
options(knitr.purl.inline = TRUE)              # Include R inline expressions
# setwd("/Users/Tim/Dropbox/R-Programming")      # Set workind directory
require(ggplot2)                               # Resolves some conflicts with packages and ggplot2
options(tidyverse.quiet = TRUE)                # Magic Option to surpress warnings
options(dplyr.summarise.inform = FALSE)        # Supress warnins from summarize() w/dplyr
# version                                       # version.string R version 3.6.3 (2020-02-29)
#tinytex::install_tinytex()                    # Run this line if .pdf are not created by knitr()
# Sys.which('pdflatex')                         # Make sure this path isn't empty.  Empty = no pdf.
options(datatable.showProgress = FALSE)        # disable progress bars for final output

project_results <- data.frame()         # Empty data frame... reduces code validatons and checks later. 
winning_model <- data.frame()           # Empty data frame for printing at the end.
test_confusion <- data.frame()          # Empty data frame for printing each confusion matrix result

train_file <- "./ElectionData/train2016.csv"
test_file <- "./ElectionData/test2016.csv"
questions_file <- "./ElectionData/Questions.csv"
questions_graphic_1 <- "./ElectionData/Questions-page1.png"
questions_graphic_2 <- "./ElectionData/Questions-page2.png"
model_comparison <- "./ElectionData/Model_Comparison.png"
machine_learning_algo <- "./ElectionData/MachineLearningAlgorithms.pdf"


#' 
## ----installing_project_packages, warning = FALSE, message = FALSE,  echo = FALSE--------------------------------------------------------------------------------------------------------------------
func_install_project_packages  <- function() {

#What pacman does it it checks if the required packages are already installed and if not, install them automatically

if (!require("pacman")) install.packages("pacman")
  pacman::p_load(data.table,
  'tidyverse',
  'caTools',
  'knitr',
  'forcats',
  'boot',
  'randomForest',
  'scales',
  'kableExtra',
  'caret',
  'data.table',
  'lubridate',
  'stringr',
  'recosystem',
  'tinytex',
  'DescTools',
  'ggplot2',
  'pROC',
  'party',
  'rpart',
  'rpart.plot',
  'MASS',
  'ggthemes',
  'class',
  'dplyr',
  'corrplot',
  'PerformanceAnalytics',
  'e1071',
  'Boruta',
  "glmnet",
  'naivebayes')
  
}

#' 
#' 
## ----installing_librarys, warning = FALSE, message = FALSE,  echo = FALSE----------------------------------------------------------------------------------------------------------------------------
# Function specializing in library installations.  

func_installing_librarys <- function() {
library(tidyverse)
library(forcats)
library(boot)
library(caTools)
library(randomForest)
library(kableExtra)
library(scales)
library(caret)
library(data.table)
library(lubridate)
library(stringr)
library(recosystem)
library(tinytex)
library(party)
library(DescTools)
library(ggplot2)
library(pROC)
library(rpart)
library(rpart.plot)
library(caTools)
library(ggthemes)
library(MASS)
library(class)
library(naivebayes)
library(caTools)
library(corrplot)
library(PerformanceAnalytics)
library(e1071)
library(Boruta)
library(glmnet)
library(dplyr, warn.conflicts = FALSE)                 #Suppress warnings from summarize()
}


#' 
## ----func_update_print_results, echo = FALSE, message = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------------------------
# Customized funtion for adding the results from each model into the project results table.
# Ths function also identifies the Top 2 performing moodel predictions and the worst. 
#     Green = Best performing 
#     Light Green is the runner-up (or 2nd best performing model)
#     Red is for the  model with the worst results. 

func_update_print_results <- function(test_cm, model_name) {
  
   confusion_class_results <- as.matrix(test_cm, what = "classes")
   confusion_overall_results <- as.matrix(test_cm, what = "overall")
   confustion_positive_results <- test_cm$positive              # who is projected to win

   confusion_class_results[1]   #Sensitivity  
   confusion_class_results[2]   #Specificity  
   confusion_class_results[5]   #Precision    
   confusion_overall_results[1] #Accuracy     
   
   confusion_positive_results <- test_cm$positive
   
   
   project_results <<- bind_rows(project_results, data_frame(ModelType = model_name, 
              Accuracy = confusion_overall_results[1], 
              Precision = confusion_class_results[5], 
              Sensitivity = confusion_class_results[1], 
              Specifity = confusion_class_results[1],
              Winner = confusion_positive_results)) 
 
}     # end function_update_print_results



#' 
#' 
## ----func_create_print_results, figures-side, fig.show="hold", out.with="100", echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------

func_create_print_results <- function(test_cm, model_name, display_results) {
 #Prints the final result table and highlights the top performer, runner up performer and worst performer.
   total_results <- nrow(project_results)
   ordered_results <-sort(project_results$Accuracy, decreasing=FALSE)
   sorted_results <- sort(project_results$Accuracy, decreasing=TRUE)
   index_winner_value <- which(project_results$Accuracy ==  sorted_results[1])
   index_loser_value <- which(project_results$Accuracy ==  ordered_results[1])
   index_runnerup_value <-  which(project_results$Accuracy ==  sorted_results[2])
 
   model_name_caption <- paste(sprintf("Prediction Results: %s Model - Added", model_name))
   
   if  (display_results & total_results == 1) { 
     project_results %>%
     knitr::kable(caption = model_name_caption) %>% 
          kable_paper("striped", position = "center") %>% 
          kable_styling(latex_options = "hold_position")  %>%
          row_spec(row=0, bold=T) %>%
          row_spec(index_winner_value: index_winner_value, bold = F, color = "black", background = "green") 
   } else if (display_results & total_results ==2) {  
      project_results %>%
        knitr::kable(caption = model_name_caption) %>% 
            kable_paper("striped", position = "center") %>% 
            kable_styling(latex_options = "hold_position")  %>%
            row_spec(row=0, bold=T) %>%
            row_spec(index_winner_value: index_winner_value, bold = F, color = "black", background = "green") %>%
            row_spec(index_loser_value: index_loser_value, bold = F, color = "black", background = "#ffa07a") 
   } else if (display_results) {
      project_results %>%
        knitr::kable(caption = model_name_caption) %>% 
            kable_paper("striped", position = "center") %>% 
            kable_styling(latex_options = "hold_position")  %>%
            row_spec(row=0, bold=T) %>%
            row_spec(index_winner_value: index_winner_value, bold = F, color = "black", background = "green") %>%
            row_spec(index_runnerup_value: index_runnerup_value, bold = F, color = "black", background = "#90ee90") %>% 
            row_spec(index_loser_value: index_loser_value, bold = F, color = "black", background = "#ffa07a")
   }

     
}     # end function_create_results


#' 
#' 
#' 
## ----func_draw_confusion_matrix, figures-side, fig.show="hold", out.with="100", echo = FALSE, message = FALSE, warning = FALSE-----------------------------------------------------------------------
# Function to create a standardized table/chart of Model results created via the confusionMatrix function.
func_draw_confusion_matrix <- function(cm, plot_data, model_name) {

  label_class1 <- "Democrat"
  label_class2 <- "Republican"
  label_x <- "Voting Party"
  label_y <- "Totals"
  title_string = paste("CONFUSION MATRIX for:",model_name)
 
 # title_string = paste("Predicting Outcome for:", model_name)
 #  plot(plot_data, main= title_string, col='lemonchiffon3', xlab=label_x, ylab=label_y)
  
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,6,2,0))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(title_string, cex.main=1.5)

  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, label_class1, cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, label_class2, cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, label_class1, cex=1.2, srt=90)
  text(140, 335, label_class2, cex=1.2, srt=90)

  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')

  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 5), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 5), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 5), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 5), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 5), cex=1.2)

  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 5), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 5), cex=1.4)
  
}  


#' 
## ----func_alpha_n_bootstrap, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------
# Used to Bootstrap analysis.
func_alpha.fn = function(data, index) {
  
  X <- as.numeric(data$Party[index])
  Y <- as.numeric(data$Gender[index])
  
  result <- (var(Y)-cov(X,Y))/(var(X)+var(Y) -2*cov(X,Y))
 
  return (result)   # stored result in separate variable for redability and debugging
}


#' 
#' 
#' 
## ----func_boot.fn, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Used for Bootstrapping prediction
func_boot.fn = function (data ,index)
return(coef( lm(Party ~ ., data=data, subset=index)))

#' 
#' 
#' 
## ----project_clean_up, warning = FALSE, message = FALSE,  echo = FALSE-------------------------------------------------------------------------------------------------------------------------------
# cleanup, great for clean runs and also clean up at the end.

func_project_clean_up <- function() {
 rm(list = ls(all.names = TRUE))    #clearing all objects including the hiddent objects
 gc()                               # free up memory and report the memory usage
 rm(project_results)                # from reason, it doesn't seem to always get removed. 
}


#' 
#' 
#' 
## ----project_start_up, warning = FALSE, message = FALSE,  echo = FALSE-------------------------------------------------------------------------------------------------------------------------------
# makes it easier to install packages and libraries especially after a restart
func_install_project_packages()
func_installing_librarys()

#' 
#' 
#' 
#' \newpage
#' 
#' # Dataset Acquisition - The Original Datasets
#' 
#' For this project, 3 files were provided as part of the Kaggle competition.  Those files are: 
#' 
#' - Train.csv          : Contains the dataset to be used for training the Machine Learning Model
#' - Test.csv           : Contains the dataset to test the Machine Learning Model
#' - Questions.csv      : The questions which were given to the participants. 
#' 
#' 
#' Before proceding we will inspect the datasets to ensure all data has been properly wrangled. 
#' 
#' Data Fields for the Train Dataset: 
#' 
#' - USER_ID            : an anonymous id unique to a given user
#' - YOB                : the year of birth of the user
#' - Gender             : the gender of the user, either Male or Female
#' - Income             : the household income of the user. Either not provided, or one of:
#' 
#'          - "under $25,000"
#'          - "$25,001 - $50,000"
#'          - "$50,000 - $74,999"
#'          - "$75,000 - $100,000"
#'          - "$100,001 - $150,000"
#'          - or "over $150,000"
#'          
#' - HouseholdStatus    : the household status of the user. Either not provided, or one of:
#' 
#'          - "Domestic Partners (no kids)"
#'          - "Domestic Partners (w/kids)"
#'          - "Married (no kids)"
#'          - "Married (w/kids)"
#'          - "Single (no kids)"
#'          - or "Single (w/kids)"
#' 
#' - EducationalLevel   : the education level of the user. Either not provided, or one of:
#' 
#'          - "Current K-12"
#'          - "High School Diploma"
#'          - "Current Undergraduate"
#'          - "Associate's Degree"
#'          - "Bachelor's Degree"
#'          - "Master's Degree"
#'          - or "Doctoral Degree".
#'          
#' - Party              : the political party for whom the user intends to vote for. Either "Democrat" or "Republican
#' - Q124742, Q124122, . . . , Q96024 
#'         101 different questions that the users were asked on Show of Hands. If the user didn't answer the 
#'         question, there is a blank. For information about the question text and possible answers, see the file Questions.pdf.
#' 
#' 
#' Data fields for the Test Dataset are the same as the Train Dataset excluding the Party field. 
#' 
## ----checking_required_files, warning = FALSE, message = FALSE,  echo = FALSE------------------------------------------------------------------------------------------------------------------------
func_checking_required_files <- function() {
  
# checks for all files before allowing the user to run the program. 
# if the files are installed in the correct directory, an error message is reported and the program exits
# The correct location is ./ElectionData/

    train_file <- "./ElectionData/train2016.csv"
    test_file <- "./ElectionData/test2016.csv"
    questions_file <- "./ElectionData/Questions.csv"
    questions_graphic_1 <- "./ElectionData/Questions-page1.png"
    questions_graphic_2 <- "./ElectionData/Questions-page2.png"
    model_comparison <- "./ElectionData/Model_Comparison.png"
    machine_learning_algo <- "./ElectionData/MachineLearningAlgorithms.pdf"
    Split8020 <<- "./ElectionData/SplitRatio8020.png"
    Split7030 <<- "./ElectionData/SplitRatio7030.png"
    Split6733 <<- "./ElectionData/SplitRatio6733.png"
    Split6040 <<- "./ElectionData/SplitRatio6040.png"
    Split5050 <<- "./ElectionData/SplitRatio5050.png"
    missing_file <- ""
    
    ZipFile   <- "./ProjectElection.zip"
    ZipOutputPath <- "./ElectionData"
 
    check_for_files <- c(train_file, test_file, questions_file, 
                         questions_graphic_1, questions_graphic_2, 
                         model_comparison, machine_learning_algo,
                         Split8020, Split7030, Split6733, Split6040, Split5050)

    total_files_to_check = length(check_for_files)

    if (!file.exists(ZipFile)) {
           print("Zip File not found")
           print("Please place the downloaded .zip file in your current working directory location")
           print(paste(sprintf("----- Your current working directory is: %s", getwd())) )
           stop("Program Execution Terminated because unable to read required files")
        } # end of conditional check activities 
  
   unzip(ZipFile, exdir=ZipOutputPath, overwrite=TRUE)      # Unzipping files to a diretory

  # Checking all files were unzip properly
   for(index in 1:total_files_to_check) {
        missing_file <- paste(sprintf("----- File Name: %s      ------- NOT FOUND", check_for_files[index]))

        if (!file.exists(check_for_files[index])) {
           print(missing_file)
           print("Please install the required files in their proper directory.  Per the instructions in the report file.")
           print("The program looks for files in the working directory with a subdirectory called /ElectionData.")
           print("    Example:  ~./Election/Data/")
           stop("Program Execution Terminated because unable to read required files")
        } # end of conditional check activities
        
   } # end of for loop
}  # End of checking for files    
  

#' 
#' 
## ----reading_datasets, warning = FALSE, message = FALSE,  echo = FALSE-------------------------------------------------------------------------------------------------------------------------------
# Kaggle compeetion files read into a a data.frame
# After reading in the data, we will extract those records that have an invalid YOB = NA, and outof bounds. 

func_checking_required_files()     # Check that all files are available before proceeding

kaggle_test <- read.csv("./ElectionData/test2016.csv")
kaggle_train <- read.csv("./ElectionData/train2016.csv")
kaggle_questions <- read.csv("./ElectionData/Questions.csv")

kaggle_test_copy <- kaggle_test         #backup copy ... just in case
kaggle_train_copy <- kaggle_train       #backup copy ... just in case


train <- read.csv("./ElectionData/train2016.csv", na.strings = c("","NA"))     # Wrangling for NA
test <- read.csv("./ElectionData/test2016.csv", na.strings = c("","NA"))       # Wrangling for NA
the_questons <- kaggle_questions

max_outlier_year <- max(train$YOB, na.rm=TRUE)     #can't have someone born 2039, it hasn't happened yet
min_outlier_year <- min(train$YOB, na.rm=TRUE)     # can't have someone born 1875 = 135 years old, doesn't align with world facts.

incomplete_YOB <- data.frame()                                 # needed to initialize to get this to work
new_df <- data.frame()                                         # needed to initialize to get this to work    
complete_YOB <- data.frame()                                   # needed to initialize to get this to work 
clean_train <- data.frame()                                    # initializeing to for corretness



# Tried to clean the data even further, but the results got worse vs. better.

#clean_train <- train[!(train$YOB == max_outlier_year | train$YOB == min_outlier_year), ]   # Remove records first - key
#clean_train <- clean_train[!is.na(clean_train$YOB),]                       # = 5232   removed NA's
#train <- clean_train                                                       # assign back to train


#' 
#' \newpage
#' 
#' ## Data Wrangling & Data Subset Creation
#' 
#' After reading the original files in, they were separated into the following:
#' 
#' - Kaggle_train                 : Original data read in from Train.csv
#' - Kaggle_test                  : Original data read in from Test.csv
#' - Kaggle_questions             : Original data read in from Questions.csv
#' - train_subset                 : Removing the questions from the Train dataset
#' - test_subset                  : Removing the questions from the Test dataset
#' - train_subset_questions       : Adding *key* questions back into the Train dataset
#' - test_subset_questions        : Adding *key* question back into the Test dataset
#' - train_sample                 : Machine Learning ratio split of dataset for training
#' - test_sample                  : Machine Learning ratio split of the dataset for testing
#' - train_sample_questions       : Train sample wih *key* questions added back in
#' - test_sample_questions        : Test sample with *key* questions added back in
#' - train_sample_no_na           : Removing NA's from the Train Sample
#' - train_sample_questions_no_na : Removing NA's from the Train Sample with *key* questions
#' - test_sample_no_na            : Removing NA's from the Test Sample
#' 
#' Worth noting, even though we checked for "NA" upon reading the training dataset, test dataset and questions,
#' we can see from Appendix B - Data Inspection summaries that some fields contain NA's within their vector.  
#' Hence we wrangled the datasets to remove NA's and created new datasets.
#' 
#' 
#' 
#' Later in this project we will be using the survey questions to tune our model predictions.   
#' While subjective, we have reduced the 100 questions down to a few.  Later we will reduce this list even further based upon their correlation influence. 
#' 
#' For now the *key* questions which were identified were: 
#' 
#' - 100010,Do you watch some amount of TV most days?,"Yes,No"
#' - 100562,Do you think your life will be better five years from now than it is today?,"Yes,No"
#' - 102674, Do you have any credit card debt that is more than one month old?,"Yes,No"
#' - 106042,Are you taking any prescription medications?,"Yes,No"
#' - 106388,Do you work 50+ hours per week?,"Yes,No"
#' - 108343,Do you feel like you have too much personal financial debt?,"Yes,No"
#' - 108617,Do you live in a single-parent household?,"Yes,No"
#' - 109244,Are you a feminist?,"Yes,No"
#' - 112512,Are you naturally skeptical?,"Yes,No"
#' - 113992,Do you gamble?,"Yes,No"
#' - 115899,Would you say most of the hardship in your life has been the result of circumstances beyond your own control
#' - 123464,Do you currently have a job that pays minimum wage?,"Yes,No"
#' - 123621,Are you currently employed in a full-time job?,"Yes,No"
#' 
#' 
## ----data_wrangling_subsetting, warning = FALSE, message = FALSE,  echo = FALSE----------------------------------------------------------------------------------------------------------------------

#  Dropping the quesiton fields for now

train_subset <- dplyr::select(train, USER_ID,YOB,Gender,Income,HouseholdStatus,EducationLevel,Party)
test_subset  <- dplyr::select(test, USER_ID,YOB,Gender,Income,HouseholdStatus,EducationLevel)


# Adding the following quesitons, which we are hypotheizing could have an inmpact. 
# Questions added are: 
#     100010,Do you watch some amount of TV most days?,"Yes,No"
#     100562,Do you think your life will be better five years from now than it is today?,"Yes,No"
#     102674, Do you have any credit card debt that is more than one month old?,"Yes,No"
#     106042,Are you taking any prescription medications?,"Yes,No"
#     106388,Do you work 50+ hours per week?,"Yes,No"
#     108343,Do you feel like you have too much personal financial debt?,"Yes,No"
#     108617,Do you live in a single-parent household?,"Yes,No"
#     109244,Are you a feminist?,"Yes,No"
#     112512,Are you naturally skeptical?,"Yes,No"
#     113992,Do you gamble?,"Yes,No"
#     115899,Would you say most of the hardship in your life has been the result of circumstances beyond your own control
#     123464,Do you currently have a job that pays minimum wage?,"Yes,No"
#     123621,Are you currently employed in a full-time job?,"Yes,No"


train_subset_questions <- dplyr::select(train, USER_ID,YOB,Gender,Income,HouseholdStatus,EducationLevel,Party,
                                        Q100010, Q100562, Q102674, Q106042, Q106388, Q108343, Q108617, 
                                        Q109244, Q112512, Q113992,Q115899, Q123464, Q123621)


# head(train_subset_questions)

test_subset_questions <- dplyr::select(test, USER_ID,YOB,Gender,Income,HouseholdStatus,EducationLevel,
                                       Q100010, Q100562, Q102674, Q106042, Q106388, 
                                       Q108343, Q108617, Q109244, Q112512, Q113992, 
                                       Q115899, Q123464, Q123621)

# head(test_subset_questions)



#' 
#' \newpage
#' 
#' 
#' ## Splitting the Training and Test Datasets
#' We have learned that when splitting the datasets, there are two competing concerns.  
#' 
#' - Having too small training data, our parameters estimates will have greater variance.
#' - Having too small testing data, our performance statistics will have greater variance. 
#' 
#' A training dataset is defined to be the data used to fit or train the model. Conversely, a testing dataset is the sample of the data used to provide an unbiased evaluation of the final model which was determined from the training dataset. 
#' 
#' Optimal performance of our machine learning model is achieved by identifing the best split ratio between the training and testing dataset.  The larger the original dataset the more appropriate it is to identify an optimial split ratio thereby improving the effectiviness of both teaching and testing the model. 
#' 
#' Common split percentages vary from:
#' 
#' - Train: 80%, Test: 20%
#' - Train: 70%, Test: 30%
#' - Train: 67%, Test: 33%
#' - Train: 50%, Test: 50%
#' 
#' The most common used ratio is the 80:20 split, refered to as the [Pareto Principle](https://en.wikipedia.org/wiki/Pareto_principle), which states that roughly 80% of the effects come from 20% of the causes.
#'   
#' According to the following research done at AT&T Bell Laboratories, [A Scaling law for validation-set training-set size ratio](https://stackoverflow.com/questions/13610074/is-there-a-rule-of-thumb-for-how-to-divide-a-dataset-into-training-and-validatio)
#' the optimal ratio is achieved through the following formula: Test Set(v) : training set(t) = v/t, scales like ln(N/h-max), where N is the number of data families and h-max is the largest complexity of these families. 
#' 
#' Ratio selection is also influenced by which type of modeling technique is being used.   Since we are using multiple modeling techniques, we will choose a less signitific method for selecting our final split ratio.  We will run our modeling project with the for ratios above and select the ratio which offers the best accuracy results. 
#' 
#' While changing the ratio didn't significantly change our accuracy results, the best performing ratio was an 70/30 split.   Therefore, we will use the 80/20 split for the final version of this project. 
#' 
## ----creating_ratio_results,  out.width="50%", fig.align="default", fig.show="hold", fig.cap = "Train and Test Ratio Splits - Actual Results", warning = FALSE, message = FALSE,  echo = FALSE-------
# creates a split ratio analysis table with results to illustrate why a particular ratio was used.
split_ratio_results <- data.frame("Train"=c(.80,.70,.67,.60,.50), "Test"=c(.20,.30,.33,.40,.50), "Optimal Algorithm"=c("Leave-1-Out","Leave-1-Out","Leave-1-Out","Leave-1-Out","Leave-1-Out"), "Optimal Accuracy"=c(.5911,.59443,.58898,.58546,.56955))

   total_results <- nrow(split_ratio_results)
   ordered_results <-sort(split_ratio_results$Optimal.Accuracy, decreasing=FALSE)
   sorted_results <- sort(split_ratio_results$Optimal.Accuracy, decreasing=TRUE)
   index_winner_value <- which(split_ratio_results$Optimal.Accuracy ==  sorted_results[1])
   index_loser_value <- which(split_ratio_results$Optimal.Accuracy ==  ordered_results[1])
   index_runnerup_value <-  which(split_ratio_results$Optimal.Accuracy ==  sorted_results[2])
 
 model_name <- "Train and Test Ratio Splits - Comparing Results"
 split_ratio_results %>%
   knitr::kable(caption = model_name, "latex", digits = 5, format.args = list(scientific=FALSE)) %>% 
            kable_paper("striped", position = "center") %>% 
            kable_styling(full_width=F, latex_options="HOLD_position")  %>%
            row_spec(row=0, bold=T) %>%
            row_spec(index_winner_value: index_winner_value, bold = F, color = "black", background = "green") %>%
            row_spec(index_runnerup_value: index_runnerup_value, bold = F, color = "black", background = "#90ee90") %>% 
            row_spec(index_loser_value: index_loser_value, bold = F, color = "black", background = "#ffa07a")
 
  knitr::include_graphics(c(Split8020, Split7030, Split6733, Split6040, Split5050))

#' 
#' \newpage
#' 
## ----create_ratio_actual_results,  out.width="50%", fig.align="default", fig.show="hold", warning = FALSE, message = FALSE,  echo = FALSE------------------------------------------------------------
# Not used ... place holder

#' 
#' \newpage
#' 
## ---- create_datasets_train_test, warning = FALSE, message = FALSE,  echo = FALSE--------------------------------------------------------------------------------------------------------------------
# Breaking the data in training and testing sets
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
#set.seed(2712)

# used to identify the best performing ratios, uncomment the prefered ratio use.  
# these are the most common ratio used. 
#split_ratio <- .8
split_ratio <- .7
#split_ratio <- .67
#split_ratio <- .6
#split_ratio <- .50

# From above the ratio split results were: 
#Train: 80%    Test: 20%     Optimal Algorithm: Leave-1-Out     Optimal Accuracy: .59711      
#Train: 70%    Test: 30%     Optimal Algorithm: Leave-1-Out     Optimal Accuracy: .59443    
#Train: 67%    Test: 33%     Optimal Algorithm: Leave-1-Out     Optimal Accuracy: .58898
#Train: 60%    Test: 40%     Optimal Algorithm: Leave-1-Out     Optimal Accuracy: .58546
#Train: 50%    Test: 50%     Optimal Algorithm: Leave-1-Out     Optimal Accuracy: .56955

split <- caTools::sample.split(train_subset$Party, SplitRatio = split_ratio)
train_sample <- subset(train_subset, split)
test_sample <- subset(train_subset, !split)


split <- caTools::sample.split(train_subset_questions$Party, SplitRatio = split_ratio)
train_sample_questions <- subset(train_subset_questions, split)
test_sample_questions <- subset(train_subset_questions, !split)

#' 
## ----looking_for_na, echo=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# No value for the overall total.... better value is for each field.
# Looking for NA across all observations for each dataset.
#sum(is.na(train))
#sum(is.na(test))
#sum(is.na(train_subset))
#sum(is.na(test_subset))
#sum(is.na(train_sample))
#sum(is.na(test_sample))

#inspect how NA's impacted the data analysis
train_sample_no_na <- na.omit(train_sample)
train_sample_questions_no_na <- na.omit(train_sample_questions)
test_sample_questions_no_na <- na.omit(test_sample_questions)
#summary(train_sample_no_na)
#summary(train_sample_questions_no_na)


#' 
#' \newpage
#' 
#' ## Data Structure Analysis - The Datasets
#' A complete inspection of the datasets have been provided in Appendix - B - Dataset Inspection.
#' 
#' 
#' ## Visualizing the Data / Initial Observations
#' As an initial method to help understand the survey results collected from the participants, we have generated some simple distribution graphs.    These graphs can be found in Appendix - C: Demographic Figures. 
#' 
#' Summarizing the demographics: 
#' 
#' - Dataset by Party            :  Results show 53% Democrat, 47% Republican
#' - Dataset by Gender           :  Results show 39.1% Female, 60.1% Male
#' - Dataset by Income Bands     :  Top 2 largest Income bands are - 50k-74,999 @ 18.4%, 100,001-150k @ 17.5%
#' - Dataset by Household Status :  Top 2 largest Household Status bands are: Single (no kids) @ 46.7%, Married (w/kids) @ 31.6%
#' - Dataset by Educaiton Levels :  Top 2 largest Education Level bands are: Bachelor's degree @ 25.9%, Current K-12 @ 17.8%
#' - Dataset by Age              :  Average age was 48.16 years.
#' 
#' \newpage
#' 
#' # Correlation Matrixes and HeatMaps 
#' Appendix - F (Correlation Matrixes and Heat Maps) helps identify those fields which are highly correlated. 
#' 
#' A subset of these visualizations have been provided below.  Positive correlations are displayed in blue and negative correlations in red color. Color intensity and the size of the circle are proportional to the correlation coefficients. In the right side of the correlogram, the legend color shows the correlation coefficients and the corresponding colors.
#' 
#' We have trimmed the correlation matrix results to only show the up quadrant.   From these plots, we can identify high correlations to be: 
#' 
#' - Dataset without Questions:   The top 4 coorelations are HouseholdStatus, Income, YOB, and Party
#' - Dataset with Questions   :   The top 4 coorelations are: Q106388, Q102674, Q100562, and Q1023621 
#' 
#' 
## ----print_correlation_matrix_summaries, out.width="70%", fig.align="centered", fig.cap = "Correlation Matrixes", echo=FALSE-------------------------------------------------------------------------
# correlation matrix identificcation
train_sample_num <- data.matrix(train_sample)
train_sample_num_cor <- cor(train_sample_num, use="complete.obs")
train_sample_questions_num <- data.matrix(train_sample_questions)
train_sample_questions_num_cor <- cor(train_sample_questions_num, use="complete.obs")
col<- colorRampPalette(c("blue", "white", "red"))(20)
par(mar=c(2,18,2,0))
corrplot(train_sample_num_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(train_sample_questions_num_cor, type = "upper", order = "hclust", tl.col = "black")

#' 
#' \newpage
#' 
#' # The Modeling Approach:  Simple and More 
#' ## Modeling Background
#' Machine Learning can be summarized as a learning function (f) that maps input variables (X) to output variables (Y).
#' $$ Y = f(x) $$. This basic function takes on different forms, and is for all general purposes unknown. The model's learning 
#' aspect is accomplished through training data. 
#' 
#' Different algorithms make different assumptions or biases about the form of the function and how it can be learned. 
#' 
#' On the preceeding page we have provided an overiview of just a few of these different methods respective to their machine learning disciplines. 
#' 
#' For the purpose of this project, we will focuse on the following model methods: 
#' 
#' -     Classification Trees
#' -  Conditional Probability 
#' -        Linear Regression
#' -      Logistic Regression
#' -         Tree Based Model
#' -   Cross Validation Model
#' 
#' \newpage 
#' 
#' ## Machine Learning Modeling Methods
## ----including_ml_algo, fig.cap="Machine Learning Algorithms", echo=FALSE, out.width='70%',fig.align="centered", fig.show="hold"---------------------------------------------------------------------
  knitr::include_graphics("./ElectionData/MachineLearningAlgorithms.pdf")
#\includepdf[pages={-}]{./ElectionData/MachineLearningAlgorithms.pdf} - replaced to have better size and location control

#' 
#' \newpage
#' 
#' ## Models Choosen
#' During our project endevour,  we will analyize the results of 10 different models across these 6 modeling categories.  The majority of our models used are within the logistic regression discipline. 
#' 
#' - Classification Trees         : Classification and Regression Tree (CART) 
#' - Conditional Probability      : Naive Bayes (NB) 
#' - Linear Regression            : Binary Linear Regression (BLR)
#' - Logistic Regression
#' 
#'      - Stepwise (Simple)
#'      - Binary Logistic Regression (BLR)
#'      - Linear Discriminant Analysis (LDA)
#'      - Quadratic Discriminant Anlaysis (QDA)
#' - Tree-Based                   : Random Forest
#' - Cross-Validation 
#'  
#'      - k-Fold
#'      - Leave-One-Out 
#' 
#' \newpage 
#' 
#' ## Strengths and Weaknesses of the Modeling Categories
#' Strengths and weaknesses of each of the selected model categories has been summarized in the tabel below. 
#' 
## ----including_model_comparison, fig.cap="Model Comparisons", echo=FALSE, out.width='60%', fig.align="centered", fig.show="hold"---------------------------------------------------------------------
  knitr::include_graphics("./ElectionData/Model_Comparison.png")


#' 
#' \newpage
#' 
#' ## Model Tuning 
#' 
#' Initial analysis was performed on the datasets excluding the original survey questions.  These models were later tuned by including the subset of the *key* questions. 
#' 
#' From the datasets, the field USER_ID was irreleveant.  For the fields Gender, HouseholdStatus and EducationLevel the data was analyized via buckets.  
#' 
#' - Gender (2 Buckets)
#' 
#'          - Male
#'          - Female
#'         
#' - Income (6 Buckets)         
#' 
#'          - "under $25,000"
#'          - "$25,001 - $50,000"
#'          - "$50,000 - $74,999"
#'          - "$75,000 - $100,000"
#'          - "$100,001 - $150,000"
#'          - or "over $150,000"
#'          
#' - HouseholdStatus (6 Buckets)   
#' 
#'          - "Domestic Partners (no kids)"
#'          - "Domestic Partners (w/kids)"
#'          - "Married (no kids)"
#'          - "Married (w/kids)"
#'          - "Single (no kids)"
#'          - or "Single (w/kids)"
#' 
#' - EducationalLevel (7 Buckets)  
#' 
#'          - "Current K-12"
#'          - "High School Diploma"
#'          - "Current Undergraduate"
#'          - "Associate's Degree"
#'          - "Bachelor's Degree"
#'          - "Master's Degree"
#'          - or "Doctoral Degree".
#'       
#' Worth noting, these buckets were already created as part of the original data wrangling. 
#'  
#' 
#' 
#' 
#' \newpage
#' 
#' ## Initial Model Results
#' 
#' We now begin our model analysis.   As described in the previous section, we will be analyzing the following machine learning algorithms.
#' 
#' - Classification Trees    : Classification and Regression Tree (CART) 
#' - Conditional Probability : Naive Bayes (NB) 
#' - Linear Regression       : Binary Linear Regression (BLR)
#' - Logistic Regression     : Stepwise (Simple)
#'                           : Binary Logistic Regression (BLR)
#'                           : Linear Discriminant Analysis (LDA)
#'                           : Quadratic Discriminant Anlaysis (QDA)
#' - Tree-Based              : Random Forest
#' - Cross-Validation        : k-Fold
#'                           : Leave-One-Out 
#' 
#' For each algorithm, we will summarize the results in two tables.  A confusion matrix table and a combined results table highlighting the best and worst performs up to that point, starting wth classifications trees. 
#' 
#' \newpage
#' ### Classification Tree Model - Classification and Regression Tree (CART)
#' A Classification And Regression Tree (CART), is a predictive model, which explains how an outcome's values can be predicted based on other values. A CART output is a decision tree where each fork is a split in a predictor variable and each end node contains a prediction for the outcome variable.
#' 
#' In addtion to our tables, we will also present a decision tree graphic to illustrate the different forks and predictors. 
#' 
#' 
## ----cart_model, echo = FALSE, message = FALSE, warning = FALSE, eval = TRUE-------------------------------------------------------------------------------------------------------------------------
# Classification Tree Model - Classification and Regression Tree (CART)
model_name <- "Classification Model: CART"

cartmodeltuned <- rpart(Party ~ .-USER_ID, 
                        data = train_sample,  
                        method = "class", 
                        control = rpart.control(cp = 0.015, maxdepth = 4, minsplit = 10))

cart1predtuned <- predict(cartmodeltuned, newdata = test_sample, type = "class")
test_confusion <<- confusionMatrix(cart1predtuned, test_sample$Party)
temp_train_sample <- train_sample   # going to change level names to shorter names

# changing level names to ensure fits well on plot
levels(temp_train_sample$Party) <- gsub("Republican","REP", levels(temp_train_sample$Party))
levels(temp_train_sample$Party) <- gsub("Democrat","DEM", levels(temp_train_sample$Party))
levels(temp_train_sample$HouseholdStatus) <- gsub("Domestic Partners ","DP", levels(temp_train_sample$HouseholdStatus))
levels(temp_train_sample$HouseholdStatus) <- gsub("Married ","M", levels(temp_train_sample$HouseholdStatus))
levels(temp_train_sample$HouseholdStatus) <- gsub("Single ","S", levels(temp_train_sample$HouseholdStatus))
levels(temp_train_sample$HouseholdStatus) <- gsub(".kids","-k", levels(temp_train_sample$HouseholdStatus))
#levels(temp_train_sample$HouseholdStatus)

#prp(cartmodeltuned)

#' 
#' Decisons tree based for the Classificaiton Model - CART
#' 
## ----plotting_decision_trees, out.width="80%", fig.align="centered", fig.show="hold", fig.cap="Decision Tree", fig.show="hold", echo = FALSE, message = FALSE, warning = FALSE-----------------------
# Creating and printing the CART decision tree

party_fit <- ctree(Party ~ .-USER_ID, data = temp_train_sample)
par(mar = c(3,8,3,2))
plot(party_fit, main="Conditional Interference Tree for Voting Party",  gp = gpar(fontsize = 8))

#' 
#' \newpage
#' 
## ----create_CART_CM, fig.cap="Confustion Matrix - CART", fig.show="hold", out.width="100%", fig.align="centered", fig.show="hold", echo = FALSE, message = FALSE, warning = FALSE--------------------
func_draw_confusion_matrix(test_confusion, cart1predtuned, model_name) 
func_update_print_results(test_confusion, model_name)

#' 
#' 
## ----results_cart, out.width="50%", fig.align="centered", fig.show="hold", echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------
# trying to foce knit pdf to be the correct order.  
func_create_print_results(test_confusion, model_name, TRUE)  

#' 
#' 
#' \newpage
#' 
#' ### Tree Based Model - Random Forest Model (RFM)
#' Continuing our tree based models, we will look at the performace of a Random Forest Model (RFM).  A Random Forest Tree
#' is a learning method for classification, regression and other tasks that operate by constructing a multitude of decision trees at the training time and outputting classes (classification) or mean prediction (regression) of the individual trees.
#' 
#' Our results are as follows: 
#' 
## ----random_forest_model, fig.cap="Confusion Matrix - RFM", out.width="100%", fig.align="centered", fig.show="hold",echo=FALSE-----------------------------------------------------------------------

# Tree Based Model - Random Forest Model (RFM)
model_name <- "Tree Based Model: Random Forest (RFM)"

fit_rf <-randomForest(Party ~.-USER_ID, data=train_sample, importance=TRUE, prOximity=TRUE, na.action=na.roughfix)
PredTestRF1 <- predict(fit_rf, newdata = test_sample)

test_confusion <<- confusionMatrix(PredTestRF1, test_sample$Party)
func_draw_confusion_matrix(test_confusion, PredTestRF1, model_name)
func_update_print_results(test_confusion, model_name)

#' 
## ----results_rfm, out.width="50%", fig.align="centered", fig.show="hold", echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------
# trying to foce knit pdf to be the correct order.  
func_create_print_results(test_confusion, model_name, TRUE)  

#' 
#' 
#'  \newpage
#' 
#' ### Conditional Probablity Model - Naive Bayes (NB)
#' Continuing our learning model momentum, we move on to a different class of models.  Specifically, conditional probability modeling and the Naive Bayes (NB) algorithm. 
#' 
#' Naïve Bayes is a classification method based on Bayes’ theorem that derives the probability of the given feature vector being associated with a label. Naïve Bayes has a naive assumption of conditional independence for every feature, which means that the algorithm expects the features to be independent which may not always be the case.
#' 
#' Naïve Bayes assumes all the features to be conditionally independent. So, if some of the features are in fact dependent on each other (in case of a large feature space), the prediction might be poor.
#' 
## ----naive_bayes_model, fig.cap="Confusion Matrix - Naive Bayes", out.width="100%", fig.align="centered", fig.show="hold", echo=FALSE----------------------------------------------------------------
# Conditional Probablity Model - Naive Bayes (NB)

model_name <- "Conditional Probability Model - Naive Bayes"

naive_model <- naiveBayes(Party ~., data = train_sample)
naive_bayes_predict <- predict(naive_model, newdata = test_sample, type = "class")      #Identifiying the predictors

test_confusion <<- confusionMatrix(naive_bayes_predict, test_sample$Party)
func_draw_confusion_matrix(test_confusion, naive_bayes_predict, model_name)
func_update_print_results(test_confusion, model_name)

#' 
## ----results_nb, out.width="50%", fig.align="centered", fig.show="hold", echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------
# trying to foce knit pdf to be the correct order.  
func_create_print_results(test_confusion, model_name, TRUE)  

#' 
#' \newpage
#' 
#' ### Logistic Regression Model (LRM) - Step Wise
#' Moving on to a different type of regression models (linear and logistic), We will start with logistic regression algorithms. 
#' The majority of our models will be from this discipline. Logistic regression is an algorithm used to predict the probability of a target variable. The nature of the target or dependent variable is dichotomous, which means there would be only two possible classes.
#' 
#' It is the go-to method for binary classification problems (problems with two class values). 
#' 
#' Starting off the logistic regressions series of models is Stepwise Regression.   Stepwise Regression is a method of fitting regression models in which the choice of predictive variables is carried out by a procedure where each step utilizes a variable considered for addition to or subtraction from the set of explanatory variables based on some prespecified criterion.
#' 
#' The results from our Stepwise mode are listed below.
#' 
#' 
## ----simple_logistic_regression_model_stepAIC, fig.cap="Confusion Matrix - LRM", out.width="100%", fig.align="centered", fig.show="hold", warning = FALSE, message = FALSE,  echo = FALSE------------

# Logistic Regression Model (LRM) - Step Wise

model_name <- "Logistic Regression Model (LRM) - Stepwise"

model_blr_2 <- glm(Party ~ .-USER_ID, data = train_sample_no_na, family = "binomial")  
simple_stepAIC <- stepAIC(model_blr_2, trace=FALSE)
threshold <- 0.5
pred_blr_stepAIC <- predict(model_blr_2, newdata = test_sample, type = "response")  
predtestLabel_blr_stepAIC <- as.factor(ifelse(pred_blr_stepAIC < threshold, "Democrat", "Republican"))

test_confusion <<- confusionMatrix(predtestLabel_blr_stepAIC, test_sample$Party)
func_draw_confusion_matrix(test_confusion, predtestLabel_blr_stepAIC, model_name)
func_update_print_results(test_confusion, model_name)

#' 
## ----results_step, out.width="50%", fig.align="centered", fig.show="hold", echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------
# trying to foce knit pdf to be the correct order.  
func_create_print_results(test_confusion, model_name, TRUE)  

#' 
#' 
#' \newpage
#' 
#' ### Logistic Regression (BLR) - Binary
#' Binary logistic regression (BLR) is the simplest form of logistic regression in which the target or dependent variable can have only 2 possible types either 1 or 0.
#' 
#' Results from our binary logistic regression model can be found below. 
#' 
## ----binary_logistic_regression_blr, fig.cap="Confusion Matrix - BLR", out.width="100%", fig.align="centered", fig.show="hold", echo = FALSE, message = FALSE, warning = FALSE-----------------------
# Logistic Regression (BLR) - Binary
model_name <- "Logistic Regression Moodel (LRM) - BLR"
tot_dataset_columns <- ncol(train_sample_no_na)
train_sample_no_na_num <- data.matrix(train_sample_no_na, rownames.force = NA)

HouseholdStatus_bucket <- train_sample_no_na$HouseholdStatus
EducationLevel_bucket <-  train_sample_no_na$EducationLevel
Gender_bucket <-  train_sample_no_na$Gender

#xtabs(~Party + HouseholdStatus_bucket, data=train_sample_no_na)
#xtabs(~Party + EducationLevel_bucket, data=train_sample_no_na)
#xtabs(~Party + Gender_bucket, data=train_sample_no_na)

model_blr <- glm(Party ~ .-USER_ID, data = train_sample, family = "binomial")         # simple logistic regression model
pred_blr <- predict(model_blr, newdata = test_sample, type = "response")      #Identifiying the predictors
threshold <- 0.5
predtestLabel_blr <- as.factor(ifelse(pred_blr < threshold, "Democrat", "Republican"))
test_confusion <<- confusionMatrix(predtestLabel_blr, test_sample$Party)

func_draw_confusion_matrix(test_confusion, predtestLabel_blr, model_name)
func_update_print_results(test_confusion, model_name)


#' 
## ----results_blr, out.width="50%", fig.align="centered", fig.show="hold", echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------
# trying to foce knit pdf to be the correct order.  
func_create_print_results(test_confusion, model_name, TRUE)  

#' 
#' 
#' 
#' \newpage
#' 
#' ### Logistic Regression - Latent Dirichlet Allocation (LDA)
#' Linear Dirichlet Allocation (LDA) logistic regression algorithems. LDA algorithms are a generalization of Fisher's linear discriminant, a method used in statistics, pattern recognition, and machine learning to find a linear combination of features that characterizes or separates two or more classes of objects or events. The resulting combination may be used as a linear classifier, or, more commonly, for dimensionality reduction before later classification
#' 
#' Below are our observed resutls using LDA techniques.
#' 
## ----LDA_Model, fig.cap="Confusion Matrix - LDA", out.width="100%", fig.align="centered", fig.show="hold", echo = FALSE, message = FALSE, warning = FALSE--------------------------------------------
# Logistic Regression - Latent Dirichlet Allocation (LDA)

model_name <- "Logistic Regression Model (LRM) - LDA"

ldaModel <- lda(Party ~ ., data=train_sample)
# ldaModel <- lda(Party ~Gender + HouseholdStatus, data=train_sample)

predictions_lda <- predict(ldaModel, newdata = test_sample, type = "response")
lda_model_accuracy <- mean(!is.na(predictions_lda$class==test_sample$Party))

test_confusion <<- confusionMatrix(predictions_lda$class, test_sample$Party)
func_draw_confusion_matrix(test_confusion, predictions_lda$posterior, model_name)
func_update_print_results(test_confusion, model_name)

#' 
#' 
## ----results_lda, out.width="50%", fig.align="centered", fig.show="hold", echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------
# trying to foce knit pdf to be the correct order.  
func_create_print_results(test_confusion, model_name, TRUE)  

#' 
#' 
#' \newpage
#' ### Logistic Regression -  Quadratic Discrimination Analysis (QDA)
#' Like LDA, the QDA classifier assumes that for the observations  each identified class is drawn from a Gaussian distribution. However, unlike LDA, QDA assumes that each class has its own covariance matrix. In other words, the predictor variables are not assumed to have common variance across each of their associated levels. 
#' 
#' QDA performance is ... 
#' 
## ----quadratic_discriminant_model, out.width="100%", fig.align="centered", fig.show="hold", fig.cap="Confusion Matrix - QDA", echo = FALSE, message = FALSE, warning = FALSE-------------------------
# Logistic Regression -  Quadratic Discrimination Analysis (QDA)
model_name <- "Logistic Regression Model (LRM) - QDA"

qdaModel <- qda(Party ~ ., data=train_sample)
predictions_qda <- predict(qdaModel,test_sample)     # Make predictions with Training Sample
qda_model_accuracy <- mean(!is.na(predictions_qda$class==train_sample$Party)) 

test_confusion <<- confusionMatrix(predictions_qda$class, test_sample$Party)
func_draw_confusion_matrix(test_confusion, predictions_qda$posterior, model_name)
func_update_print_results(test_confusion, model_name)

#' 
#' 
## ----results_qda, out.width="50%", fig.align="centered", fig.show="hold", echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------
# trying to foce knit pdf to be the correct order.  
func_create_print_results(test_confusion, model_name, TRUE)  

#' 
#' 
#' \newpage
#' 
#' ## Model Tuning Results
#' In the previous sections, we explored 7 different predictor algorithms without considering any tuning opportunities. We will now apply a generalized tuning across these 7 different algorithms to see if our prediction results experience any improvements. 
#' 
#' Our tuning approach will include additonal predictors which were based upon the survey questionaire. 
#' 
#' While the questionaire included 100 questions, we used our correlation matrixes and heatmaps to narrow our tuning dataset to include only those questions that had the highest correlation. 
#' 
#' In the interest of conserving space, we will only display that final table of results vs. individual model results which was done in the previous sections. 
#' 
## ----tuned_models, out.width="50%", fig.align="centered", fig.show="hold", fig.cap="Confusion Matrix - Tuned Models", warning = FALSE, message = FALSE,  echo = FALSE--------------------------------

# Model 7: LDA-Tuned
model_name <- "* TUNED LRM - LDA Model"
threshold <- 0.5

ldaModel_tuned <- lda(Party ~ HouseholdStatus +YOB, data=train_sample_questions)
predictions_lda_tuned <- predict(ldaModel_tuned, newdata = test_sample_questions, type = "response", na.action=na.exclude)
test_confusion_tuned <- confusionMatrix(predictions_lda_tuned$class, test_sample_questions$Party)
confusion_class_results <- as.matrix(test_confusion_tuned, what = "classes")
func_update_print_results(test_confusion_tuned, model_name)
func_create_print_results(test_confusion_tuned, model_name, FALSE) 


# Model 6 - QDA Tuned
model_name <- "* TUNED LRM - QDA Model"
qdaModel_tuned <- qda(Party ~ HouseholdStatus +YOB, data=train_sample_questions)
predictions_qda_tuned <- predict(qdaModel_tuned,test_sample_questions, type = "response", na.action=na.exclude)    
test_confusion_tuned <- confusionMatrix(predictions_qda_tuned$class, test_sample_questions$Party)
#func_draw_confusion_matrix(test_confusion_tuned, predictions_qda_tuned$posterior, model_name)
func_update_print_results(test_confusion_tuned, model_name)
func_create_print_results(test_confusion_tuned, model_name, FALSE) 


# Model 5 - LRM StepWise
model_name <- "* TUNED LRM - Stepwise"
model_blr_2_tuned <- glm(Party ~ HouseholdStatus +YOB, data=train_sample_questions_no_na, family = "binomial")  
simple_stepAIC_tuned <- stepAIC(model_blr_2_tuned, trace=FALSE)
pred_blr_stepAIC_tuned <- predict(model_blr_2_tuned, newdata = test_sample_questions, type = "response")  
predtestLabel_blr_stepAIC_tuned <- as.factor(ifelse(pred_blr_stepAIC_tuned < threshold, "Democrat", "Republican"))
test_confusion_tuned <- confusionMatrix(predtestLabel_blr_stepAIC_tuned, test_sample_questions$Party)
func_update_print_results(test_confusion_tuned, model_name)
func_create_print_results(test_confusion_tuned, model_name, FALSE) 



# Model 4 - naive Bayes
model_name <- "* TUNED - Naive Bayes"
naive_model_tuned <- naiveBayes(Party ~ HouseholdStatus +YOB, data=train_sample_questions)
naive_bayes_predict_tuned <- predict(naive_model_tuned, newdata = test_sample_questions, type = "class")   
test_confusion_tuned <- confusionMatrix(naive_bayes_predict_tuned, test_sample_questions$Party)
func_update_print_results(test_confusion_tuned, model_name)
func_create_print_results(test_confusion_tuned, model_name, FALSE) 


# Model 3 - BLR
model_name <- "* TUNED - LRM - BLR"
model_blr_tuned <- glm(Party ~ HouseholdStatus +YOB, data=train_sample_questions, family = "binomial")   
pred_blr_tuned <- predict(model_blr_tuned, newdata = test_sample_questions, type = "response")     
predtestLabel_blr_tuned <- as.factor(ifelse(pred_blr_tuned < threshold, "Democrat", "Republican"))
test_confusion_tuned <- confusionMatrix(predtestLabel_blr_tuned, test_sample$Party)
func_update_print_results(test_confusion_tuned, model_name)
func_create_print_results(test_confusion_tuned, model_name, FALSE) 


# Model 2 - Random Forest
model_name <- "* TUNED - Random Forest"
fit_rf_tuned<-randomForest(Party ~ HouseholdStatus +YOB, data=train_sample_questions, importance=TRUE, prOximity=TRUE, na.action=na.roughfix)
PredTestRF1_tuned <- predict(fit_rf_tuned, newdata = test_sample_questions)
test_confusion_tuned <- confusionMatrix(PredTestRF1_tuned, test_sample_questions$Party)
func_update_print_results(test_confusion_tuned, model_name)
func_create_print_results(test_confusion_tuned, model_name, FALSE) 


# Model 1 - CART Tuned
model_name <- "* TUNED - CART"
cartmodeltuned_tuned <- rpart(Party ~ HouseholdStatus +YOB, 
                        data = train_sample_questions, 
                        method = "class", 
                        control = rpart.control(cp = 0.015, 
                                                maxdepth = 4, 
                                                minsplit = 10))
cart1predtuned_tuned <- predict(cartmodeltuned_tuned, newdata = test_sample_questions, type = "class")
test_confusion_tuned <- confusionMatrix(cart1predtuned_tuned, test_sample_questions$Party)
func_update_print_results(test_confusion_tuned, model_name)


#' 
## ----results_tuned, out.width="50%", fig.align="centered", fig.show="hold", echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------
# required to force knit pdf to be the correct order.  
func_create_print_results(test_confusion_tuned, model_name, TRUE)  

#' 
#' 
#' \newpage
#' ### Model Resampling: Cross-Validatioan - Leave-One-Out
#' Venturing out a bit farther in the relm of machine learning algorithms, we will be extending our modeling analysis to include another class of models called cross-validation.  Cross-validation is a procedure that has a single parameter called k which refers to the number of groups or folds that a given data sample is to be split into.  Cross-validation is primarily used in applied machine learning to estimate the skill of a machine learning model on unseen data. It uses a limited sample in order to estimate how the model is expected to perform.
#' 
#' It is a popular method because it is simple to understand and because it generally results in a less biased or less optimistic estimate of the model skill over other methods, such as a simple train/test split.
#' 
#' The first (of our two) cross-validation algorithms will be the Leave-One-Out.  Leave-One-Out models number of folds equals the number of instances in the data set. Thus, the learning algorithm is applied once for each instance, using all other instances as a  training set and using the selected instance as a single-item test set. This process is closely related to the statistical method of jack-knife estimation.
#' 
#' Our Leave-One-Out performance results are listed below. 
#' 
## ----cv_leave_one_out_tuning, out.width="100%", fig.align="centered", fig.show="hold", fig.cap="Confusion Matrix - X-VAL - Leave-One-Out", echo=FALSE, message=FALSE, warning=FALSE------------------
# Model Resampling: Cross-Validatioan - Leave-One-Out

model_name <- "Cross Validation Model - Leave-One-Out"
glm.fit_loot <- glm(Party ~ ., data=train_sample_no_na, family = "binomial")  #coef(glm.fit)
lm.fit_loot <- lm(Party ~ ., data=train_sample_no_na, family = "binomial")    #coef(lm.fit)
glm.fit_loot <- glm(Party ~ ., data=train_sample_no_na, family = "binomial")  
#cv.err=cv.glm(train_sample_no_na, glm.fit)

pred_cv_glm_loot <- predict(glm.fit_loot, newdata = test_sample_questions, type = "response")  
pred_cv_glm_factor_loot <- as.factor(ifelse(pred_cv_glm_loot < threshold, "Democrat", "Republican"))
test_confusion_cv <- confusionMatrix(pred_cv_glm_factor_loot, test_sample_questions$Party)
func_draw_confusion_matrix(test_confusion_cv,pred_cv_glm_factor_loot, model_name)
func_update_print_results(test_confusion_cv, model_name)

#' 
#' \newpage
#' 
## ----print_leave_one_out_results, out.width="100%", fig.align="centered", fig.show="hold", echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------
func_create_print_results(test_confusion_cv, model_name, FALSE) 

#' 
#' \newpage
#' 
## ----print_final_model_summary, out.width="100%", fig.align="centered", fig.show="hold", fig.cap="Results - X-VAL - Leave-One-Out", warning = FALSE, message = FALSE,  echo = FALSE------------------
summary(glm.fit_loot)

#' 
#' \newpage
#' ### Model Resampling: K-Fold
#' Our final category for both cross-validation and this project is the k-Fold algorithm.  A K-Fold cross-validation algortihm takes a given data set, splits it into K number of sections/folds where each fold is used as a testing set at some point. 
#' 
#' Using a 5-Fold example, the first iteration - representing the first fold - is used to test the model and the rest are used to train the model. In the second iteration - the 2nd fold is used as the testing set while the rest serve as the training set. This process is repeated until each fold of the 5 folds have been used as the testing set.
#' 
#' This model is highly dependent on a carefully choosen k value. 
#' 
#' A poorly chosen value for k may result in mis-representative, such as a score with a high variance (that may change a lot based on the data used to fit the model), or a high bias, (such as an overestimate of the skill of the model).
#' 
#' Three common tactics for choosing a value for k are as follows:
#' 
#' - Representative: The value for k is chosen such that each train/test group of data samples is large enough to be statistically representative of the broader dataset.
#' - k=10: The value for k is fixed to 10, a value that has been found through experimentation to generally result in a model skill estimate with low bias a modest variance.
#' - k=n: The value for k is fixed to n, where n is the size of the dataset to give each test sample an opportunity to be used in the hold out dataset. This approach is called leave-one-out cross-validation.
#' 
#' For our analysis we will be using the k=10 approach with results displayed below. 
#' 
## ----cv_kfold_tuning, out.width="100%", fig.align="centered", fig.show="hold", fig.cap="Confusion Matrix - X-VAL - k-Fold", echo=FALSE, message=FALSE, warning=FALSE---------------------------------
# Model Resampling: K-Fold
model_name <- "Cross Validation Model - k-Fold"
cv.error.10=rep(0,10)
for (i in 1:10) {
   glm.fit <- glm(Party~Gender, data=train_sample_no_na, family = "binomial")  
   cv.error.10[i] =cv.glm(train_sample_no_na, glm.fit, K=10)$delta[1]
}
pred_cv_glm <- predict(glm.fit, newdata = test_sample_questions, type = "response")  
pred_cv_glm_factor <- as.factor(ifelse(pred_cv_glm < threshold, "Democrat", "Republican"))
test_confusion_cv <-  confusionMatrix(pred_cv_glm_factor, test_sample_questions$Party)
func_draw_confusion_matrix(test_confusion_cv,pred_cv_glm_factor,model_name)
func_update_print_results(test_confusion_cv, model_name)

#' 
#' \newpage
#' 
#' # Conclusion
#' We started this project with the desire to achieve two goals.  Recalling from Section 3 - Project Goal, these goals were:
#' 
#' - Goal #1:  Predict which candidate platform will win the election
#' - Goal #2:  Identify which questions have the largest influence on Goal #1.
#' 
#' In reviewing the results from our corelation matrixes and heatmaps (Section 6), we achieved Goal #2 by identify the questions having the highest correlation.  They were: Q106388, Q102674, Q100562 and Q1023621.   As an anxillary benefit, we were also able to identify the fields having the highest correlation.  The top 4 fields were: HouseholdStatus, Income, YOB and Party. 
#' 
#' Based upon our dataset, Goal #1 was uninamiously identified to be the Democratic party.   These results were achieved and validated using 10 different modeling techniques spanning 6 different modeling categories. 
#' 
#' The following table summarizes our results achieved. 
#' 
## ----print_kfold_results, out.width="50%", fig.align="centered", fig.show="hold", echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------
func_create_print_results(test_confusion_cv, model_name, TRUE)     # Printing the final table

#' 
#' From these results, the top two performing techniques were: 
#' 
#' - Cross Validation Leave-One-Out Model with an accuracy of .59443
#' - Tree Based Random Forest Tuned Model with an accuracy of .56560
#' 
#' Our worst peforming technique was the Binary Logistic Regression model with an accuracy of .50803
#' 
#' Another conclusion that can be extraopolated from our final results envolves the performance of the Random Forest model.  Despite having the 2nd best accuracy results, when comparing the top performers for the tuned and non-tuned models the Random Forest model had the biggest gain in accuracy improvement (with a gain of .0203).  
#' 
#' Finally, exploring our top performing technique further, we can observe additional information from the top performing model.  This information was found in Section 8.7.1-Model Resampling: Cross-Validation - Leave-One_Out which  tells us that: 
#' 
#' For continuous variables, our top performing interpretations are as follows: 
#' 
#' - For every one unit increase in Gender=Male, the log odds of being a Democrat (vs. Republican) increases by .507
#' - For every one unit increase in Married (w/kids), the log odds of being a Democrat increases by .892
#' - For every one unit increase in Married (no kids), the log odds of being a Democrat increases by .715
#'       
#' For categorical variables, our top performing technique further, we can observe that: 
#' 
#' - Gender:     Being Male, changes the log odds of being Democrat by .507
#' - Income:     Being in the Income bracket of $25,001-$50,000 changes the log odds of being Democrate by .644
#' - Status:     Being Married (w/kids) changes the log odds of being Democrat by .892 and Married (w/o kids) changes by .715
#' - Education:  Having a Masters degree changes the log odds of being Democrat by -.443 (in favor of Republican)
#' 
#' Note: National poverty is classified as anyone making less than $32,000 per year for single person household,  $43,000 for a household with two persons and $54,300 for a 3 person household. [2020 Health & Human Services Poverty Guidelines / Federal Poverty Levels](https://www.payingforseniorcare.com/federal-poverty-level)
#' 
#' 
#' 
#' \newpage  
#' 
#' # Future Work
#' ## Data Acquisition Improvements
#' ### Voter Turnout
#' Since the Voting Rights Act of 1965, there has been a long term increase in the ability of individuals to paricipate in elections. Especially here within the United States.   Conversely the effects of other legislation intended to increase voter turnout, such as the National Voter Registration Act, have been more limited on their improved performance. 
#' 
#' Many believe that voter turnout has a strong correlation to a thriving democracy.  Hence, policymakers and citizens often support electorial reform measures based on whether they will or will not increase voter turnout.   Despite these political debates, academic research suggests that in most cases, policy changes usually has little or not effct on voter turnout. 
#' 
#' However, according to [What Affects Voter Turnout Rates](https://www.fairvote.org/what_affects_voter_turnout_rates).  There are 5 major categories that influence voter turnout.  They are: 
#' 
#' - Electorial Competitiveness
#' - Election Type
#' - Voting Laws
#' - Demographics
#' 
#' Additional research also suggest another category - years with presidential elections.   This is defined to be either "on years" representing presidential election years or "off years" those years that are not part of presidential election years.  Elections that occur in odd-numbered years and at times other than November typically have significant lower turnout rates. 
#' 
#' From the above additions, the only variable which was included in our project were the demographics category.  Extending our general model to include these additional categories would be another opportunity. 
#' 
#' However, it should be noted that the chief difficulty in using public opinion surveys to determine individual turnout and thus predict election results, is the problem of social-desirability bias.  This unique phenonama can be defined as someone voting because of being a 'good citizen'. 
#' 
#' Our recommendation would be to avoid using this modeling effect.  The other categories mentioned above should be sufficient.
#' 
#' ### 13 keys to the White House
#' Based on the research devised by the American historian Allan Lichtman and Russian scientist Vladimir Keilis-Borok (Lichtman et al., 1981), the authors were able to create a data model method that included 13 key factors.  The authors believed these 13 key factors would more accruately predict presidential election outcomes.   
#' 
#' Depending on how many questions were answered in a certain way, their model predicted an outcome of the election. It is believed that this method has been found to be quite predictive of the election results: it has been predictive of every election since the method was devised in 1981. 
#' 
#' It would be an interesting exercise to run our model based upon their most recent dataset. Extending predictions even further could be done by adding this data to other predictive datasources.
#' 
#' 
#' ### More Data / Datasources
#' While the datasets contained a good sample size, it would be interesting to investigate other datas sources espcially those that focus on polling, voter census, and demographic data.  Obtaining additional sources will also help minimize any biases that may have occurred within our dataset. 
#' 
#' Datasets could also contain such potential influencers as holidays, events, airport travel demographics, and most recently COVID related cases and volume for potentially each state or its entirity. 
#' 
#' 
#' ### COVID Demographics
#' With the recent COVID pandemic, it is anticipated that we will experience some changes in voting behavior.   The actual influence at this time is unclear and will be studied for years to come. 
#' 
#' ## Modeling Improvements
#' ### Ensemble Methods - Bagging, Boosting and Stacking
#' Machine Learning using Ensemble Methods, help improve results by combining several methods improving predictive performance compared to singular models.   It should be noted, that the use of Ensemble Methods have placed first in many competitions such as: the Netflix Competition, KDD 2009, and Kaggle. 
#' 
#' Bagging stands for bootstrap aggregation and is a way to decrease the variance in the prediction by generating additional data for training from datasets using combinations with repetitions to produce multi-sets of the original data. 
#' 
#' Boosting is an iterative technique which adjusts the weight of an observation based on the last classification adding more weight to data which was misclassified by earlier evaluation rounds.  It is used to convert weak learning algorithms into strong learning algorithms. 
#' 
#' Stacking is a learning technique that combines multiple classification or regression models via a meta-classifier or meta-regressor where the meta-model is trained on the output of the base level model as features. 
#' 
#' Since Ensemble Methods have had huge compeition success, it would be worth an exploration on how they perform for our election predictions. 
#' 
#' ### Random Forest Tuning with Boruta
#' An interesting observation in comparing the final results between our tuned and non-tuned models was that the Random Forest model had the biggest gain in improvement.   Despite having the 2nd best accuracy results, there may be an opportunity to achieve better results using Boruta Tuning Machine algorithms. 
#' 
#' A Boruta algorithm is a wrapper built around the random forest classification algorithm implemented in the R package randomForest (Liaw and Wiener 2002). It uses a concept of of Feature Importance.  Feature Importance is a class of techniques for assigning scores to the input features as a predictive model that indicates the relative importance of each feature when making a prediction.
#' 
#' Through this technique, Boruta modeling tries to capture all the important/interesting features you might have in the dataset with respect to an outcome variable. 
#' 
#' With this tuning, it is anticipated that our accuruacy results would be further improved and may even become the best perfomring technique. 
#' 
#' ### Neural Learning Model 
#' The methods used in this project were, for the most part, based upon some type of classification problem.  Another opportunity would be to build a Neural Network Learning model based on predicting voter's political preferences. 
#' 
#' 
#' ## Code Optimization
#' Code opitimization opportunities include such areas as: Libriary and Package Optimization, R performance tuning, and general organizational changes. 
#' 
#' 
#' ## Report Optimizations
#' Due to the idiosynchroncies of YAML, Rmarkdown, Kniter and LaTex generation, we have a wealth of conversion improvements rich within the area of output generation specific to cross-referencing, formatting, figure and table placement, color control and font formatting for emphasis. 
#' 
#' Additionally, the table formating function could be updated to:
#' 
#' - format a particular cell versus the entire row
#' - evaluating other criteria when accuracy results are identical (i.e. multiple criteria for ordered ranking)
#' 
#' For those that would be interested in having more background on the models, mathimatical formulas could also be added. 
#' 
#' 
#' ## Data Wrangling
#' Optimization for wrangling the requried datasets would also be recommended.  Checks and clean up should be consolidated in the intial beginning in a format that can be used throughout the entire project.   Currenty, the program has had to perform additional conditional checks and conversions for the modules to properly execute. 
#' 
#' 
#' ## Results
#' While our project has produced results, it is unclear if these results are close to the antipiated results. On the surface, it appears they are lower than expected.   If this is the case, guideance will be needed to understand where mistakes were made. 
#' 
#' 
#' \newpage
#' 
#' # Appendixes
#' 
#' ## Appendix - A: Package Installations
#' 
#' The following packages were loaded for this project: 
#' 
## ----disply_installed_packages, warning = FALSE, message = FALSE,  echo = FALSE----------------------------------------------------------------------------------------------------------------------
(.packages())

#' 
#' 
#' \newpage
#' 
#' 
#' ## Appendix - B - Dataset Inspection
#' 
#' ### Dataset - Train
#' 
## ----inspecting_train_dataset, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------
# Original data read in from the train.csv file
cat(sprintf("Data Stucture: Original Dataset = Train Dataset\n"))
str(train)
cat(sprintf("\n Inspecting Initial Rows: Original Dataset = Train Dataset\n"))
head(train)
cat(sprintf("\n Statistical Summary: Original Dataset = Train Dataset"))
summary(train)

#' 
#' 
#' \newpage
#' 
#' ### Dataset - Test
#' 
## ----inspecting_test_dataset, echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------
# Original data read in from the test.csv file
cat(sprintf("Data Stucture: Original Dataset = Test Dataset\n"))
str(test)
cat(sprintf("\n Inspecting Initial Rows: Original Dataset = Test Dataset\n"))
head(test)
cat(sprintf("\n Statistical Summary: Original Dataset = Test Dataset"))
summary(test)

#' 
#' 
#' \newpage
#' 
#' ### Dataset - Train Subset
#' 
## ----inspecting_train_subset_dataset, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------
# Dropping the questions fields from the original train dataset
cat(sprintf("Data Structure: Subset Dataset = Train Subset\n"))
str(train_subset)
cat(sprintf("\n Inspecting Initial Rows: Subset Dataset = Train Subset\n"))
head(train_subset)
cat(sprintf("\n Statistical Summary: Subset Dataset = Train Subset"))
summary(train_subset)

#' 
#' 
#' \newpage
#' 
#' 
#' 
#' ### Dataset - Test Subset
#' 
## ----inspecting_test_subset_dataset, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
# Dropping the questions fields from the original test dataset
cat(sprintf("Data Structure: Subset Dataset = Test Subset\n"))
str(test_subset)
cat(sprintf("\n Inspecting Initial Rows: Subset Dataset = Test Subset\n"))
head(test_subset)
cat(sprintf("\n Statistical Summary: Subset Dataset = Test Subset\n"))
summary(test_subset)

#' 
#' \newpage
#' 
#' ### Dataset - Train Sample (70% of Training Dataset)
#' 
## ----inspecting_train_sample_dataset, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------
# ML Dataset split 70% for training
cat(sprintf("Data Structure: Train Sample 70%% Dataset = Train Sample\n"))
str(train_sample)
cat(sprintf("\n Inspection Initial Rows: Train Sample 70%% Dataset = Train Sample\n"))
head(train_sample)
cat(sprintf("\n Statistical Summary: Train Sample 70%% Dataset = Train Sample \n"))
summary(train_sample)

#' 
#' \newpage
#' 
#' ### Dataset - Test Sample (30% of Test Dataset)
#' 
## ----inspecting_test_sample_dataset, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
# ML Dataset split 30% for testing
cat(sprintf("Data Structure: Test Sample 30%% Dataset = Test Sample\n"))
str(test_sample)
cat(sprintf("\n Inspection Initial Rows: Test Sample 30%% Dataset = Test Sample\n"))
head(test_sample)
cat(sprintf("\n Statistical Summary: Test Sample 30%% Dataset = Test Sample\n"))
summary(test_sample)

#' 
#' \newpage
#' 
#' ### Dataset - Train Sample (70% of Training Dataset excluding NA Fields) with Key Questions
## ----inspecting_train_sample_no_na_dataset, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------
# ML Dataset split 70% for training with key quesitons added back in
cat(sprintf("Data Structure: Train Sample Dataset excluding NA Fields = Test Sample No NA"))
str(train_sample_no_na)
cat(sprintf("\n Inspection Initial Rows: Train Sample Dataset excluding NA Fields - Test Sample No NA\n"))
head(train_sample_no_na)
cat(sprintf("\n Statistial Summary: Train Sample Dataset excluding NA Fields - Test Sample No NA\n"))
summary(train_sample_no_na)

#' 
#' 
#' \newpage
#' 
#' ## Appendix - C: Demographic Figures
#' ### Plotting Dataset by Voting Party
#' 
## ----plot_SampleSize_ByParty, fig.show="hold", out.width="85%",  echo=FALSE--------------------------------------------------------------------------------------------------------------------------
# train_sample %>% group_by(Party) %>% summarize(n())
test_graph <- train_sample %>% filter(Party != "NA") %>% dplyr::group_by(Party) %>% dplyr::summarise(Totals=n())
all_votes <- sum(test_graph$Totals)

ggplot(test_graph,aes(x=Party,y=Totals,fill=Party)) + 
    geom_bar(stat="identity") +
    scale_fill_manual(values = c("red", "blue")) +
    geom_text(aes(x=Party, y=Totals, label = paste(sprintf("%i/%.1f%%",Totals, (Totals/all_votes) * 100)), 
          vjust=3.5), position = position_dodge(width=0.9)) +
    labs(title="Sample Size",subtitle="By Party") 


#' 
#' ### Plotting Dataset by Gender
#' 
## ----plot_by_gender, fig.show="hold", out.width="85%",  echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------
# Bar Graph - By Gender, eliminating NA's
test_graph <- train_sample %>% filter(Gender != "NA")  %>% dplyr::group_by(Gender) %>% dplyr::summarise(Totals=n())

#sum(test_graph$Totals)

all_votes <- sum(test_graph$Totals)

test_graph %>% filter(Gender != "NA") %>% ggplot(aes(x=Gender,y=Totals,fill=Gender)) + 
    geom_bar(stat="identity") +
    scale_fill_manual(values = c("pink", "steelblue2")) +
    geom_text(aes(x=Gender, y=Totals, label = paste(sprintf("%i/%.1f%%",Totals, (Totals/all_votes) * 100)), 
          vjust=3.5), position = position_dodge(width=0.9)) +
    labs(title="Sample Size",subtitle="By Gender") 


#' 
#' \newpage
#' 
#' ### Plotting Dataset by Income Bands
#' 
## ----plot_by_income_bands, fig.show="hold", out.with="30%", echo=FALSE-------------------------------------------------------------------------------------------------------------------------------
# Bar Graph - By Income Bands

test_graph <- train_sample %>% filter(Income != "NA") %>%
    dplyr::group_by(Income) %>% 
    dplyr::summarize(Totals=n()) %>% 
    filter(Income != "NA")

all_votes <- sum(test_graph$Totals)
min_size <- max(test_graph$Totals) + 50

test_graph %>% 
     mutate(Income = fct_reorder(Income, Totals)) %>% 
     ggplot(aes(x=Income, y=Totals, fill=Totals)) + 
     geom_col() +
     scale_fill_distiller(palette="BuGn") +
     labs(title="Sample Size",subtitle="By Income Brackets") +
     geom_text(aes(x=Income, y=Totals, label = paste(sprintf("%i/%.1f%%",Totals, (Totals/all_votes) * 100 ))), nudge_y = -28.0) +
     coord_flip()


#' 
#' 
#' ### Plotting Dataset by Household Status
#' 
## ----plot_by_household_status, fig.show="hold", out.with="30%", echo=FALSE---------------------------------------------------------------------------------------------------------------------------
# Bar Graph - By Household Status
test_graph <- train_sample %>% filter(HouseholdStatus != "NA") %>%
    dplyr::group_by(HouseholdStatus) %>% 
    dplyr::summarize(Totals=n()) %>% 
    filter(HouseholdStatus != "NA")

all_votes <- sum(test_graph$Totals)
min_size <- max(test_graph$Totals) + 50

test_graph %>% 
     mutate(HouseholdStatus = fct_reorder(HouseholdStatus, Totals)) %>% 
     ggplot(aes(x=HouseholdStatus, y=Totals, fill=Totals)) + 
     geom_col() +
     scale_fill_distiller(palette="Pastel1") +
     labs(title="Sample Size",subtitle="By House Hold Status") +
     geom_text(aes(x=HouseholdStatus, y=Totals, label = paste(sprintf("%i/%.1f%%",Totals, 
               (Totals/all_votes) * 100))), position = position_stack(vjust=.5)) +
     coord_flip()


#' 
#' \newpage
#' 
#' ### Plotting Dataset by Education Levels
#' 
## ----plot_by_education_levels,  fig.show="hold", out.with="30%", echo=FALSE--------------------------------------------------------------------------------------------------------------------------
# Bar Graph - By Education Levels
test_graph <- train_sample %>% filter(EducationLevel != "NA") %>%
    dplyr::group_by(EducationLevel) %>% 
    dplyr::summarize(Totals=n()) %>% 
    filter(EducationLevel != "NA")

all_votes <- sum(test_graph$Totals)
min_size <- max(test_graph$Totals) + 50

test_graph %>% 
     mutate(EducationLevel = fct_reorder(EducationLevel, Totals)) %>% 
     ggplot(aes(x=EducationLevel, y=Totals, fill=Totals)) + 
     geom_col() +
     scale_fill_distiller(palette="OrRd") +
     labs(title="Sample Size",subtitle="By Education Level") +
     geom_text(aes(x=EducationLevel, y=Totals, label = paste(sprintf("%i/%.1f%%",Totals,             
              (Totals/all_votes) *100))), nudge_y = -28.0) +
     coord_flip()

#' 
#' ### Plotting Dataset by Age Distribution
#' 
## ----plot_age_distribution, fig.show="hold", out.with="30%", echo = FALSE, message = FALSE, warning = FALSE, eval = TRUE-----------------------------------------------------------------------------
# Plot: Age Distribution
test_graph <- train_sample %>% filter(YOB != "NA") %>% dplyr::group_by(YOB) %>% dplyr::summarise(Totals=n())
#summary(test_graph)
#dim(test_graph)
max_outlier_year <- max(test_graph$YOB, na.rm=TRUE)     #can't have someone born 2039, it hasn't happened yet
min_outlier_year <- min(test_graph$YOB, na.rm=TRUE)     # can't have someone born 1875 = 135 years old, doesn't align with world facts.

test_graph <- subset(test_graph, YOB != max_outlier_year & YOB != min_outlier_year)  # Eliminating NA and outlier year

#summary(test_graph)
#str(summary(test_graph))
#dim(test_graph)

mean_year <- mean(test_graph$YOB, na.rm=TRUE)
mean_age <- round(mean(test_graph$Totals), digits=2)

str_title <- paste("By Age and Year: mean age =  ",mean_age)
 
# ggplot(test_graph, aes(x=YOB,y=Totals, size=Totals)) + geom_point(ylim=c(0,2013)) + ggtitle(str_title)
# boxplot(test_graph$Totals)
ggplot(test_graph, aes(x=Totals, y=YOB, fill=YOB)) + 
     geom_point(fill="wheat",alpha=0.2) + 
     xlab("Totals") + theme(legend.position="none") +
     labs(title="Sample Size",subtitle=str_title)

#' 
#' 
#' \newpage
#' 
#' 
#' ## Appendix - D: Correlation Matrixes and Heat Maps
#' 
#' ### Correlation Matrix with Original Dataset
#' 
## ----coorelaton_matrix_results, fig.show="hold", out.with="30%",warning = FALSE, message = FALSE,  echo = FALSE--------------------------------------------------------------------------------------
train_sample_num <- data.matrix(train_sample)
#str(train_sample_num)
train_sample_num_cor <- cor(train_sample_num, use="complete.obs")

train_sample_questions_num <- data.matrix(train_sample_questions)
#str(train_sample_questions_num)
train_sample_questions_num_cor <- cor(train_sample_questions_num, use="complete.obs")

col<- colorRampPalette(c("blue", "white", "red"))(20)


#```{r 'plot_corr_tain_sample_num' }


# Created individual chunks so we could reference independently wihtin the report



#Positive correlations are displayed in blue and negative correlations in red color. Color intensity and the size of the circle are #proportional to the correlation coefficients. In the right side of the correlogram, the legend color shows the correlation #coefficients and the corresponding colors.

# chart.Correlation(train_sample_questions_num_cor,historam=TRUE)  - too many to see results

#In the above plot:

#The distribution of each variable is shown on the diagonal.
#On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed
#On the top of the diagonal : the value of the correlation plus the significance level as stars
#Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols(“***”, “**”, “*”, “.”, " “)
corrplot(train_sample_num_cor)
corrplot(train_sample_num_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
#chart.Correlation(train_sample_num_cor,historam=TRUE, pch=19)

#' 
#' \newpage
#' 
#' ### Correlation Matrix with Enhanced Dataset including Survey Questions
#' 
## ----corr_matrix_questions, fig.show="hold", out.with="30%",warning = FALSE, message = FALSE,  echo = FALSE------------------------------------------------------------------------------------------
corrplot(train_sample_questions_num_cor, type = "upper", order = "hclust", tl.col = "black")
#chart.Correlation(train_sample_questions_num_cor,historam=TRUE) 

#' 
#' \newpage
#' 
#' ### Heat Maps for both Datasets
## ----heatmaps,  fig.show="hold", out.with="40%", warning = FALSE, message = FALSE,  echo = FALSE-----------------------------------------------------------------------------------------------------
heatmap(x = train_sample_num_cor, col = col, symm = TRUE)
heatmap(x = train_sample_questions_num_cor, col = col, symm = TRUE)

#' 
#' 
#' \newpage 
#' 
#' ## Appendix - E: References
#' 
#' 01. [Mark Wickham (2018) Practical Jave Machine Learning:  Projects with Google Cloud Platform and Amazon Web Services](www.amazon.com/Practical-Java-Machine-Learning-Projects/dp/1484239504/ref=sr_1_2?dchild=1&keywords=book+practical+java+machine+learning+mark+wickham&qid=1599620820&sr=8-2)
#' 02. [Rafael A. Irizarry (2019), Introduction to Data Science: Data Analysis and Prediction Algorithms with R](www.amazon.com/Introduction-Data-Science-Prediction-Algorithms/dp/0367357984/ref=sr_1_2?dchild=1&keywords=rafael+irizarry+introduction+to+data+science&qid=1599620995&sr=8-2)
#' 03. [Hie,Allaire,Grolemund (2020) R Markdown: The Definitive Guide](www.bookdown.org/yihui/rmarkdown/)
#' 04. [Yixuan Qiu (2017), recosystem: recommendation System Using Parallel Matrix Factorization](wwww.statr.me/2016/07/recommender-system-using-parallel-matrix-factorization/)
#' 05. [Tilman M. Davies (2016), The Book of R: A First Course In Programming and Statistics](www.amazon.com/Book-First-Course-Programming-Statistics/dp/1593276516/ref=asc_df_1593276516/?tag=hyprod-20&linkCode=df0&hvadid=312280575053&hvpos=&hvnetw=g&hvrand=15631778436968807202&hvpone=&hvptwo=&hvqmt=&hvdev=c&hvdvcmdl=&hvlocint=&hvlocphy=1022653&hvtargid=pla-406163956753&psc=1)
#' 06. [Vries, Meys (2015), R For Dummies](www.amazon.com/R-Dummies-Andrie-Vries/dp/1119055806/ref=sr_1_2?dchild=1&keywords=r+for+dummies&qid=1599622198&s=books&sr=1-2)
#' 07. [Alvira Swalin (2018), Choosing the Right Metric for Evaluationg Machine Learning Models - Part 1](www.medium.com/usf-msds/choosing-the-right-metric-for-machine-learning-models-part-1-a99d7d7414e4)
#' 08. [Shervin Minaee (2019), 20 Popular Machine Learning Metrics. Part 1: Classification & Regression Evaluation Metrics](www.towardsdatascience.com/20-popular-machine-learning-metrics-part-1-classification-regression-evaluation-metrics-1ca3e282a2ce)
#' 09. [Georgios Drakos (2018), How to select the Right Evalation Metri for Machine Learning Models:  Part 2 Regression Metrics](www.laptrinhx.com/how-to-select-the-right-evaluation-metric-for-machine-learning-models-part-2-regression-metrics-604272514/)
#' 10. [Machine Learning: Classification Models ](www.medium.com/fuzz/machine-learning-classification-models-3040f71e2529)
#' 11. [Predicting Voting Affiliation Using Machine Learning](www.core.ac.uk/download/pdf/38107056.pdf)
#' 12. [Types of Classification Tasks in Machine Learning](www.machinelearningmastery.com/types-of-classification-in-machine-learning/)
#' 13. [8 Proven Ways for Improving the "Accuracy" of a Machine Learning Model](www.analyticsvidhya.com/blog/2015/12/improve-machine-learning-results/)
#' 14. [What Affects Voter Turnout Rates](www.fairvote.org/what_affects_voter_turnout_rates)
#' 15. [Voter Turnout Demographics](www.electproject.org/home/voter-turnout/demographics)
#' 16. [Voter Turnout](www.//electionlab.mit.edu/research/voter-turnout)
#' 17. [Parametric vs Nonparametric models?](www.//medium.com/@dataakkadian/what-are-parametric-vs-nonparametric-models-8bfa20726f4d)
#' 18. [Classification and Regression Trees for Machine Learning](www.machinelearningmastery.com/classification-and-regression-trees-for-machine-learning/)
#' 19. [Modern Machine Learning Algorithms: Strengths and Weaknesses](www.elitedatascience.com/machine-learning-algorithms)
#' 20. [Pros and cons of common Machine Learning Algorithms](www.medium.com/@gokul.elumalai05/pros-and-cons-of-common-machine-learning-algorithms-45e05423264f)
#' 21. [Machine Learning Algorithms Pros and Cons](www.hackingnote.com/en/machine-learning/algorithms-pros-and-cons)
#' 22. [Advantages and Disadvantages of Cross Validation in Machine Learning](www.theprofessionalspoint.blogspot.com/2019/02/advantages-and-disadvantages-of-cross.html#:~:text=Advantages%20and%20Disadvantages%20of%20Cross%20Validation%20in%20Machine%20Learning,-Cross%20Validation%20in&text=1.,from%20overfitting%20the%20training%20dataset)
#' 23. [Journa of Statistical Software: Feature Selection with the Boruta Package](www.d1wqtxts1xzle7.cloudfront.net/32056178/v36i11.pdf?1381442992=&response-content-disposition=inline%3B+filename%3DFeature_Selection_with_the_Boruta_Packag.pdf&Expires=1600109744&Signature=I8myRwjAsYjbQjmyYCAFGRCj1nng2kCNI6kAQAVMfgahWpdRib62nJkT5kP5x4kIrkOu9uzMBcQaOabi53NES2xykjQyBOth4gGLjYn9ENT9RlaEdujsUxvjaY-jP2R1RFUg3eav9nmH01lT8anT-SKnIzEO9F0FFWQKPCeIp2hWAVssMixFxLZ8SKgRm70r~FQcEWoYJlxJY2Rk47gg6dqs1hL5y4vxt07ZTdSzWE33goQqQlwPwWGDGdk-wuJ5vmYvBOa3AwJXVso0fTawuMBk0EImBbo0f7rOVhX~FDnbfJX6aCqZZUFiqbGkSrtLVRlYvupEIxWemLUv38AklA__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA)
#' 24. [Feature Selection in R with the Boruta R Package](www.datacamp.com/community/tutorials/feature-selection-R-boruta)
#' 25. [bookdown: Authoring Books and Technical Documents with R Markdown](www.bookdown.org/yihui/bookdown/)
#' 26. [A Gentle Introduction to Threshold-Moving for Imbalanced Classification](www.machinelearningmastery.com/threshold-moving-for-imbalanced-classification/)
#' 27. [Linear & Quadratic Discriminant Analysis](www.uc-r.github.io/discriminant_analysis)
#' 28. [How to choose machine learning algorithms](https://medium.com/@aravanshad/how-to-choose-machine-learning-algorithms-9a92a448e0df)
#' 29. [How to Choose a Machine Learning Model - Some Guidelines](www.datasciencecentral.com/profiles/blogs/how-to-choose-a-machine-learning-model-some-guidelines)
#' 30. [An easy guide to choose the right Machine Learning Algorithm](www.kdnuggets.com/2020/05/guide-choose-right-machine-learning-algorithm.html)
#' 31. [How to Read a Confusion Matrix](https://honingds.com/blog/confusion-matrix-in-r/)
#' 32. [An Introduction to Statistical Learning](www.faculty.marshall.usc.edu/gareth-james/ISL/ISLR%20Seventh%20Printing.pdf)
#' 33. [Using a Neural Network to Predict Voter Preferences](https://towardsdatascience.com/using-a-neural-network-to-predict-voter-preferences-ccb9122a6df1)
#' 34. [Ensemble Learning to Improve Machine Learning Results](https://blog.statsbot.co/ensemble-learning-d1dcd548e936)
#' 35. [Train-Test Split for Evaluating Machine Learning Algorithms](https://machinelearningmastery.com/train-test-split-for-evaluating-machine-learning-algorithms/)
#' 36. [A Scaling law for validation-set training-set size ratio](https://stackoverflow.com/questions/13610074/is-there-a-rule-of-thumb-for-how-to-divide-a-dataset-into-training-and-validatio)
#' 37. [Linear Discriminant Analysis vs Random Forests](https://maths-people.anu.edu.au/~johnm/courses/r/exercises/pdf/r-exercisesXII-XVI.pdf)
#' 38. [Beginner’s Guide to LDA Topic Modelling with R](https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25)
#' 39. [Evaluation of Classification Model Accuracy: Essentials](http://www.sthda.com/english/articles/36-classification-methods-essentials/143-evaluation-of-classification-model-accuracy-essentials/)
#' 40. [Forecasting the 2015 General Election with Internet Big Data: An Application of the TRUST Framework](https://www.gla.ac.uk/media/Media_440128_smxx.pdf)
#' 
#' 
#' \newpage 
#' 
#' ## Appendix - F: Peer Assignment Grading Requirements
#' Grading (Rubric to be used by Peers)
#' 
#' Files (5 Points possible): Files Requirements 3 files: R script, RMD, and .pdf must be submitted.: 
#' - Files Points (5 points possible): Files Requirements: 
#'  
#'       - 0 points: No files provided AND/OR the files provided appear to violate the edX Honor Code.
#'       - 3 points: One file is missing and/or not in the correct format.
#'       - 5 points: All 3 files were submitted in the requested formats.
#' 
#' Report (25 points possible) : Report Requirements: 
#' 
#'       - Documents the analysis and presents findings, Contains supporting statistics and figures
#'       - Written in English and must include the following (at a minimum sections)
#'           - Introduction/Overview/Executive Summary: 
#'                 Describes the dataset and summarizes the goal of the project and key steps performed
#'           -  Methods/Analysis
#'                 Explains the process and techniquess used including: data cleaning, 
#'                 data exploration and visualization, insights gained, and modeling approach
#'           -  Results: Presents modeling results and discusses the model performance
#'           -  Conclusion: Brief summary of the report, its limitations and future work
#' 
#' - Report Points:
#'      -  0 points: The report is either not uploaded or contains very minimal information AND/OR the report appears                                     to violate the edX Honor Code.
#'      -  5 points: Multiple required sections of the report are missing.
#'      - 10  points:The report includes all required sections, but the report is significantly difficult to follow or                                    missing supporting detail in multiple sections.
#'      - 15 points: The report includes all required sections, but the report is difficult to follow or missing                                          supporting detail in one section.
#'      - 20 points: The report includes all required sections and is easy to follow, but with minor flaws in one section.
#'      - 25 points: The report includes all required sections, is easy to follow with good supporting detail throughout,                                 and is insightful and innovative. 
#' 
#' Code (20 points): Code Requirements-  Code should be well commented and easy to follow
#' 
#' - Code Points
#'      -  0 points: Code does not run and produces many errors or the code appears to violate the edX Honor Code.
#'      -  5 points: Code runs but does not produce output consistent with what is presented in the report OR there is                                    overtraining (the test set is used for training steps).
#'      -  10 points:Code runs but is difficult to follow and/or may not produce output entirely consistent with what is presented in                     the report. 
#'      -  15 points: Code runs, can be followed, is at least mostly consistent with the report, but is lacking (sufficient) comments                     and explanation OR uses absolute paths instead of relative paths OR does not automatically install missing packages                   OR does not provide easy access to the dataset (either via automatic download or inclusion in a GitHub repository).
#'      -  20 points: Code runs easily, is easy to follow, is consistent with the report, and is well-commented. All file paths are                       relative and missing packages are automatically installed with if(!require) statements.
#'      
#' 
#' 
#' \newpage
#' ## Appendix - H: List of tables
#' \renewcommand{\listtablename}{} <!-- removes default section name -->
#' \listoftables
#' 
#' \newpage
#' ## Appendix - J: List of figures
#' \renewcommand{\listfigurename}{}
#' \listoffigures
#' 
#' \newpage
#' 
#' ## Appendix - G: Survey Questions
## ----including_quesitons_1, out.width="80%", fig.align="default", fig.show="hold",fig.cap="Survey - Election Questions - Page 1", warning = FALSE, message = FALSE,  echo = FALSE--------------------
#\includepdf[pages=-]{./ElectionData/Questions.pdf}
#\captionof{figure}{"Survey - Election Questions"}
#\includepdf[pages=-]{./ElectionData/Questions.pdf}
#fig.cap="Survey - Election Questions"
# The above didn't work 

#NOTE:  Do to the limitations of include_graphics() for .pdf only including the 1st page
#       We have split thhis file into 2 graphics images. 
# knitr::include_graphics("./ElectionData/Questions.pdf")
   knitr::include_graphics("./ElectionData/Questions-page1.png")

#' 
#' 
#' \newpage
#' 
## ----including_quesitons_2, out.width="100%", fig.align="default", fig.show="hold", fig.cap="Survey - Election Questions - Page 2", warning = FALSE, message = FALSE,  echo = FALSE------------------
#\includepdf[pages=-]{./ElectionData/Questions.pdf}
#\captionof{figure}{"Survey - Election Questions"}
#\includepdf[pages=-]{./ElectionData/Questions.pdf}
#fig.cap="Survey - Election Questions"
# The above didn't work 

#NOTE:  Do to the limitations of include_graphics() for .pdf only including the 1st page
#       We have split thhis file into 2 graphics images. 
# knitr::include_graphics("./ElectionData/Questions.pdf")
   knitr::include_graphics("./ElectionData/Questions-page2.png")

#' 
## ----things_to_do_before_exiting, echo=FALSE, , message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------
func_project_clean_up()

#' 
#' 
#' 
#' 
#' 
#' 
#' 
