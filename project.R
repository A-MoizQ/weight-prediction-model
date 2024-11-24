#PROBABILITY AND STATISTICS ASSIGNMENT # 4
#Member 1: Abdul Moiz 
#Member 2: Rehan Tariq 

library(readxl)
file_path = "./data_set.xlsx"
#read data from excel
data <- read_excel(file_path,sheet = "Sheet1", range = "A6:G54")
head(data)
#clean column names for MLRM
colnames(data) <- make.names(colnames(data))
head(data)
#create mlrm of entire data
MLRM = lm(Weight ~ Working.Hours + Average.weights.of.both.parents + Responsibilities + Stress.Level + Binge.Eating.Frequency.in.last.Week + Height.cm., data = data)
#check summary and significance
summary(MLRM)
#clean the data to only significant columns
cleaned_data = data[,c("Weight","Average.weights.of.both.parents","Stress.Level","Working.Hours")]
head(cleaned_data)
#create mlrm of significant columns
cleaned_MLRM = lm(Weight ~ Average.weights.of.both.parents + Stress.Level + Working.Hours,data = cleaned_data)
#check summary of significant columns
summary(cleaned_MLRM)
#check coefficents of significant variables
coefficients(cleaned_MLRM)
#create scatter plot
pairs(cleaned_data,main = "Scatter Plot for all variables",pch = 21)
#create boxplot
boxplot(cleaned_data, main = "Box and whisker plot of all variables", xlab = "values", ylab = "variables", names = c("Weight", "Avg Parents Wt", "Stress", "Work Hrs"),col = rainbow(ncol(cleaned_data)), pch = 20,horizontal = TRUE)
