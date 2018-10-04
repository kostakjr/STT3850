library(readxl)
library(dplyr)
library(ggplot2)


DF <- read_excel("TMP.xlsx")

#fix errors in the data set created by excel
DF <- DF %>%
      mutate(Age_Cohort = gsub("12-Jun", "6-12", Age_Cohort))

DF <- DF %>%
  mutate(Age_Cohort = gsub("42898", "6-12", Age_Cohort))

DF <- DF %>%
  mutate(Age_Cohort = gsub("0 - 5", "0-5", Age_Cohort))

DF


#start finding answers for the questions on the assignment
DF %>%
  filter(Gender == "Male") %>%
  summarize(MeanMaleExpenditures = mean(Expenditures))

male <- 18001

DF %>%
  filter(Ethnicity == "Hispanic") %>%
    summarize(MeanHispanicExpenditures = mean(Expenditures))

hispanic <- 11066

DF %>%
  filter(Age_Cohort == "22-50") %>%
  summarize(Mean22to50Expenditures = mean(Expenditures))

twentytwotofifty <- 40209

DF %>%
  filter(Age_Cohort == "22-50", Ethnicity == "White not Hispanic", Gender == "Male") %>%
  summarize(MeanMW22to50Expenditures = mean(Expenditures))

whiteMale22to50 <- 38604

DF %>%
  filter(Age_Cohort == "22-50", Ethnicity == "Asian") %>%
  summarize(MeanAsian22to50Expenditures = mean(Expenditures))

asian22tofifty <- 39581


#make a dataframe from the results and turn dataframe into a bar chart
bars <- data.frame(Catagory = c("Male","Hispanic","22-50", "White Male 22-50", "Asian 22-50"), 
                  values = c(male, hispanic, twentytwotofifty, whiteMale22to50, asian22tofifty))

ggplot(bars, aes(x=Catagory, weight=values)) + 
  geom_bar() +
  labs(x = "Catagory", y = "Mean Expenditures", title = "Average Expenditures")





#instructor example of grouping by gender and piping into ggplot
DF %>% 
  group_by(Gender) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n()) %>%
  ggplot(aes(x = Gender, y= ME, fill = Gender)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Average Expenditure by Gender", y = "Mean Expenditure") + 
  theme_bw() + 
  scale_fill_manual(values = c("pink", "blue"))

#instructor example of grouping by ethnicity and piping into ggplot
DF %>% 
  group_by(Ethnicity) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n()) %>%  
  ggplot(aes(x = reorder(Ethnicity, ME), y = ME)) +
  geom_bar(stat="identity", fill = "red") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  labs(x = "", y = "Mean Expenditure", title = "Average Expenditure by Ethnicity")

