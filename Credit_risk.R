library("tidyverse")
library("psych")
library("car")
library("corrplot")
library("caret")
library("caTools")
library("pastecs")
library("ROCR")
library("cluster")
library("factoextra")
library("DataExplorer")
library("funModeling")
library("Hmisc")
library("knitr")
library("missForest")
library("mice")
library("VIM")
library("Amelia")
library("corrr")
library("rsconnect")
library("shiny")
library("dplyr")
library("tm")
library("RColorBrewer")
library("wordcloud")
library("choroplethrMaps")
library("ggthemes")
library("DT")

#########################################################
loan = read.csv("loan.csv")
# View(loan)
options(scipen = 999)
# summary(loan)
{
loan$inq_last_12m = NULL
loan$total_cu_tl = NULL
loan$inq_fi = NULL
loan$total_rev_hi_lim = NULL
loan$all_util = NULL
loan$max_bal_bc = NULL
loan$open_rv_24m = NULL
loan$open_rv_12m = NULL
loan$il_util = NULL
loan$total_bal_il = NULL
loan$mths_since_rcnt_il = NULL
loan$open_il_24m = NULL
loan$open_il_12m = NULL
loan$open_il_6m = NULL
loan$open_acc_6m = NULL
loan$dti_joint = NULL
loan$annual_inc_joint = NULL
loan$mths_since_last_major_derog = NULL
loan$mths_since_last_record = NULL
loan$mths_since_last_delinq = NULL
}
# View(loan)

dim(loan)

# summary(loan)

#################

loan$issue_d = as.Date(gsub("^", "01-", loan$issue_d), format="%d-%b-%Y")

amnt_df = loan %>% 
  select(issue_d, loan_amnt) %>% 
  group_by(issue_d) %>% 
  summarise(Amount = sum(loan_amnt))

ts_amnt = ggplot(amnt_df, aes(x = issue_d, y = Amount)) + geom_line() + xlab("Date issued")
ts_amnt
#################

box_status = ggplot(loan, aes(loan_status, loan_amnt))
box_status + geom_boxplot(aes(fill = loan_status)) +
  theme(axis.text.x = element_blank()) +
  labs(list(
    title = "Loan amount by status",
    x = "Status",
    y = "Amount"))

#################
amnt_df_grade = loan %>% 
  select(issue_d, loan_amnt, grade) %>% 
  group_by(issue_d, grade) %>% 
  summarise(Amount = sum(loan_amnt))


ts_amnt_grade = ggplot(amnt_df_grade, 
                        aes(x = issue_d, y = Amount))
ts_amnt_grade + geom_area(aes(fill=grade)) + xlab("Date issued")

#################
ggplot(loan, aes(purpose, fill = purpose)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#################
ggplot(data=loan, aes(loan_amnt))+geom_histogram(bins=40,color="blue",fill="cyan")
ggplot(data=loan, aes(loan_amnt))+geom_density(color="blue",fill="cyan",alpha=0.2)
ggplot(data=loan,aes(loan_amnt, col=grade))+geom_histogram(bins=40)+facet_grid(grade ~ .)
ggplot(data=loan,aes(loan_amnt, fill=grade))+geom_density(alpha=0.25)+facet_grid(grade ~ .)
ggplot(loan, aes(x=grade, y=loan_amnt, fill=grade))+stat_summary(fun.y="sum", geom="bar")+labs(y ="Total Loan Amount",title="Total loan amount based on loan grade")
ggplot(data=loan, aes(home_ownership,loan_amnt,fill=home_ownership))+geom_boxplot(outlier.color = "blue")+labs(title="Box plot of loan amount")



#################
loan_1 = loan %>%
  select(loan_status , loan_amnt , int_rate , grade , emp_length , home_ownership , 
         annual_inc , term)

sapply(loan_1 , function(x) sum(is.na(x)))

loan_1 = loan_1 %>%
  filter(!is.na(annual_inc) , 
         !(home_ownership %in% c('NONE' , 'ANY')) , 
         emp_length != 'n/a')

loan_1 %>%
  count(loan_status) %>%
  ggplot(aes(x = reorder(loan_status , desc(n)) , y = n , fill = n)) + 
  geom_col() + 
  coord_flip() + 
  labs(x = 'Loan Status' , y = 'Count')


#################
loan_2 = loan_1 %>%
  mutate(loan_outcome = ifelse(loan_status %in% c('Charged Off' , 'Default') , 
                               1, 
                               ifelse(loan_status == 'Fully Paid' , 0 , 'No info')
  ))

barplot(table(loan_2$loan_outcome) , col = 'lightblue')
#################
# Create the new dataset by filtering 0's and 1's in the loan_outcome column and remove loan_status column for the modelling
loan_3 = loan_2 %>%
  select(-loan_status) %>%
  filter(loan_outcome %in% c(0 , 1))
#################

ggplot(loan_3 , aes(x = grade , y = int_rate , fill = grade)) + 
  geom_boxplot() + theme_igray() + labs(y = 'Interest Rate' , x = 'Grade')
#################
table(loan_3$grade , factor(loan_3$loan_outcome , c(0 , 1) , c('Fully Paid' , 'Default')))

ggplot(loan_3 , aes(x = grade , y = ..count.. , fill = factor(loan_outcome , c(1 , 0) , c('Default' , 'Fully Paid')))) + 
  geom_bar() + 
  theme(legend.title = element_blank())
################
ggplot(loan_3[sample(244179 , 10000) , ] , aes(x = annual_inc , y = loan_amnt , color = int_rate)) +
  geom_point(alpha = 0.5 , size = 1.5) + 
  geom_smooth(se = F , color = 'darkred' , method = 'loess') +
  xlim(c(0 , 300000)) + 
  labs(x = 'Annual Income' , y = 'Loan Ammount' , color = 'Interest Rate')
#################








