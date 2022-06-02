library(dplyr)
library(mice)
library(ggplot2)
library(car)
library(pROC)
library(cvTools)


hr_train = read.csv('E:/Edvancer/R programming/HR/hr_train.csv')
hr_test = read.csv('E:/Edvancer/R programming/HR/hr_test.csv')

hr_train$data = 'train'
hr_test$data = 'test'

hr_test$left = NA

hr_all = rbind(hr_train,hr_test)

glimpse(hr_all)

hr_all = hr_all %>% 
  mutate(s_acc = as.numeric(sales=='accounting'),
         s_hr = as.numeric(sales=='hr'),                     s_IT = as.numeric(sales=='IT'),                     s_man = as.numeric(sales=='management'), 
         s_mrkt = as.numeric(sales=='marketing'),
         s_pm = as.numeric(sales=='product_mng'),
         s_rd = as.numeric(sales=='RandD'),
         s_sales = as.numeric(sales=='sales'),
         s_support = as.numeric(sales=='support'),
         s_tech = as.numeric(sales=='technical')) %>% select (-sales)

hr_all = hr_all %>% mutate(salary_l = as.numeric(salary == 'low'),salary_m = as.numeric(salary == 'medium'), salary_h = as.numeric(salary == 'high')) %>%
  select(-salary)

hr_train = hr_all %>% filter(data == 'train') %>%
  select(-data)
hr_test = hr_all %>% filter(data == 'test') %>%
  select(-data)

set.seed(9)
s = sample(nrow(hr_train),0.7*nrow(hr_train))
hr_train1 = hr_train[s,]
hr_train2 = hr_train[-s,]


#Logistic regression model

lm = lm(left ~. - s_tech - salary_h, data = hr_train)

fit = glm(left ~. - s_tech - salary_h, data = hr_train1, family = 'binomial')
summary(fit)

step = step(fit)
form = formula(step)

predict.train1 = predict(step,hr_train2,type = 'response')
predict.train1

auc(roc(hr_train2$left,predict.train1))

# Area under the curve: 0.7246



#random forest model

library(randomForest)
library(caret)

rf = randomForest(left~.,data = hr_train1)

rf.predict.train1 = predict(rf,hr_train2,type = 'response')
auc(roc(hr_train2$left,rf.predict.train1))

#Area under the curve: 0.8261

# hypertuning

param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))
expand.grid(param)

## Function for selecting random subset of params

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trials=50
my_params=subset_paras(param,num_trials)

myerror=9999999

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,left~.,
             data =hr_train1,
             tuning =params,
             folds = cvFolds(nrow(hr_train1), K=10, type = "random"),
             seed =2
  )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    # print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
    # print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  # print('DONE')
  # uncomment the line above to keep track of progress
}


rf.hr_train1 = randomForest(left~.,
                        mtry=best_params$mtry,
                        ntree=best_params$ntree,
                        maxnodes = best_params$maxnodes,
                        nodesize=best_params$nodesize,data = hr_train1)

rf.hr_train2 = predict(rf.hr_train1, hr_train2, type = 'response')

auc(roc(hr_train2$left,rf.hr_train2,))

# Area under the curve: 0.8274



# model on whole hr_train set


rf.final = randomForest(left~.,
                        mtry=best_params$mtry,
                        ntree=best_params$ntree,
                        maxnodes = best_params$maxnodes,
                        nodesize=best_params$nodesize,data = hr_train)

rf.final.predict = predict(rf.final, hr_test, type = 'response')

write.csv(rf.final.predict,'Hardik_Fumakiya_P4_Part2.csv' )


















