setwd("D:\\prac")
#install.packages("data.table")
#install.packages("caret")
#install.packages("corrplot")
#install.packages("xgboost")
#install.packages("cowplot")
library(cowplot)
library(dplyr)
library(data.table)
library(ggplot2)
library(caret)
library(corrplot)
library(xgboost)

train = fread("train.csv")
test = fread("test.csv")
submission = fread("sample.csv")

dim(train); dim(test)

names(train)
names(test)

str(train)
str(test)

ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales),binwidth = 100, fill = "darkgreen") +
  xlab("Item_Outlet_Sales")

ggplot(train) + geom_histogram(aes(train$Item_Weight),binwidth = 0.2, fill = "darkgreen") +
  xlab("Item_Weight")

ggplot(train) + geom_histogram(aes(train$Item_Visibility ),binwidth = 0.009, fill = "darkgreen") +
  xlab("Item_Visibility ")

ggplot(train) + geom_histogram(aes(train$Item_MRP ), binwidth = 1, fill = "darkgreen") +
  xlab("Item_MRP ")

test[,Item_Outlet_Sales := NA] 
combi = rbind(train, test) # combining train and test datasets 




p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue") 
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue") 
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue") 
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +   
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +   
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

# plot for Item_Type 
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) +   
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +  
  xlab("") +  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  
  ggtitle("Item_Type")
p4

# plot for Outlet_Identifier 
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +   
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p5

# plot for Outlet_Size 
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +   
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p6

# plot for Outlet_Establishment_Year 
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) +   
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +  
  xlab("Outlet_Establishment_Year") +  
  theme(axis.text.x = element_text(size = 8.5))
p7

# plot for Outlet_Type 
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +   
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(size = 8.5))
p8

train = combi[1:nrow(train)] # extracting train data from the combined data

# Item_Weight vs Item_Outlet_Sales 
p9 = ggplot(train) +      
  geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "blue", alpha = 0.3) +     
  theme(axis.title = element_text(size = 8.5))
p9

# Item_Visibility vs Item_Outlet_Sales 
p10 = ggplot(train) +       
  geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "blue", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))
p10

# Item_MRP vs Item_Outlet_Sales 
p11 = ggplot(train) +       
  geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "blue", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))
p11

# Item_Type vs Item_Outlet_Sales 
p12 = ggplot(train) +       
  geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +      
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 6), axis.title = element_text(size = 8.5))
p12
# Item_Fat_Content vs Item_Outlet_Sales 
p13 = ggplot(train) +       
  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +      
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 8), axis.title = element_text(size = 8.5))
p13

# Outlet_Identifier vs Item_Outlet_Sales 
p14 = ggplot(train) +       
  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +      
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 8),  axis.title = element_text(size = 8.5))
p14

ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

p15 = ggplot(train) + 
  geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta") 
p15

p16 = ggplot(train) + 
  geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta") 
p16
plot_grid(p15, p16, ncol = 1)


########################### mising value treatment #############################################

sum(is.na(combi$Item_Weight))

missing_index = which(is.na(combi$Item_Weight)) 
for(i in missing_index) { item = combi$Item_Identifier[i]  
combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T) }

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)


zero_index = which(combi$Item_Visibility == 0) 
for(i in zero_index){ item = combi$Item_Identifier[i]  
combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)  }

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

###################################### Feature Engineering ####################################

perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")

non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

# create a new feature 'Item_Type_new' 
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]


table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))


combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]

combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible" 
# years of operation for outlets 
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year] 
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year) 
# Price per unit weight 
combi[,price_per_unit_wt := Item_MRP/Item_Weight]

# creating new independent variable - Item_MRP_clusters 
combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st", ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd", ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]

#################################### Encoding categorical variables ##########################

combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0, ifelse(Outlet_Size == "Medium", 1, 2))] 

combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0, ifelse(Outlet_Location_Type == "Tier 2", 1, 2))] 

# removing categorical variables after label encoding 

combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]


ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T) 
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")])) 
combi = cbind(combi[,"Item_Identifier"], ohe_df)


############################### reducing skewness in data ####################################

combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero 
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]


################################# scaling and centering numeric variables ###################

num_vars = which(sapply(combi, is.numeric)) # index of numeric features 
num_vars_names = names(num_vars) 
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F] 
prep_num = preProcess(combi_numeric, method=c("center", "scale")) 
combi_numeric_norm = predict(prep_num, combi_numeric)
combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables 
combi = cbind(combi, combi_numeric_norm)



####################### splitting combined data into train and test #########################

train = combi[1:nrow(train)] 
test = combi[(nrow(train) + 1):nrow(combi)] 
test[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA for test dataset


########################## checking for correlated variabls ###############################

cor_train = cor(train[,-c("Item_Identifier")]) 
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)



######################### model building process #########################################

linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])


# preparing dataframe for submission and writing it in a csv file 
submission$Item_Outlet_Sales = predict(linear_reg_mod, test[,-c("Item_Identifier")]) 
write.csv(submission, "Linear_Reg_submit.csv", row.names = F)

######################## Regularised Regression ######################################

set.seed(1235) 
my_control = trainControl(method="cv", number=5) 
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002)) 
lasso_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales, method='glmnet', trControl= my_control, tuneGrid = Grid)


############################### Ridge Regression ####################################

set.seed(1236) 
my_control = trainControl(method="cv", number=5) 
Grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002)) 
ridge_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,method='glmnet', trControl= my_control, tuneGrid = Grid)


################################ Random Forest #######################################3

set.seed(1237) 
my_control = trainControl(method="cv", number=5) # 5-fold CV 
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
)
rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
               y = train$Item_Outlet_Sales,
               method='ranger',
               trControl= my_control,
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")


plot(rf_mod)


plot(varImp(rf_mod))

############################## XGBOOST Algorithm ##############################

param_list = list(objective = "reg:linear", eta=0.01, gamma = 1, max_depth=6, subsample=0.8, colsample_bytree=0.5)

dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]), label= train$Item_Outlet_Sales) 
dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))



set.seed(112) 
xgbcv = xgb.cv(params = param_list, data = dtrain, nrounds = 1000, nfold = 5, print_every_n = 10, early_stopping_rounds = 30, maximize = F)

xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 447)



var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")), model = xgb_model) 
xgb.plot.importance(var_imp)




