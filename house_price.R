data <- read.csv("complete.csv")
data1 <-data
summary(data)
real_data <- read.csv("train.csv")

#Remove the coloums 

data1 = subset(data1, select =  -c(Alley,Utilities,BsmtCond,CentralAir,GarageQual,GarageCond,PavedDrive,PoolQC,MiscFeature,LandSlope,LandContour,Street,Condition2,Condition1,RoofMatl,ExterCond,Heating,SaleCondition,SaleType))
View(data1)
data1 = subset(data1, select =  -c(Functional))
data1 = subset(data1, select = -c(Fence))

summary(data1)

# Replace NA with NP

removeNA = function(column) {
  levels <- levels(column)
  levels[length(levels) + 1] <- "NP"
  
  column <- factor(column, levels = levels)
  column[is.na(column)] <- "NP"
  return(column)
}
data1$BsmtQual = removeNA(data1$BsmtQual)
data1$BsmtExposure = removeNA(data1$BsmtExposure)
data1$BsmtFinType1 = removeNA(data1$BsmtFinType1)
data1$BsmtFinType2 = removeNA(data1$BsmtFinType2)
data1$FireplaceQu = removeNA(data1$FireplaceQu)
data1$GarageType = removeNA(data1$GarageType)
data1$GarageFinish = removeNA(data1$GarageFinish)

summary(data1)

# Remove NA values 

mode = function(col){
  mod = names(which.max(table(col)))
  return(mod)
}

data1$MSZoning[is.na(data1$MSZoning)]= mode(data1$MSZoning)
data1$Exterior1st[is.na(data1$Exterior1st)]= mode(data1$Exterior1st)
data1$Exterior2nd[is.na(data1$Exterior2nd)]= mode(data1$Exterior2nd)
data1$MasVnrType[is.na(data1$MasVnrType)]= mode(data1$MasVnrType)
data1$KitchenQual[is.na(data1$KitchenQual)]= mode(data1$KitchenQual)
data1$Electrical[is.na(data1$Electrical)]= mode(data1$Electrical)

summary(data1)
class(data1$GarageArea)

data1$LotFrontage[is.na(data1$LotFrontage)] <- mean(data1$LotFrontage,na.rm = T)
data1$MasVnrArea[is.na(data1$MasVnrArea)] <- mean(data1$MasVnrArea,na.rm = T)
data1$BsmtFinSF1[is.na(data1$BsmtFinSF1)] <- mean(data1$BsmtFinSF1,na.rm = T)
data1$BsmtFinSF2[is.na(data1$BsmtFinSF2)] <- mean(data1$BsmtFinSF2,na.rm = T)
data1$BsmtUnfSF[is.na(data1$BsmtUnfSF)] <- mean(data1$BsmtUnfSF,na.rm = T)
data1$TotalBsmtSF[is.na(data1$TotalBsmtSF)] <- mean(data1$TotalBsmtSF,na.rm = T)
data1$BsmtFullBath[is.na(data1$BsmtFullBath)] <- mean(data1$BsmtFullBath,na.rm = T)
data1$BsmtHalfBath[is.na(data1$BsmtHalfBath)] <- mean(data1$BsmtHalfBath,na.rm = T)
data1$GarageYrBlt[is.na(data1$GarageYrBlt)] <- mean(data1$GarageYrBlt,na.rm = T)
data1$GarageCars[is.na(data1$GarageCars)] <- mean(data1$GarageCars,na.rm = T)
data1$GarageArea[is.na(data1$GarageArea)] <- mean(data1$GarageArea,na.rm = T)

#train and test data

train = data1[1:1460,]
test = data1[1461:2919,]
sales_price = real_data$SalePrice
train = cbind(train,sales_price)
summary(train)
summary(real_data)


#mdel


summary(linear_mod)

library(caTools)
vif(linear_mod)

linear_mod = lm(train$sales_price ~.,data = train[,!colnames(train) %in% c("MSZoning", "LotShape", "LotConfig", "Neighborhood", "RoofStyle", "Exterior2nd",
                                                                           "Exterior1st", "MasVnrType", "Foundation", "BsmtFinType1","BsmtFinSF1","BsmtFinType2",
                                                                           "BsmtFinSF2","HeatingQCFa","BsmtQualNP","GarageTypeBasment","GarageTypeBuiltIn",
                                                                           "GarageTypeCarPort","EnclosedPorch","X3SsnPorch","MoSold","YrSold","PoolArea","GarageArea",
                                                                           "GarageFinishRFn","GarageYrBlt","GarageYrBlt","GarageFinishRFn","GarageFinishUnf","GarageTypeCarPort",
                                                                           "FireplaceQuPo","LowQualFinSF","ElectricalMix","ElectricalFuseP","ElectricalFuseF","ElectricalSBrkr")])
summary(linear_mod)




#predit the value 
result = predict(linear_mod,test)
write.csv(result, file = "Result.csv")

