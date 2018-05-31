library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

Training_data <- read.csv("Train_UWu5bXk.csv")
Test_data <- read.csv("Test_u94Q5KV.csv")

x1 <- rep(0, length=nrow(Training_data))
for (i in 1:nrow(Training_data)) {
if (Training_data[i, 3] == "LF" | Training_data[i, 3] == "low fat" | Training_data[i, 3] == "Low Fat") { x1[i] = 0 }
else if (Training_data[i, 3] == "reg" | Training_data[i, 3] == "Regular") { x1[i] = 1 }
else { x1[i] = 0 } }
Training_data[, "Item_Fat_Content_factor"] <- x1

x1_2 <- rep(0, length=nrow(Test_data))
for (i in 1:nrow(Test_data)) {
if (Test_data[i, 3] == "LF" | Test_data[i, 3] == "low fat" | Test_data[i, 3] == "Low Fat") { x1_2[i] = 0 }
else if (Test_data[i, 3] == "reg" | Test_data[i, 3] == "Regular") { x1_2[i] = 1 }
else { x1_2[i] = 0 } }
Test_data[, "Item_Fat_Content_factor"] <- x1_2

x2 <- rep(0, length=nrow(Training_data))
for (i in 1:nrow(Training_data)) {
if (Training_data[i, 5] == "Baking Goods") { x2[i] = 0}
else if (Training_data[i, 5] == "Breads") { x2[i] = 1}
else if (Training_data[i, 5] == "Breakfast") { x2[i] = 2}
else if (Training_data[i, 5] == "Canned") { x2[i] = 3}
else if (Training_data[i, 5] == "Dairy") { x2[i] = 4}
else if (Training_data[i, 5] == "Frozen Foods") { x2[i] = 5}
else if (Training_data[i, 5] == "Fruits and Vegetables") { x2[i] = 6}
else if (Training_data[i, 5] == "Hard Drinks") { x2[i] = 7}
else if (Training_data[i, 5] == "Health and Hygiene") { x2[i] = 8}
else if (Training_data[i, 5] == "Household") { x2[i] = 9}
else if (Training_data[i, 5] == "Meat") { x2[i] = 10}
else if (Training_data[i, 5] == "Others") { x2[i] = 11}
else if (Training_data[i, 5] == "Seafood") { x2[i] = 12}
else if (Training_data[i, 5] == "Snack Foods") { x2[i] = 13}
else if (Training_data[i, 5] == "Soft Drinks") { x2[i] = 14}
else if (Training_data[i, 5] == "Starchy Foods") { x2[i] = 15}
else { x2 = 0} }
Training_data[, "Item_Type_factor"] <- x2

x2_2 <- rep(0, length=nrow(Test_data))
for (i in 1:nrow(Test_data)) {
if (Test_data[i, 5] == "Baking Goods") { x2_2[i] = 0}
else if (Test_data[i, 5] == "Breads") { x2_2[i] = 1}
else if (Test_data[i, 5] == "Breakfast") { x2_2[i] = 2}
else if (Test_data[i, 5] == "Canned") { x2_2[i] = 3}
else if (Test_data[i, 5] == "Dairy") { x2_2[i] = 4}
else if (Test_data[i, 5] == "Frozen Foods") { x2_2[i] = 5}
else if (Test_data[i, 5] == "Fruits and Vegetables") { x2_2[i] = 6}
else if (Test_data[i, 5] == "Hard Drinks") { x2_2[i] = 7}
else if (Test_data[i, 5] == "Health and Hygiene") { x2_2[i] = 8}
else if (Test_data[i, 5] == "Household") { x2_2[i] = 9}
else if (Test_data[i, 5] == "Meat") { x2_2[i] = 10}
else if (Test_data[i, 5] == "Others") { x2_2[i] = 11}
else if (Test_data[i, 5] == "Seafood") { x2_2[i] = 12}
else if (Test_data[i, 5] == "Snack Foods") { x2_2[i] = 13}
else if (Test_data[i, 5] == "Soft Drinks") { x2_2[i] = 14}
else if (Test_data[i, 5] == "Starchy Foods") { x2_2[i] = 15}
else { x2_2 = 0} }
Test_data[, "Item_Type_factor"] <- x2_2

x3 <- rep(0, length = nrow(Training_data))
for (i in 1:nrow(Training_data)) {
if (Training_data[i, 7] == "OUT010") { x3[i] = 0 }
else if (Training_data[i, 7] == "OUT013") { x3[i] = 1 }
else if (Training_data[i, 7] == "OUT017") { x3[i] = 2 }
else if (Training_data[i, 7] == "OUT018") { x3[i] = 3 }
else if (Training_data[i, 7] == "OUT019") { x3[i] = 4 }
else if (Training_data[i, 7] == "OUT027") { x3[i] = 5 }
else if (Training_data[i, 7] == "OUT035") { x3[i] = 6 }
else if (Training_data[i, 7] == "OUT045") { x3[i] = 7 }
else if (Training_data[i, 7] == "OUT046") { x3[i] = 8 }
else if (Training_data[i, 7] == "OUT049") { x3[i] = 9 }
else { x3[i] = 0} }
Training_data[, "Outlet_Identifier_factor"] <- x3

x3_2 <- rep(0, length = nrow(Test_data))
for (i in 1:nrow(Test_data)) {
if (Training_data[i, 7] == "OUT010") { x3_2[i] = 0 }
else if (Test_data[i, 7] == "OUT013") { x3_2[i] = 1 }
else if (Test_data[i, 7] == "OUT017") { x3_2[i] = 2 }
else if (Test_data[i, 7] == "OUT018") { x3_2[i] = 3 }
else if (Test_data[i, 7] == "OUT019") { x3_2[i] = 4 }
else if (Test_data[i, 7] == "OUT027") { x3_2[i] = 5 }
else if (Test_data[i, 7] == "OUT035") { x3_2[i] = 6 }
else if (Test_data[i, 7] == "OUT045") { x3_2[i] = 7 }
else if (Test_data[i, 7] == "OUT046") { x3_2[i] = 8 }
else if (Test_data[i, 7] == "OUT049") { x3_2[i] = 9 }
else { x3_2[i] = 0} }
Test_data[, "Outlet_Identifier_factor"] <- x3_2

x4 <- rep(0, length=nrow(Training_data))
for (i in 1:nrow(Training_data)) {
if (Training_data[i, 9] == "High") { x4[i] = 0 }
else if (Training_data[i, 9] == "Medium") { x4[i] = 1 }
else if (Training_data[i, 9] == "Small") { x4[i] = 2 }
else { x4[i] = 0} }
Training_data[, "Outlet_Size_factor"] <- x4

x4_2 <- rep(0, length=nrow(Test_data))
for (i in 1:nrow(Test_data)) {
if (Test_data[i, 9] == "High") { x4_2[i] = 0 }
else if (Test_data[i, 9] == "Medium") { x4_2[i] = 1 }
else if (Test_data[i, 9] == "Small") { x4_2[i] = 2 }
else { x4_2[i] = 0} }
Test_data[, "Outlet_Size_factor"] <- x4_2

x5 <- rep(0, length=nrow(Training_data))
for (i in 1:nrow(Training_data)) {
if (Training_data[i, 10] == "Tier 1") { x5[i] = 0 }
else if (Training_data[i, 10] == "Tier 2") { x5[i] = 1 }
else if (Training_data[i, 10] == "Tier 3") { x5[i] = 2 }
else { x5[i] = 0 } }
Training_data[, "Outlet_Location_Type_factor"] <- x5

x5_2 <- rep(0, length=nrow(Test_data))
for (i in 1:nrow(Test_data)) {
if (Test_data[i, 10] == "Tier 1") { x5_2[i] = 0 }
else if (Test_data[i, 10] == "Tier 2") { x5_2[i] = 1 }
else if (Test_data[i, 10] == "Tier 3") { x5_2[i] = 2 }
else { x5_2[i] = 0 } }
Test_data[, "Outlet_Location_Type_factor"] <- x5_2

x6 <- rep(0, length=nrow(Training_data))
for (i in 1:nrow(Training_data)) {
if (Training_data[i, 11] == "Grocery") { x6[i] = 0}
else if (Training_data[i, 11] == "Store") { x6[i] = 1}
else if (Training_data[i, 11] == "Supermarket Type1") { x6[i] = 2}
else if (Training_data[i, 11] == "Supermarket Type2") { x6[i] = 3}
else if (Training_data[i, 11] == "Supermarket Type3") { x6[i] = 4}
else { x6[i] = 0} }
Training_data[, "Outlet_Type_factor"] <- x6

x6_2 <- rep(0, length=nrow(Test_data))
for (i in 1:nrow(Test_data)) {
if (Test_data[i, 11] == "Grocery") { x6_2[i] = 0}
else if (Test_data[i, 11] == "Store") { x6_2[i] = 1}
else if (Test_data[i, 11] == "Supermarket Type1") { x6_2[i] = 2}
else if (Test_data[i, 11] == "Supermarket Type2") { x6_2[i] = 3}
else if (Test_data[i, 11] == "Supermarket Type3") { x6_2[i] = 4}
else { x6_2[i] = 0} }
Test_data[, "Outlet_Type_factor"] <- x6_2

mean.weight = mean(Training_data$Item_Weight, na.rm = TRUE)
Training_data$Item_Weight[is.na(Training_data$Item_Weight)] <- mean.weight
mean.weight_2 = mean(Test_data$Item_Weight, na.rm = TRUE)
Test_data$Item_Weight[is.na(Test_data$Item_Weight)] <- mean.weight

dataframe <- data.frame(Item_Weight = Training_data$Item_Weight, Item_Fat_Content_factor = Training_data$Item_Fat_Content_factor, Item_Visibility = Training_data$Item_Visibility, Item_Type_factor = Training_data$Item_Type_factor, Item_MRP = Training_data$Item_MRP, Outlet_Identifier_factor = Training_data$Outlet_Identifier_factor, Outlet_Establishment_Year = Training_data$Outlet_Establishment_Year, Outlet_Size_factor = Training_data$Outlet_Size_factor, Outlet_Location_Type_factor = Training_data$Outlet_Location_Type_factor, Outlet_Type_factor = Training_data$Outlet_Type_factor)
dataframe_2 <- data.frame(Item_Weight = Test_data$Item_Weight, Item_Fat_Content_factor = Test_data$Item_Fat_Content_factor, Item_Visibility = Test_data$Item_Visibility, Item_Type_factor = Test_data$Item_Type_factor, Item_MRP = Test_data$Item_MRP, Outlet_Identifier_factor = Test_data$Outlet_Identifier_factor, Outlet_Establishment_Year = Test_data$Outlet_Establishment_Year, Outlet_Size_factor = Test_data$Outlet_Size_factor, Outlet_Location_Type_factor = Test_data$Outlet_Location_Type_factor, Outlet_Type_factor = Test_data$Outlet_Type_factor)

xgb <- xgboost(data = data.matrix(dataframe[1:floor(0.50 * nrow(Training_data)), ]), label = Training_data$Item_Outlet_Sales[1:floor(0.50 * nrow(Training_data))], nround = 27, max_depth = 3, eta = 0.2, gamma = 10, min_child_weight = 8)
Item_Outlet_Sales <- predict(xgb, data.matrix(dataframe_2))
x <- data.frame(Item_Identifier = Test_data[, 1], Outlet_Identifier = Test_data[, 7], Item_Outlet_Sales, objective = "reg:linear", max_depth = 3, seed = 2)
x <- x[, 1:3]
write.csv(x, file = "70_59253_us_predicted_values_2_aBZV09e.csv")
