#install.packages("C50")
library("C50")
library("caret")
library("ggpubr")

df <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data"), 
               header=FALSE,
               na.strings = c("?"))
columns = c(
  "class", "handicapped_infants", "water_project_cost_sharing", "adoption_of_the_budget_resolution", "physician_fee_freeze", 
  "el_salvador_aid", "religious_groups_in_schools",   "anti_satellite_test_ban", "aid_to_nicaraguan_contras", "mx_missile", 
  "immigration", "synfuels_corporation_cutback", "education_spending", "superfund_right_to_sue", "crime", "duty_free_exports",
  "export_administration_act_south_africa")
colnames(df) <- columns

df$class = factor(df$class, levels = c("republican","democrat"), labels = c("republican", "democrat"))
#flags de los datos categoricos
flags = c("no", "yes")

# Discretizo los datos

df$handicapped_infants = factor(df$handicapped_infants, levels = c("y","n"), labels = c("yes", "no"))
df$water_project_cost_sharing = factor(df$water_project_cost_sharing, levels = c("y","n"), labels = c("yes", "no"))
df$adoption_of_the_budget_resolution  = factor(df$adoption_of_the_budget_resolution , levels = c("y","n"), labels = c("yes", "no"))
df$physician_fee_freeze  = factor(df$physician_fee_freeze , levels = c("y","n"), labels = c("yes", "no"))
df$el_salvador_aid     = factor(df$el_salvador_aid    , levels = c("y","n"), labels = c("yes", "no"))
df$religious_groups_in_schools = factor(df$religious_groups_in_schools, levels = c("y","n"), labels = c("yes", "no"))
df$anti_satellite_test_ban  = factor(df$anti_satellite_test_ban , levels = c("y","n"), labels = c("yes", "no"))
df$aid_to_nicaraguan_contras   = factor(df$aid_to_nicaraguan_contras  , levels = c("y","n"), labels = c("yes", "no"))
df$mx_missile = factor(df$mx_missile, levels = c("y","n"), labels = c("yes", "no"))
df$immigration = factor(df$immigration, levels = c("y","n"), labels = c("yes", "no"))

df$synfuels_corporation_cutback = factor(df$synfuels_corporation_cutback, levels = c("y","n"), labels = c("yes", "no"))
df$education_spending = factor(df$education_spending, levels = c("y","n"), labels = c("yes", "no"))
df$superfund_right_to_sue = factor(df$superfund_right_to_sue, levels = c("y","n"), labels = c("yes", "no"))
df$crime = factor(df$crime, levels = c("y","n"), labels = c("yes", "no"))

df$duty_free_exports = factor(df$duty_free_exports, levels = c("y","n"), labels = c("yes", "no"))
df$export_administration_act_south_africa = factor(df$export_administration_act_south_africa, levels = c("y","n"), labels = c("yes", "no"))

#Iteración 1 60%
set.seed(77)
training.index = createDataPartition(df$class, p=0.6)$Resample1
training.set = df[training.index, ]
test.set = df[-training.index, ]

tree = C5.0(class ~ ., training.set)


tree.pred.class = predict(tree, test.set[,-1], type = "class")
tree.pred.prob = predict(tree, test.set[,-1], type = "prob")

plot(tree)
summary(tree)

conf.matrix.tree = confusionMatrix(table(test.set$class, tree.pred.class))
print(conf.matrix.tree)

#Iteración 2 70%
set.seed(77)
training.index = createDataPartition(df$class, p=0.7)$Resample1
training.set = df[training.index, ]
test.set = df[-training.index, ]

tree = C5.0(class ~ ., training.set)


tree.pred.class = predict(tree, test.set[,-1], type = "class")
tree.pred.prob = predict(tree, test.set[,-1], type = "prob")

plot(tree)
summary(tree)

conf.matrix.tree = confusionMatrix(table(test.set$class, tree.pred.class))
print(conf.matrix.tree)

#Iteración 3 80%
set.seed(77)
training.index = createDataPartition(df$class, p=0.7)$Resample1
training.set = df[training.index, ]
test.set = df[-training.index, ]

tree = C5.0(class ~ ., training.set)


tree.pred.class = predict(tree, test.set[,-1], type = "class")
tree.pred.prob = predict(tree, test.set[,-1], type = "prob")

plot(tree)
summary

conf.matrix.tree = confusionMatrix(table(test.set$class, tree.pred.class))
print(conf.matrix.tree)

#Iteración 4 90%
set.seed(77)
training.index = createDataPartition(df$class, p=0.9)$Resample1
training.set = df[training.index, ]
test.set = df[-training.index, ]

tree = C5.0(class ~ ., training.set)


tree.pred.class = predict(tree, test.set[,-1], type = "class")
tree.pred.prob = predict(tree, test.set[,-1], type = "prob")

plot(tree)
summary(tree)
conf.matrix.tree = confusionMatrix(table(test.set$class, tree.pred.class))
print(conf.matrix.tree)
