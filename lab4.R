#install.packages("e1071")
#install.packages("caret")
#install.packages("ROCR")

library(e1071)
library(caret)
library(ROCR)

df <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data"), 
               header=FALSE,
               na.strings = c("?"))
columns = c(
  "class", "handicapped_infants", "water_project_cost_sharing", "adoption_of_the_budget_resolution", "physician_fee_freeze", "el_salvador_aid", "religious_groups_in_schools", 
  "anti_satellite_test_ban", "aid_to_nicaraguan_contras", "mx_missile", "immigration", "synfuels_corporation_cutback", "education_spending", "superfund_right_to_sue", "crime", "duty_free_exports",
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

#Evaluo preliminarmente los datos
summary(df)

# Elimino la variable export_administration_act_south_africa por la alta cantidad de valores NA
df$export_administration_act_south_africa<- NULL

#Extraigo solo las observaciones completas
dfLimpio <- df[complete.cases(df), ]

str(dfLimpio)

#Test 1: Datos Limpios

set.seed(100)

# Se crean las particiones de datos
training.index = createDataPartition(dfLimpio$class, p=0.8)$Resample1

#Se crea el consunto de entrenamiento y el de test
training.set = dfLimpio[training.index, ]
test.set = dfLimpio[-training.index, ]

#Se aplica el modelo bayesiano ingenuo
bayesian.classifier = naiveBayes(class ~ ., data = training.set)
bayesian.classifier

bayesian.results = predict(object = bayesian.classifier, test.set)
test.set["predicted"] = bayesian.results

#Se obtiene la matriz de confusión
conf.matrix.cb = confusionMatrix(table(test.set$class, test.set$predicted))
print(conf.matrix.cb)

#Test 2 Aplicar el clasificador anterior con el set completo de datos

set.seed(100)

#Se repite el proceso, con el mismo clasificador, pero particionando un subconjunto con el set completo de entrenamiento
training2.index = createDataPartition(df$class, p=0.8)$Resample1
training2.set = df[training2.index, ]
test2.set = df[-training2.index, ]

bayesian2.results = predict(object = bayesian.classifier, test2.set)
test2.set["predicted"] = bayesian2.results

conf.matrix.cb2 = confusionMatrix(table(test2.set$class, test2.set$predicted))
print(conf.matrix.cb2)


#Test 3 - Nuevo clasificador

set.seed(100)

training3.index = createDataPartition(df$class, p=0.80)$Resample1
training3.set = df[training3.index, ]
test3.set = df[-training3.index, ]

bayesian2.classifier = naiveBayes(class ~ ., data = training3.set)
bayesian2.classifier

bayesian2.results = predict(object = bayesian2.classifier, test3.set)
test3.set["predicted"] = bayesian2.results

conf.matrix.cb2 = confusionMatrix(table(test3.set$class, test3.set$predicted))
print(conf.matrix.cb2)


#test 4 Se considera solo la variable physician_fee_freeze
set.seed(100)

training4.index = createDataPartition(df$class, p=0.80)$Resample1
training4.set = df[training4.index, ]
test4.set = df[-training4.index, ]

bayesian3.classifier = naiveBayes(class ~ physician_fee_freeze, data = training4.set)
bayesian3.classifier

bayesian3.results = predict(object = bayesian3.classifier, test4.set)
test4.set["predicted"] = bayesian3.results

conf.matrix.cb4 = confusionMatrix(table(test4.set$class, test4.set$predicted))
print(conf.matrix.cb4)


pred1 <- prediction(as.numeric(bayesian3.results), as.numeric(test4.set$class))
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1)



