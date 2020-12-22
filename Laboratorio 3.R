#install.packages("arulesViz")
library("arulesViz")
df <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data"), 
               header=FALSE,
               na.strings = c("?"))
columns = c(
  "Class", "AGE", "SEX", "STEROID", "ANTIVIRALS", "FATIGUE", "MALAISE", "ANOREXIA", 
  "LIVER_BIG", "LIVER_FIRM", "SPLEEN_PALPABLE", "SPIDERS", "ASCITES", "VARICES", 
  "BILIRUBIN", "ALK_PHOSPHATE", "SGOT", "ALBUMIN", "PROTIME", "HISTOLOGY")
colnames(df) <- columns

df$Class = factor(df$Class, levels = c(1,2), labels = c("DIE", "LIVE"))

#se definen las clasificaciones de los datos categóricos
flags = c("no", "yes")
df$SEX = factor(df$SEX, levels = c(1,2), labels = c("male", "female"))
df$STEROID = factor(df$STEROID, levels = c(1,2), labels = flags)
df$ANTIVIRALS = factor(df$ANTIVIRALS, levels = c(1,2), labels = flags)
df$FATIGUE = factor(df$FATIGUE, levels = c(1,2), labels = flags)
df$MALAISE = factor(df$MALAISE, levels = c(1,2), labels = flags)
df$ANOREXIA = factor(df$ANOREXIA, levels = c(1,2), labels = flags)
df$LIVER_BIG = factor(df$LIVER_BIG, levels = c(1,2), labels = flags)
df$LIVER_FIRM = factor(df$LIVER_FIRM, levels = c(1,2), labels = flags)
df$SPLEEN_PALPABLE = factor(df$SPLEEN_PALPABLE, levels = c(1,2), labels = flags)
df$SPIDERS = factor(df$SPIDERS, levels = c(1,2), labels = flags)
df$ASCITES = factor(df$ASCITES, levels = c(1,2), labels = flags)
df$VARICES = factor(df$VARICES, levels = c(1,2), labels = flags)
df$HISTOLOGY = factor(df$HISTOLOGY, levels = c(1,2), labels = flags)

str(df.rules)

summary(df)
summary(df[df$Class == "DIE",])

df.rules = df
AGE = c(-Inf, 30, 38.75, 46.59, 54.5, 70, Inf)
AGE.names = c("menor", "joven", "adulto-joven", "adulto", "adulto-mayor", "cuarta edad")

BILIRUBIN = c(-Inf,0.4, 1.2, 2.543, 3.650, 8, Inf)
BILIRUBIN.names = c("muy baja", "baja", "media-baja", "media", "media-alta", "alta")

ALK_PHOSPHATE = c(-Inf, 62, 84.75, 122.38, 142.25, 280,Inf)
ALK_PHOSPHATE.names = c("muy baja", "baja", "media-baja", "media", "media-alta", "alta")

SGOT = c(-Inf, 16, 48.25, 99.83, 118.5, 528, Inf)
SGOT.names = c("muy baja", "baja", "media-baja", "media", "media-alta", "alta")

ALBUMIN = c(-Inf, 2.1, 2.65, 3.152, 3.55, 4.2, Inf)
ALBUMIN.names = c("muy baja", "baja", "media-baja", "media", "media-alta", "alta")

PROTIME = c(-Inf, 29, 31.25, 43.5, 45.25, 90, Inf)
PROTIME.names = c("muy baja", "baja", "media-baja", "media", "media-alta", "alta")

df.rules$AGE = cut(df.rules$AGE, breaks = AGE, labels = AGE.names)
df.rules$BILIRUBIN = cut(df.rules$BILIRUBIN, breaks = BILIRUBIN, labels = BILIRUBIN.names)
df.rules$SGOT = cut(df.rules$SGOT, breaks = SGOT, labels = SGOT.names)
df.rules$ALBUMIN = cut(df.rules$ALBUMIN, breaks = ALBUMIN, labels = ALBUMIN.names)
df.rules$PROTIME = cut(df.rules$PROTIME, breaks = PROTIME, labels = PROTIME.names)
df.rules$ALK_PHOSPHATE = cut(df.rules$ALK_PHOSPHATE, breaks = ALK_PHOSPHATE, labels = ALK_PHOSPHATE.names)

head(df.rules)
str(df.rules)

##Iteración inicial DIE

rules = apriori(
  data = df.rules, 
  parameter= list(support = 0.05, 
                  minlen = 2, 
                  maxlen = 20, 
                  target="rules",
                  maxtime=0,
                  confidence=0.8),
  appearance=list(rhs = c("Class=DIE"))    
)
##inspect(sort(x = rules, decreasing = TRUE, by = "support"))

reglas_maximas <- rules[is.maximal(rules)]
reglas_maximas

inspect(reglas_maximas)

##2a Iteración

df.rules$SPLEEN_PALPABLE=yes<-NULL
df$SPLEEN_PALPABLE=yes<- NULL

rules = apriori(
  data = df.rules, 
  parameter= list(support = 0.17, 
                  minlen = 2, 
                  maxlen = 19, 
                  target="rules",
                  maxtime=0,
                  confidence=0.1),
  appearance=list(rhs = c("Class=DIE"))    
)
inspect(sort(x = rules, decreasing = TRUE, by = "confidence"))

#Como resultado de la iteración #2 Se obtiene un aumento en el soporte y una disminución en la confianza (72% y 83%)

### Iteración 3 DIE
df.rules$PROTIME<-NULL
df$PROTIME<-NULL

rules = apriori(
  data = df.rules, 
  parameter= list(support = 0.17, 
                  minlen = 2, 
                  maxlen = 18, 
                  target="rules",
                  maxtime=0,
                  confidence=0.1),
  appearance=list(rhs = c("Class=DIE"))    
)
inspect(sort(x = rules, decreasing = TRUE, by = "confidence"))

reglas_maximas <- rules[is.maximal(rules)]
reglas_maximas
inspect(reglas_maximas[1:1])
plot(rules)

### Iteración 4 DIE

df.rules$MALAISE<-NULL
df$MALAISE<-NULL

rules = apriori(
  data = df.rules, 
  parameter= list(support = 0.17, 
                  minlen = 2, 
                  maxlen = 18, 
                  target="rules",
                  maxtime=0,
                  confidence=0.1),
  appearance=list(rhs = c("Class=DIE")))

inspect(sort(x = rules, decreasing = TRUE, by = "confidence"))
  
plot(rules)

reglas_maximas <- rules[is.maximal(rules)]
reglas_maximas
inspect(reglas_maximas[1:1]) 

df.rules$ANTIVIRALS<-NULL
df$ANTIVIRALS<-NULL

df.rules$SPLEEN_PALPABLE<-NULL
df$SPLEEN_PALPABLE<-NULL

df.rules$HISTOLOGY<-NULL
df$HISTOLOGY<-NULL

df.rules$ALK_PHOSPHATE<-NULL
df$ALK_PHOSPHATE<-NULL


df.rules$SPIDERS<-NULL
dfs$SPIDERS<-NULL

df.rules$FATIGUE<-NULL
dfs$FATIGUE<-NULL

rules = apriori(
  data = df.rules, 
  parameter= list(support = 0.03, 
                  minlen = 2, 
                  maxlen = 15, 
                  target="rules",
                  maxtime=0,
                  confidence=1),
  appearance=list(rhs = c("Class=DIE")))

inspect(sort(x = rules, decreasing = TRUE, by = "confidence"))

reglas_maximas <- rules[is.maximal(rules)]
reglas_maximas
inspect(reglas_maximas) 

rules = apriori(
  data = df.rules, 
  parameter= list(support = 0.06, 
                  minlen = 2, 
                  maxlen = 20, 
                  target="rules",
                  maxtime=0,
                  confidence=0.75),
  appearance=list(rhs = c("Class=DIE"))    
)
inspect(sort(x = rules, decreasing = TRUE, by = "support"))

reglas_maximas <- rules[is.maximal(rules)]
reglas_maximas
inspect(reglas_maximas)