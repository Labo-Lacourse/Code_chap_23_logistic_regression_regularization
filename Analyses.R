###     -*- Coding: utf-8 -*-          ###
### Analyste Charles-Édouard Giguère   ###

require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
require(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
require(CUFF, quietly = TRUE, warn.conflicts = FALSE)
require(haven, quietly = TRUE, warn.conflicts = FALSE)


### VD.df <- read_spss(Sys.glob("*.sav")) %>%
###     filter(statut %in% 1:2)
### 
### 
### ### Je vérifie la bd.
### 
### CEG.df <- with(VD.df,
###                data.frame(
###                  ID             = ID,
###                  ECOLE          = ecole,
###                  STATUT         = 2 - statut,
###                  MALE           = male,
###                  AGE            = ageent,
###                  PAR_IMM        = immigmax,
###                  MINORITY       = minority,
###                  SCOLMAX        = scolmax,
###                  TRAVAILM       = travmdic,
###                  TRAVAILP       = travpdic,
###                  PAR_SEP        = parsepdic,
###                  ADAPT          = adapt,
###                  SRDQ           = SRDQ,
###                  ## items du SRDQ.
###                  DOUBLE         = doublé,
###                  AIMES          = aimes,
###                  IMP            = imp,
###                  AMB            = amb,
###                  PR_AUTRES      = pr_autres,
###                  NOTES_FR       = notes_fr,
###                  NOTES_MATH     = notes_math,
###                  ### Autres échelles.
###                  EVDISTSEV      = evdistsev,
###                  EVDISTMOD      = evdistmod,
###                  SEVER03DICO    = sever03dico,
###                  SEVER36DICO    = sever36dico,
###                  SEVER69DICO    = sever69dico,
###                  SEVER912DICO   = sever912dico,
###                  MODER203DICO   = moder203dico,
###                  MODER236DICO   = moder236dico,
###                  MODER269DICO   = moder269dico,
###                  MODER2912DICO  = moder2912dico,
###                  LOW203DICO     = low203dico,
###                  LOW236DICO     = low236dico,
###                  LOW269DICO     = low269dico,
###                  LOW2912DICO    = low2912dico,
###                  CHRONSEVACT    = CHRONSEVACT_dico                 
###                ))  %>%
###     as.data.frame
### 
### 
### 
### 
### ### 1. Généré 1000 données synthétiques qui suivent ce modèle.
### CEG.df <- CEG.df[c(1:366,rep(1,1000)),]
### CEG.df[367:1366,] <- NA
### CEG.df[367:1366,1] <- 1:1000
### CEG.df[367:1366,2] <- rep(13+1:33,c(c(34,36),rep(30,31)))
### CEG.df[367:1366,3] <- unlist(tapply(1:1000,CEG.df[367:1366,2],
###                                     FUN = function(x)
###                                         sample(rep(0:1, length(x)/2),
###                                                    size = length(x))))
### 
### require(mice)
### pm <- matrix(1,35,35)
### diag(pm) <- 0
### pm[,c(1,2,13)] <- 0
### mice1 <- mice(CEG.df,predictorMatrix = pm, m = 1)
### SD.df <- suppressWarnings(complete(mice1))[367:1366,]
### 
### save(SD.df,file = "SD.Rdata")
load("SD.Rdata")


TRAIN.df <- SD.df[1:700,] ; TEST.df <- SD.df[701:1000,]

### 2. Refaire les modèles principaux.

### 2a) glm (régression logistique).

glm1 <- glm(STATUT ~ MALE + AGE + PAR_IMM + MINORITY +
                SCOLMAX + TRAVAILM + TRAVAILP + PAR_SEP + ADAPT +
                SRDQ + EVDISTSEV + EVDISTMOD + SEVER03DICO +
                SEVER36DICO + SEVER69DICO + SEVER912DICO +
                MODER203DICO + MODER236DICO + MODER269DICO +
                MODER2912DICO + LOW203DICO + LOW236DICO +
                LOW269DICO + LOW2912DICO + CHRONSEVACT,
            data = TRAIN.df, family = "binomial")

summary(glm1)

glm1.orig <- glm(STATUT ~ MALE + AGE + PAR_IMM + MINORITY +
                SCOLMAX + TRAVAILM + TRAVAILP + PAR_SEP + ADAPT +
                SRDQ + EVDISTSEV + EVDISTMOD + SEVER03DICO +
                SEVER36DICO + SEVER69DICO + SEVER912DICO +
                MODER203DICO + MODER236DICO + MODER269DICO +
                MODER2912DICO + LOW203DICO + LOW236DICO +
                LOW269DICO + LOW2912DICO + CHRONSEVACT,
                data = CEG.df[1:366,], family = "binomial")




### Les coefficients de la simulation sont similaires aux données
### originales. Les p-values sont parfois plus petites étant donné
### le plus grand nombre de sujet.
cbind(cf(glm1.orig)[,1:4], cf(glm1)[,1:4])



### On fait 10 groupes de 70 participants.
PARTITION = rep(1:10, rep(70,10))
crossval <- function(mod){
    f1 <- function(x){
        modi = update(mod, data = TRAIN.df[!(PARTITION %in% x),])
        table(1*(predict(modi, newdata = TRAIN.df[PARTITION %in% x,],
                         type = "resp")>0.5),
              TRAIN.df[(PARTITION %in% x),"STATUT"])
    }
    CVT <- mapply(f1, x = 1:10)
    as.table(matrix(apply(CVT, 1, sum), 2, 2,
                    dimnames = list(c("P.ND","P.D"),
                                    c("T.ND","T.D"))))
}

locv <- function(mod){
    f1 <- function(x){
        modi = update(mod, data = TRAIN.df[-x,])
        table(factor(1*(predict(modi, newdata = TRAIN.df[x,],
                         type = "resp")>0.5), 0:1),
              factor(SD.df[x,"STATUT"],0:1))
    }
    CVT <- mapply(f1, x = 1:700)
    as.table(matrix(apply(CVT, 1, sum), 2, 2,
                    dimnames = list(c("P.ND","P.D"),
                                    c("T.ND","T.D"))))
}


### Validation avec n fold. 
ftab(locv(glm1))


### Validation du modèle initial. 
ftab(crossval(glm1))


### 2a) lasso ou ridge (glmnet)

require(glmnet)

### Faire nfolds avec assurance d'avoir la même prévalence.

cv.glmn1 <- cv.glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], alpha = 1, nfolds = 7,
      family = "binomial")

plot(cv.glmn1)

glmn1.0 <- glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], alpha = 1, 
      family = "binomial")


glmn1 <- glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], alpha = 1, lambda = cv.glmn1$lambda.min,
      family = "binomial")

plot(glmn1.0, xvar = "lambda")
abline(v = log(cv.glmn1$lambda.min),
       lty = 2)

glmn1p <- predict(glmn1,
                  newx = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                                           "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                                           "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD",
                                           "SEVER03DICO", "SEVER36DICO",
                                           "SEVER69DICO", "SEVER912DICO",
                                           "MODER203DICO", "MODER236DICO", "MODER269DICO",
                                           "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                                           "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
                               as.matrix))    


table(1*(glmn1p>0),
      TRAIN.df$STATUT) %>%
    ftab()

plot(glmn1,xvar = "lambda")



cv.glmn2 <- cv.glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], alpha = 0, nfolds = 7,
      family = "binomial")

plot(cv.glmn2)

glmn2.0 <- glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], alpha = 0, 
      family = "binomial")


glmn2 <- glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], alpha = 1, lambda = cv.glmn2$lambda.min,
      family = "binomial")

plot(glmn2.0, xvar = "lambda")
abline(v = log(cv.glmn2$lambda.min),
       lty = 2)

glmn2p <- predict(glmn2,
                  newx = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                                           "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                                           "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD",
                                           "SEVER03DICO", "SEVER36DICO",
                                           "SEVER69DICO", "SEVER912DICO",
                                           "MODER203DICO", "MODER236DICO", "MODER269DICO",
                                           "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                                           "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
                               as.matrix))    

table(1*(glmn2p>0),
      TRAIN.df$STATUT) %>%
    ftab()


table(1*(glmn1p>0),
      TRAIN.df$STATUT) %>%
    ftab()

ftab(locv(glm1))

cv.glmn3 <- cv.glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], nfolds = 7, alpha = 0.5,
      family = "binomial")

plot(cv.glmn3)
cv.glmn3

glmn3.0 <- glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], alpha = 0.5, 
      family = "binomial")


glmn3 <- glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], lambda = cv.glmn2$lambda.min, alpha = 0.5,
      family = "binomial")

plot(glmn3.0, xvar = "lambda")
abline(v = log(cv.glmn3$lambda.min),
       lty = 2)

glmn3p <- predict(glmn3,
                  newx = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                                           "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                                           "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD",
                                           "SEVER03DICO", "SEVER36DICO",
                                           "SEVER69DICO", "SEVER912DICO",
                                           "MODER203DICO", "MODER236DICO", "MODER269DICO",
                                           "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                                           "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
                               as.matrix))    

table(1*(glmn1p>0),
      TRAIN.df$STATUT) %>%
    ftab()


table(1*(glmn2p>0),
      TRAIN.df$STATUT) %>%
    ftab()

table(1*(glmn3p>0),
      TRAIN.df$STATUT) %>%
    ftab()

glmn3.0 <- glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], alpha = 1, 
      family = "binomial")


glmn3 <- glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], lambda = cv.glmn2$lambda.min, alpha = 0.5,
      family = "binomial")



### 2c) random forest (arbre décisionnelle)

### 2d) neural network (keras, neural-net, with dropout (régularisation))

### Prochaine rencontre 
### Faire un sous-échantillon de 700 (training) 300 (test).

### construire le modèle dans training puis confirmer avec l'échantillon test.

### Écrire à l'éditrice pour savoir comment écrire les équations. 


TRAIN.df <- TRAIN.df %>%
  mutate(AGE2      = AGE^2,
         SCOLMAX2  = SCOLMAX^2,
         SRDQ2     = SRDQ^2,
         EVDISTSEV2 = EVDISTSEV^2,
         EVDISTMOD2 = EVDISTMOD^2,
         MALEADAPT = MALE*ADAPT,
         MALESRDQ  = MALE*SRDQ)
         

cv.glmn4 <- cv.glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT",
                    "AGE2", "SCOLMAX2", "SRDQ2", "EVDISTSEV2", "EVDISTMOD2",
                    "MALEADAPT", "MALESRDQ")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], nfolds = 7, alpha = 1,
      family = "binomial")

plot(cv.glmn4)


glmn4.0 <- glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT",
                    "AGE2", "SCOLMAX2", "SRDQ2", "EVDISTSEV2", "EVDISTMOD2",
                    "MALEADAPT", "MALESRDQ")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], alpha = 1, 
      family = "binomial")


glmn4 <- glmnet(x = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                    "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                    "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD", "SEVER03DICO",
                    "SEVER36DICO", "SEVER69DICO", "SEVER912DICO",
                    "MODER203DICO", "MODER236DICO", "MODER269DICO",
                    "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                    "LOW269DICO", "LOW2912DICO", "CHRONSEVACT",
                    "AGE2", "SCOLMAX2", "SRDQ2", "EVDISTSEV2", "EVDISTMOD2",
                    "MALEADAPT", "MALESRDQ")] %>%
      as.matrix),
      y = TRAIN.df[,"STATUT"], lambda = cv.glmn4$lambda.min, alpha = 1,
      family = "binomial")

glmn4p <- predict(glmn4,
                  newx = scale(TRAIN.df[,c("MALE", "AGE", "PAR_IMM", "MINORITY",
                                           "SCOLMAX", "TRAVAILM", "TRAVAILP", "PAR_SEP",
                                           "ADAPT", "SRDQ", "EVDISTSEV", "EVDISTMOD",
                                           "SEVER03DICO", "SEVER36DICO",
                                           "SEVER69DICO", "SEVER912DICO",
                                           "MODER203DICO", "MODER236DICO", "MODER269DICO",
                                           "MODER2912DICO", "LOW203DICO", "LOW236DICO",
                                           "LOW269DICO", "LOW2912DICO", "CHRONSEVACT",
                    "AGE2", "SCOLMAX2", "SRDQ2", "EVDISTSEV2", "EVDISTMOD2",
                    "MALEADAPT", "MALESRDQ")] %>%
                               as.matrix))    

table(1*(glmn1p>0),
      TRAIN.df$STATUT) %>%
    ftab()


table(1*(glmn2p>0),
      TRAIN.df$STATUT) %>%
    ftab()

table(1*(glmn3p>0),
      TRAIN.df$STATUT) %>%
    ftab()


table(1*(glmn4p>0),
      TRAIN.df$STATUT) %>%
    ftab()

