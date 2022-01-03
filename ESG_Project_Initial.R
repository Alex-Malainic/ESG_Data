setwd("C:/Users/alexm/OneDrive/Disciplines/Introducere in R/Proiect")
library(psych)
library(ggplot2)

head(raw_df)
raw_df <- read.csv(file = "ESG_data.csv")
numeric_df <- Filter(is.numeric, raw_df[-1]) #select only the numeric columns

############### DESCRIPTIVE STATISTICS ########################

#### analiza descriptiva numerica a variabilelor numerice si nenumerice (+ indicatorii descriptivi pe grupuri pt variabile numerice) ####

summary(raw_df) #summary of all variables in the dataset

sapply(numeric_df, sd) #standard deviation of all variables

describeBy(raw_df$CO2.Emissions, group = raw_df$Region, digits = 4) #statistici descriptive pe regiuni

describeBy(raw_df$CO2.Emissions, group = raw_df$Income.Group, digits = 4) #statistici descriptive pe grupuri de venit

tapply(raw_df$CO2.Emissions, list(raw_df$Region, raw_df$Income.Group), mean)  #media emisiilor pe combinatia celor 2 variabile categoriale


#### analiza grafica a variabilelor numerice si nenumerice ####

#function to plot all histograms in one image

histograms = function(df){

  #calculating the subplot dimension coordinates
  kk = dim(df)[2]
  x = round(sqrt(kk),0)
  y = ceiling(kk/x)
  
  dimension <- c(x,y)

  par(mfrow = dimension)
  
  #plotting the histograms
  
  for(i in names(df)){
    hist(df[[i]] ,main="Histogram",xlab= i ,col="green",label=TRUE,plot = TRUE)
  }
}
histograms(numeric_df)

#function to plot all boxplots in one image
boxplots = function(df){
  
  #calculating the subplot dimension coordinates
  kk = dim(df)[2]
  x = round(sqrt(kk),0)
  y = ceiling(kk/x)
  
  dimension <- c(x,y)
  
  par(mfrow = dimension)
  
  #plotting the histograms
  
  for(i in names(df)){
    boxplot(df[[i]] ,main="Boxplot",xlab= i ,col="Yellow",label=TRUE,plot = TRUE)
  }
}
boxplots(numeric_df)  

pairs(numeric_df, pch = 18, col = 'green')

ggplot(raw_df, aes(x = Region, y = CO2.Emissions, color = Income.Group)) +  # ggplot function   #boxplots of emissions by income group and region
  geom_boxplot(outlier.colour="red")     #outliers will be colored in red


#### identificarea outlierilor si eliminarea acestora din baza ####

outlier_clean = function (df){
  clean_df <- df
  for (i in names(df)){
    x <- df[[i]]
    qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
    caps <- quantile(x, probs=c(.05, .95), na.rm = T)
    H <- 1.5 * IQR(x, na.rm = T)
    x[x < (qnt[1] - H)] <- caps[1]
    x[x > (qnt[2] + H)] <- caps[2]
    clean_df[[i]] <- x
  }
  return (clean_df)
}

df = outlier_clean(numeric_df)

boxplots(df)

############### ANALIZA STATISTICA A VARIABILELOR CATEGORIALE ########################

#### tabelarea datelor (frecvente marginale, conditionate, partiale) ####

region_table <- table(raw_df$Region)
income_table <- table(raw_df$Income.Group)
table(raw_df$Region)/193
table(raw_df$Income.Group)/193

crosstab <- table(raw_df$Region, raw_df$Income.Group)
#### analiza de asociere ####



summary(crosstab)  #p_value < 0.05, se respinge h0 -> exista o asociere semnificativa intre cele 2 variabile


#### analiza de concordanta ####


chisq.test(region_table)  
chisq.test(income_table)

#p values <0.05, se respinge h0, distributie observata nu concorda ca structura cu cea teoretica


################## ANALIZA DE REGRESIE SI CORELATIE ############

            #### analiza de corelatie ####

cor(df)   # matricea coeficientilor de corelatie Pearson

cor.test(df$CO2.Emissions, df$Renewable.Energy.Consumption)


            #### regresie liniara simpla ####

simple_lm_renewable <- lm(CO2.Emissions~Renewable.Energy.Consumption, df)
resid_simple_lm_aff <- resid(simple_lm_aff)

summary(simple_lm_aff)
predict(simple_lm_aff, data.frame( AFF= 15), interval = "confidence") 


plot(CO2.Emissions~Renewable.Energy.Consumption, df)
abline(coef(simple_lm_aff))

  
          #### regresie liniara multipla ####


multiple_lm <- lm(CO2.Emissions~AFF+Government.Effectiveness+Individuals.using.the.Internet+Life.Expectancy+Renewable.Energy.Consumption,df)
resid_multiple_lm <- resid(multiple_lm)

summary(multiple_lm)

#identificare existenta punctelor de extrem pentru modelul de regresie
cooksd=cooks.distance(multiple_lm)
plot(cooksd)
abline(h = 4*mean(cooksd, na.rm=T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red") #adaugare etichete puncte extreme

influencePlot(multiple_lm,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

outlierTest(multiple_lm)

qqPlot(multiple_lm)   #verificare normalitate reziduuri

#H0: observatia nr 133 este outlier, p_value > alpha -> nu respingem h0

            #### regresie neliniara ####

poly2_renewable <- lm(CO2.Emissions~Renewable.Energy.Consumption + I(Renewable.Energy.Consumption^2), df) #beta1 negativ si beta2 pozitiv

summary(poly2_renewable)
plot(CO2.Emissions~Renewable.Energy.Consumption, df)
curve(7.9506436 - 0.2005285*x + 0.0013608  * x ^2, add = T)


            #### compararea a doua modele de regresie si alegerea celui mai bun model #### 


anova(simple_lm_renewable, poly2_renewable)  #p_value < 0.05



########################## ESTIMAREA SI TESTAREA MEDIILOR #############


    #### Estimarea mediei prin interval de incredere ####

t.test(df$CO2.Emissions, mu = 4)  # 95%
t.test(df$CO2.Emissions, mu = 4, conf.level = 0.99)  #99%

    #### Testarea unei medii cu o valoarea fixa ####


t.test(df$CO2.Emissions, mu = 4)   #p_value > 0.05, nu respingem ipoteza nula, true mean is equal to 4

t.test(df$Life.Expectancy, mu = 73, alternative = 'less') #h0: true mean is greather than 73 years -> we do not reject h0

    #### Testarea diferentei dintre doua medii ####
bartlett.test(Government.Effectiveness~Region, raw_df, Region %in% c("South Asia", "Europe & Central Asia"))  #testarea variantei dintre populatii

#deoarecele variantele celor doua populatii comparate sunt egale, setam argumentul var.equal = T
t.test(Government.Effectiveness~Region, raw_df, Region %in% c("South Asia", "Europe & Central Asia"), var.equal = T)

    #### Testarea diferentei dintre trei sau mai multe medii ####

anova_region <- aov(CO2.Emissions~Region, raw_df)

anova(anova_region) 

coef(anova_region)
