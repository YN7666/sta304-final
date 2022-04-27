knitr::opts_chunk$set(echo = TRUE)
#tinytex::install_tinytex()
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(kableExtra)
library(haven)
library(usmap)
library(car)
library(broom)
library(patchwork)



amobesity <- read_csv("tmp.csv")
amobesity
states <- read.csv("state_births.csv")
data_full <- read.csv("datafull.csv")


raw_data <- read.csv("raw_data (1).csv")

data_full <- ll %>% select (`_BMI5`,SLEPTIM1,GENHLTH,ALCDAY5,SEXVAR, `_RFSMOK3`,`_AGEG5YR`,EXERANY2,DRNKANY5, `_MENT14D`) %>% filter(!is.na(`_BMI5`) & !is.na(ALCDAY5)& !is.na(`_RFSMOK3`)& !is.na(SLEPTIM1)& !is.na(GENHLTH)& !is.na(SEXVAR) &SLEPTIM1<24 &`_MENT14D` < 9 &SLEPTIM1>1 &GENHLTH<7 & `_RFSMOK3` <3 &`_AGEG5YR`<14& EXERANY2 < 7 & DRNKANY5<7& ALCDAY5<900 & ALCDAY5 != 777) %>% 
  mutate(ALCDAY5 = case_when(ALCDAY5 < 200 ~ ALCDAY5-100,
                             ALCDAY5 > 200 & ALCDAY5<300 ~ ALCDAY5-200,
                             ALCDAY5>300  ~ ALCDAY5 -888),
         `_BMI5` = `_BMI5` /100)
set.seed(000)
N=nrow(data_full)
for (i in 1:N){
  if (data_full$`_AGEG5YR`[i] == 1)
    data_full$`_AGEG5YR`[i] = sample(x = 18:24, size  = 1)
  else if (data_full$`_AGEG5YR`[i] == 2)
    data_full$`_AGEG5YR`[i] = sample(x = 25:29, size  = 1)
  else if (data_full$`_AGEG5YR`[i] == 3)
    data_full$`_AGEG5YR`[i] = sample(x = 30:34, size  = 1)
  else if (data_full$`_AGEG5YR`[i] == 4)
    data_full$`_AGEG5YR`[i] = sample(x = 35:39, size  = 1)
  else if (data_full$`_AGEG5YR`[i] == 5)
    data_full$`_AGEG5YR`[i] = sample(x = 40:44, size  = 1)
  else if (data_full$`_AGEG5YR`[i] == 6)
    data_full$`_AGEG5YR`[i] = sample(x = 45:49, size  = 1)
  else if (data_full$`_AGEG5YR`[i] == 7)
    data_full$`_AGEG5YR`[i] = sample(x = 50:54, size  = 1)
  else if (data_full$`_AGEG5YR`[i] == 8)
    data_full$`_AGEG5YR`[i] = sample(x = 55:59, size  = 1)
  else if (data_full$`_AGEG5YR`[i] == 9)
    data_full$`_AGEG5YR`[i] = sample(x = 60:64, size  = 1)
  else if (data_full$`_AGEG5YR`[i] == 10)
    data_full$`_AGEG5YR`[i] = sample(x = 65:69, size  = 1)
  else if (data_full$`_AGEG5YR`[i] == 11)
    data_full$`_AGEG5YR`[i] = sample(x = 70:74, size  = 1)
  else if (data_full$`_AGEG5YR`[i] == 12)
    data_full$`_AGEG5YR`[i] = sample(x = 75:79, size  = 1)
  else
    data_full$`_AGEG5YR`[i] = sample(x = 80:99, size  = 1)

  data_full%>% ggplot(aes(x = X_BMI5)) +
    geom_histogram(bins = 30, color = "black", fill = "#2a71e2") +
    theme_classic()+
    labs(x = "BMI", title = "Fig.1 Histogram of Body Mass Index
     for each respondent.")
  table1 <- data_full %>% summarise(mean = mean(X_BMI5),
                                    min = min(X_BMI5),
                                    `1st Qu.` = quantile(X_BMI5, 0.25),
                                    median = median(X_BMI5),
                                    `3st Qu.` = quantile(X_BMI5, 0.75),
                                    max = max(X_BMI5),
                                    IQR = `3st Qu.` -`1st Qu.`,
                                    sd = sd(X_BMI5),
                                    small_outliers = sum(X_BMI5< `1st Qu.` -1.5*IQR),
                                    large_outliers = sum(X_BMI5> `3st Qu.` +1.5*IQR)) %>% 
    kable(caption = "summary statistic of Body Mass Index") %>%
    kable_classic(full_width = F, html_font = "Cambria") %>%
    kable_styling(latex_options = "HOLD_position")
  table1
  
  data_fact1<- data_full%>% ggplot(aes(x = SLEPTIM1)) +
    geom_histogram(bins = 30, color = "black", fill = "#1b98e0") +
    theme_classic()+
    labs(x = "sleeping time", title = "Histogram of sleeping time
     for each respondent.")

  data_fact2<- data_full%>% ggplot(aes(x = GENHLTH)) +
    geom_histogram(bins = 30, color = "black", fill = "#1b98e0") +
    theme_classic()+
    labs(x = "health rate", title = "Histogram of general self rated health
     for each respondent.")

  data_fact3<- data_full%>% ggplot(aes(x = X_AGEG5YR)) +
    geom_histogram(bins = 30, color = "black", fill = "#1b98e0") +
    theme_classic()+
    labs(x = "Age", title = "Histogram of Age 
     for each respondent.")

  data_fact4<- data_full%>% ggplot(aes(x = ALCDAY5)) +
    geom_histogram(bins = 30, color = "black", fill = "#1b98e0") +
    theme_classic()+
    labs(x = "alcohol days", title = "Histogram of drinking alcohol days
     for each respondent.")

  g_all <- (data_fact1|data_fact2)/
    (data_fact3|data_fact4)+   
    plot_annotation(title = "Fig.2 Four numerical Indicators") & 
    theme(plot.title = element_text(hjust = 0.5))
  g_all
  data_full%>% select(SLEPTIM1,GENHLTH,X_AGEG5YR,ALCDAY5) %>% summary()%>%
    kable(caption = "summary statistic of Four numerical Indicators") %>%
    kable_classic(full_width = F, html_font = "Cambria") %>%
    kable_styling(latex_options = "HOLD_position")
  data1 <- data_full %>% count(DRNKANY5, sort = TRUE)
  data2 <- data_full %>% count(SEXVAR, sort = TRUE)
  data3 <- data_full %>% count(X_RFSMOK3, sort = TRUE) 
  data4 <- data_full %>% count(EXERANY2, sort = TRUE)
  character <- c("drink alcohol or not ", "Male or Female", "smoke or not", "exercise or not")
  Yes_or_male <- c(175014, 158151, 46466, 256270)
  No_or_female <- c(157465, 174328, 286013, 76209)
  df <- data.frame(character, Yes_or_male,No_or_female )
  df.long <- gather(df, variable,count, -character) 
  ggplot(data = df.long, aes(x = character, y = count, fill = variable)) +
    geom_col(position = position_dodge()) +
    labs(title = "Fig.3 Barplot of four categorical variables")
  set.seed(000)
  # Count the number of observations in the data
  n <- nrow(data_full)
  # Randomly choose 80% as training
  training <- sample(1:n, size=round(0.8*n))
  # Add a column called "rowid" to our original data
  data_full <- data_full %>% rowid_to_column()
  # Create a training set
  train <- data_full %>% filter(rowid %in% training)
  # Create a testing set
  test <- data_full %>% filter(!(rowid %in% training))
  data_tran_train <-train %>% mutate(X_BMI5 =  X_BMI5^(-.5), SLEPTIM1 = SLEPTIM1^(.66), GENHLTH = GENHLTH^(.5), SEXVAR = SEXVAR^(0.5),X_RFSMOK3 = X_RFSMOK3^(-10),EXERANY2 = EXERANY2^(-6),DRNKANY5 = DRNKANY5^(-0.5))
  model_tran_train <-lm(formula = X_BMI5 ~ SLEPTIM1 + GENHLTH + SEXVAR + X_RFSMOK3 + X_AGEG5YR + EXERANY2 + DRNKANY5, data = data_tran_train)
  res <- resid(model_tran_train)
  qqnorm(res, main = "Fig4: Normal Q-Q Plot")
  qqline(res)
  tmp <-amobesity%>% select(`_BMI5`, `_STATE`) %>% filter( !is.na(`_BMI5`))%>%mutate(`_BMI5` = `_BMI5` /100)
  tmp1 <- tmp%>% group_by(`_STATE`)%>%summarise( avg_OBES = mean(`_BMI5`))
  colnames(tmp1) <- c("state_num", "avg_OBES")
  colnames(states) <- c("state_num", "state")
  state_obes <- merge(tmp1,states,by=c("state_num"))
  p <- plot_usmap(data = state_obes, values = "avg_OBES", color = "red", labels=TRUE) + 
    scale_fill_continuous(low = "white", high = "red", 
                          name = "average obesity", label = scales::comma) + 
    theme(legend.position = "right") + 
    theme(panel.background = element_rect(colour = "black")) + 
    labs(title = "Fig.5: average obesity in each state")
  p$layers[[2]]$aes_params$size <- 3
  print(p)
  
  colnames(raw_data) <- c("state_num", "Health Spending")
  state_obes <- merge(raw_data,states,by=c("state_num"))
  p <- plot_usmap(data = state_obes, values = "Health Spending", color = "red", labels=TRUE) + 
    scale_fill_continuous(low = "white", high = "red", 
                          name = "health spending ", label = scales::comma) + 
    theme(legend.position = "right") + 
    theme(panel.background = element_rect(colour = "black")) + 
    labs(title = "Fig.6: health spending per capita in each states")
  p$layers[[2]]$aes_params$size <- 3
  print(p)