1. Overview
<<<<<<< HEAD
This paper is about American obesity and Multiple Indicators and is demonstrated by a few graph and a model.
=======
>>>>>>> 4495022f52ce520374295949736ed795492ccf32
The code in this replication packages constructs our analysis using R Studio.  The following packages is needed before running the code.
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(kableExtra)
library(knitr)
library(haven)
library(usmap)
library(car)
library(broom)

<<<<<<< HEAD
graph:
the map of american with BMI in each state
=======

the map of american with BMI by states
>>>>>>> 4495022f52ce520374295949736ed795492ccf32
```{r, echo = FALSE, message = FALSE, warning = FALSE}
tmp <-amobesity%>% select(`_BMI5CAT`, `_STATE`) %>% filter( !is.na(`_BMI5CAT`))
tmp1 <- tmp%>% group_by(`_STATE`)%>%summarise( avg_OBES = mean(`_BMI5CAT`))
colnames(tmp1) <- c("state_num", "avg_OBES")
colnames(states) <- c("state_num", "state")
state_obes <- merge(tmp1,states,by=c("state_num"))
p <- plot_usmap(data = state_obes, values = "avg_OBES", color = "red", labels=TRUE) + 
  scale_fill_continuous(low = "white", high = "red", 
                         name = "percent change of birth rate", label = scales::comma) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "average obesity in each state")
p$layers[[2]]$aes_params$size <- 3
print(p)
```
<<<<<<< HEAD
There will be more plots in the future.
=======
>>>>>>> 4495022f52ce520374295949736ed795492ccf32

model. The bmi is the response and the sleeping time and exercise experience.
```{r, echo = FALSE}
data_full <- ll %>% select (`_BMI5`,INCOME2,EDUCA,SLEPTIM1,MENTHLTH,GENHLTH,EXERANY2,AVEDRNK3,SEXVAR) %>% filter(!is.na(`_BMI5`) & !is.na(INCOME2)& !is.na(EDUCA)& !is.na(SLEPTIM1)& !is.na(MENTHLTH)& !is.na(GENHLTH)& !is.na(EXERANY2)& !is.na(AVEDRNK3)& !is.na(SEXVAR))
mod_full<- lm(`_BMI5`~SLEPTIM1+EXERANY2, data = data_full)
summary(mod_full)
vif(mod_full)
augment(mod_full,
          data = data_full)
```
```{r, echo = FALSE, message = FALSE, warning = FALSE}
 
data_full %>% filter(SLEPTIM1 < 24 & EXERANY2 <7)%>% 
  mutate(EXERANY2 = case_when(EXERANY2 == 1 ~"yes",
                              EXERANY2 == 2 ~"no"))%>%
ggplot(aes(x = SLEPTIM1,y=`_BMI5`, color = EXERANY2)) + geom_point() +
geom_smooth(method = "lm", se = FALSE) + theme_classic() +
labs(x = "sleephour", y = "_BMI", title = "Relationship between the Age (in years) of respondent and
     Average diastolic blood pressure value in different sex")
data_full %>% filter(SLEPTIM1 < 12 & EXERANY2 <7)%>% 
  mutate(EXERANY2 = case_when(EXERANY2 == 1 ~"yes",
                              EXERANY2 == 2 ~"no"))%>%
ggplot(aes(x = SLEPTIM1,y=`_BMI5`, color = EXERANY2)) + geom_point() +
geom_smooth(method = "lm", se = FALSE) + theme_classic() +
labs(x = "sleephour", y = "_BMI", title = "Relationship between the Age (in years) of respondent and
     Average diastolic blood pressure value in different sex")
<<<<<<< HEAD
```
=======
```
>>>>>>> 4495022f52ce520374295949736ed795492ccf32
