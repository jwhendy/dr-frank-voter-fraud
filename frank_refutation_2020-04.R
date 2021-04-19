## setup                                                            :noexport:
library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(readxl)
library(tidyr)
library(tibble)

## Examining the population data

## To reproduce Dr. Frank's plots, we must scale population totals for an /age range/ into an
## estimate for an /individual age/. To do this, I used the following method per range:
## - ~x = (min_age + max_age)/2~ (mean age for the range)
## - ~y = population / (max_age - min_age)~ (total divided by number of ages represented)
## - for the 85+ age group, ~x=92.5~ and ~y=population/15~ were used to distribute the
##   population over the range of 85-100yrs.

## initial attempt to scale group population to individual ages
mi_pop <- read_csv("data/census/mi_pop_census.csv") %>%
  select(3:7, 25, 19, 31, 34, 37, 52, 55, 58, 61, 64,
         67, 70, 73, 76, 79, 82, 85, 88, 91) %>%
  filter(YEAR==12) %>%
  mutate(AGE1819 = AGE1824_TOT-AGE2024_TOT) %>%
  select(CTYNAME, AGE1819, AGE2024_TOT:AGE85PLUS_TOT) %>%
  pivot_longer(cols=!CTYNAME) %>%
  group_by(CTYNAME) %>%
  mutate(age_i = c(18.5, seq(22, 82, by=5), 92.5),
         val_i = value/c(2, rep(5, 13), 15)) %>%
  ungroup() %>%
  add_column(src="Census")



## Dr. Frank's plots were converted using WebPlotDigitizer, saving one file for
## each curve, per county. WebPlotDigitizer finds a curve matching a target color and
## overlays points at fixed intervals on top. As a result, the intervals do not
## align to ages, so values were interpolated to force alignment to fixed integer ages from
## 18-100. 

## reading in WebPlotDigitizer output from each plot in the DePerno brief
file_list <- c("antrim", "barry", "charlevoix", "livingston", "kent",
               "macomb", "oakland", "wayne", "grand_traverse")
d_list <- lapply(file_list, function(f) {
  pop <- read_csv(paste0("data/extracted/", f, "_pop.csv"),
                  col_names=c("age", "value")) %>% add_column(var="pop")
  reg <- read_csv(paste0("data/extracted/", f, "_reg.csv"),
                  col_names=c("age", "value")) %>% add_column(var="reg")
  pred <- read_csv(paste0("data/extracted/", f, "_pred.csv"),
                   col_names=c("age", "value")) %>% add_column(var="pred")
  vote <- read_csv(paste0("data/extracted/", f, "_vote.csv"),
                   col_names=c("age", "value")) %>% add_column(var="vote")
  
  return(rbind(pop, reg, pred, vote) %>%
           add_column(county=f))
})

df <- do.call(rbind, d_list) %>%
  mutate(CTYNAME = recode(county,
                      `antrim`="Antrim County", `barry`="Barry County",
                      `charlevoix` = "Charlevoix County",
                      `grand_traverse`="Grand Traverse County",
                      `livingston` = "Livingston County",
                      `kent` = "Kent County", `macomb`="Macomb County",
                      `oakland`="Oakland County", `wayne`="Wayne County")
                      ) %>%
  arrange(CTYNAME, var, age) %>%
  group_by(CTYNAME, var) %>%
  summarise(age_i = approx(age, value, xout=18:100)$x,
            val_i = approx(age, value, xout=18:100)$y) %>%
  ungroup()



## With both the Census Data and the extracted Antrim County population curve from
## Dr. Frank's plot, we can compare the two results:

# plotting initial census population estimate vs. Dr. Frank's for Antrim
mi_pop %>%
  filter(CTYNAME=="Antrim County") %>%
  select(CTYNAME, age_i, val_i, src) %>%
  ggplot(aes(x=age_i, y=val_i, color=src)) + geom_line(size=1) + geom_point() +
  geom_line(aes(x=age_i, y=val_i, color=src),
            data = df %>%
              filter(var=="pop", CTYNAME=="Antrim County") %>%
              add_column(src = "Dr. Frank")) +
  facet_wrap(~CTYNAME, scales="free_y") +
  scale_x_continuous("age") + scale_y_continuous("population") +
  theme_bw() + scale_color_discrete("")


## This checks out delightfully well, and with some guess and check it appears that
## Dr. Frank used =[23, 28, ..., 83, 91]= for the =x= values for each age group. Other than
## disagreement about the low end (perhaps he did not compute the 18-19yr olds
## specifically as I did) and the details of his smoothed curve, we have ~100% alignment on
## all key points. 

# re-processing MI census data to adjust to Dr. Frank's key points per age range
mi_pop <- mi_pop %>%
  group_by(CTYNAME) %>%
  mutate(age_i = c(18, seq(23, 83, by=5), 92),
         val_i = value/c(2, rep(5, 13), 15)) %>%
  ungroup()

mi_pop %>%
  filter(CTYNAME=="Antrim County") %>%
  select(CTYNAME, age_i, val_i, src) %>%
  ggplot(aes(x=age_i, y=val_i, color=src)) + geom_line(size=1) + geom_point() +
  geom_line(aes(x=age_i, y=val_i, color=src),
            data = df %>%
              filter(var=="pop", CTYNAME=="Antrim County") %>%
              add_column(src = "Dr. Frank")) +
  facet_wrap(~CTYNAME, scales="free_y") +
  scale_x_continuous("age") + scale_y_continuous("population") +
  theme_bw() + scale_color_discrete("")

## A note on turnout

## Here I use the data extracted from Dr. Frank's Antrim County plot to create a 6th
## degree polymial key for the vote predicting model: =population * VAP_turnout * key=.

## predicting votes based on population instead of registrations
df_key <- df %>%
  filter(CTYNAME=="Antrim County") %>%
  pivot_wider(id_cols=c(CTYNAME, age_i),
              names_from=var, values_from=val_i) %>%
  select(CTYNAME, age_i, pop, pred, vote)

fit <- lm(vote ~ poly(pop * 0.835, 6), data=df_key)
df_key <- df_key %>% add_column(pred2 = predict(fit, df_key))



## What does this prediction look like?

## plot of population-based prediction vs. original
df_key %>%
  pivot_longer(cols=!c(CTYNAME, age_i)) %>%
  filter(name != "pred") %>%
  ggplot(aes(x=age_i, y=value, color=name)) + geom_line() +
  scale_y_continuous("count") +
  scale_color_manual("",
                     breaks=c("pop", "pred2", "vote"),
                     labels=c("Dr. Frank pop", "Pop key model",
                              "Votes"),
                     values=c("blue", "green3", "red")) +
  theme_bw()


## Comapred to the original Antrim County R-value of 0.993, how does this smooth prediction fare?
cor(df_key$pred2, df_key$vote)

## #+RESULTS:
## : [1] 0.9739703

## But /does/ it work in every county in Michigan? We don't have data for votes and
## registrations except for the nine counties analyzed, but we /do/ have Census age
## data covering all MI counties. This allows us to look at the distribution of age as a
## percent of the total population per county.

## looking at age group proportions for all MI counties
mi_cty_all <- read_csv("data/census/mi_pop_census.csv") %>%
  select(3:7, 25, 19, 31, 34, 37, 52, 55, 58, 61, 64,
         67, 70, 73, 76, 79, 82, 85, 88, 91) %>%
  filter(YEAR==12) %>%
  mutate(AGE1819 = AGE1824_TOT-AGE2024_TOT) %>%
  select(CTYNAME, AGE1819, AGE2024_TOT:AGE85PLUS_TOT) %>%
  pivot_longer(cols=!CTYNAME) %>%
  group_by(CTYNAME) %>%
  mutate(age_i = c(18, seq(20, 80, by=5), 85)) %>%
  mutate(val_i = value/c(2, rep(5, 13), 15))

mi_cty_all %>% group_by(CTYNAME) %>%
  mutate(perc = val_i/sum(val_i),
         target = ifelse(CTYNAME %in% c("Antrim County", "Barry County",
                                        "Charlevoix County",
                                        "Grand Traverse County",
                                        "Kent County",
                                        "Livingston County",
                                        "Macomb County",
                                        "Oakland County",
                                        "Wayne County"), "Y", "N"),
         target = recode(target, N="Others", Y="Nine counties analyzed")) %>%
  ggplot(aes(x=age_i, y = perc, group = CTYNAME)) + geom_line(size=0.7, alpha=0.5) +
  facet_wrap(~target) +
  scale_x_continuous("age") + theme_bw() + scale_y_continuous("fraction of population")

## The nine counties look rather "average" compared to some of the extremes present in
## others. Ultimately, the polynomial is simply modeling turnout, and as long as this is
## rather consistent by age across the counties, changes in age
## demographics within a county are irrelavent. More population of a certain age yields more registrations at
## that age, which is multiplied by the polynomial model for turnout, and yields more votes
## for that age. That said, factors affecting turnout among an age group (e.g. a college campaign
## encouraging students to vote, or increasing voting accessibility for a certain age group)
## /would/ deviate from the state mean turnout by age, which is what this polynomial is
## fitting. Thus, these more extreme counties /may/ still break the mold.

## finding which populations have the greatest proportion of <30yrs and >65yrs
mi_cty_all %>% group_by(CTYNAME) %>%
  mutate(perc = val_i/sum(val_i)) %>%
  filter(age_i < 30) %>%
  summarize(perc = round(sum(perc), 3)) %>%
  ungroup() %>%
  arrange(desc(perc)) %>% head()

mi_cty_all %>% group_by(CTYNAME) %>%
  mutate(perc = val_i/sum(val_i)) %>%
  filter(age_i > 64) %>%
  summarize(perc = round(sum(perc), 3)) %>%
  ungroup() %>%
  arrange(desc(perc)) %>% head()


## The correlation coefficient is more like asking "do these variables move
## together in the same patterns?" This may seem like a nuance (doesn't moving together imply
## that one predicts the other?), but I will demonstrate that R-value does not imply accuracy
## with respect to error. Using extracted Antrim County data I computed an R-value
## of 0.990 (vs. 0.993 by Dr. Frank). This was sufficient to validate the data as reasonable
## in light of it being extracted from a screenshot. Ages were filtered to \(\leq\)90 because 
## extraction was suboptimal on the tails where curves overlapped, and error would be
## proportionally larger for such small values.

## validating cor() for data extracted from plots for Antrim
df_pred <- df %>%
  filter(CTYNAME=="Antrim County", var %in% c("reg", "vote", "pred")) %>%
  pivot_wider(id_cols=c(CTYNAME, age_i), names_from=var, values_from=val_i) %>%
  filter(age_i < 91)

cor(df_pred$pred, df_pred$vote)

## #+RESULTS:
## [1] 0.9901213

## Above, using population as a predictor resulted in a smoother curve that was still
## correlative (R=0.97), but the resulting prediction was still quite in line with actual
## votes. What if we used a different key? Here, I apply a uniform sequence from 4 through 8
## (same length as the data):

## creating a completely bogus key
df_pred <- df_pred %>%
  mutate(pred2 = reg * seq(4, 8, length=length(reg)))


## Plotting this new prediction along with Dr. Franks data yields the following result:

## showing awful prediction vs. reality
df_pred %>% pivot_longer(cols=c(reg, pred, pred2, vote)) %>%
  ggplot(aes(x=age_i, y=value, color=name)) + geom_line(size=1) +
  facet_wrap(~CTYNAME) +
  scale_x_continuous("age") + scale_y_continuous("population") +
  scale_color_discrete(breaks=c("reg", "vote", "pred", "pred2"),
                       labels=c("registered", "votes", "Dr. Frank", "my key")) +
  theme_bw()


## Could this prediction be described as "accurate" or "precise"? Yet what do we find?
cor(df_pred$pred2, df_pred$vote)

## #+RESULTS:
## [1] 0.9930492


## How... is this possible? The correlation coefficient is useful for, well, correlating, and
## the data /do/ correlate: "The red line is almost a direct image of the black line, but
## just lower on the graph." (pg 11) My result is just higher on the graph. This toy example
## illustrates that correlation does not mean accurate. We can verify this further via a
## residual plot of ~error = (prediction-actual)~:[fn:10] 

## residual plot
df_pred %>%
  mutate(error = pred-vote) %>%
ggplot(aes(x=age_i, y=error)) + geom_point() + 
  scale_x_continuous("age") +
  scale_y_continuous("error, prediction-votes") +
  theme_bw()


## We're seeing errors of +30 to -20 across the range of ages, and if you didn't take note in
## earlier plots, Antrim County is /tiny/. Here's the prediction error as a percent of the
## voters at each age, highlighting that "99.3% correlation" can still manage -15 to 25%
## error.

## calculating absolute and % rmse for Dr. Franks predictions
df_pred %>%
  mutate(error = vote-pred,
         perc = (pred-vote)/vote) %>%
  summarise(
    rmse_err = sqrt(sum(error**2)/length(error)),
    rmse_perc = sqrt(sum(perc**2)/length(perc)))


## re-plotting residuals as % of age group population
df_pred %>%
  mutate(perc = (pred-vote)/vote) %>%
ggplot(aes(x=age_i, y=perc)) + geom_point() + 
  scale_x_continuous("age") +
  scale_y_continuous("error, (prediction-votes)/votes") +
  theme_bw()

## The residuals indicate a bias in the prediction. They should center about the line
## ~error=0~, be randomly distributed, and have no obvious trend across the independent variable,
## =age=. We have a downward trend in this case, which I'll point out is in the opposite
## direction of what this theory proposes. The key is creating a prediction that's /too high/ given
## the actual result for younger ages (+error) and /too low/ for older ages (-error).

## Root Mean Squared Error is a better assessment of accuracy, and is as follows for all 9
## counties, computed using Dr. Frank's prediction (light blue) and reported votes (red) in
## the plots:

## calculating RSME for all counties
df %>%
  filter(age_i < 91) %>%
  pivot_wider(id_cols=c(CTYNAME, age_i),
              names_from=var, values_from=val_i) %>%
  group_by(CTYNAME) %>%
  mutate(error = pred-vote,
         perc=(pred-vote)/vote) %>%
  summarise(rmse = round(sqrt(sum(error**2)/length(error)), 1),
            rmse_perc = round(sqrt(sum(perc**2)/length(perc)), 3))

## Taking a step back

## The root of this trick is that the population, as plotted, doesn't look like the other curves.
## How /could/ we find out if both registrations and votes truly looked like the population?
## The Census has election data for single years of age all the way back to 1998, containing
## Voting Aged Population (VAP), Voting Eligible Population (VEP), registered voters, and
## votes cast.[fn:13]  

## processing all Census Bureau elections data back to 1998
new_names <- c("age", "VAP", "VEP", "reg_n", "reg_perc",
               "vote_n", "vote_perc","reg_pop_perc", "vote_pop_perc")
new_list <- list(
  "2018" = list("year" = 2018, "file" = "2018.xlsx", "range" = "B16:R79",
                "select" = c(1:5, 10:11, 16:17), "names" = new_names),
  "2016" = list("year" = 2016, "file" = "2016.xlsx", "range" = "B16:R79",
                "select" = c(1:5, 10:11, 16:17), "names" = new_names),
  "2014" = list("year" = 2014, "file" = "2014.xls", "range" = "A19:Q82",
                "select" = c(1:5, 10:11, 16:17), "names" = new_names),
  "2012" = list("year" = 2012, "file" = "2012.xls", "range" = "A19:Q82",
                "select" = c(1:5, 10:11, 16:17), "names" = new_names),
  "2010" = list("year" = 2010, "file" = "2010.xls", "range" = "B15:R78",
                "select" = c(1:5, 10:11, 16:17), "names" = new_names),
  "2008" = list("year" = 2008, "file" = "2008.xls", "range" = "A19:M82",
                "select" = c(1:5, 8:9, 12:13), "names" = new_names),
  "2006" = list("year" = 2006, "file" = "2006.xls", "range" = "A20:M83",
                "select" = c(1:5, 8:9, 12:13), "names" = new_names))
new_d_list <- lapply(new_list, function(f) {
  d <- read_excel(paste0("data/voters/", f[["file"]]),
                  range = f[["range"]], col_names=FALSE) %>%
    select(f[["select"]])
  names(d) <- f[["names"]]
  d <- d %>% add_column(year=f[["year"]], .before="age")
  return(d)
})

old_names <- c("age", "VAP", "reg_n", "reg_pop_perc", "vote_n", "vote_pop_perc", "n_ctzn")
old_list <- list(
  "2004" = list("year" = 2004, "file" = "2004.xls", "range" = "A20:M83",
                "select" = c(1:4, 7:8, 13), "names" = old_names),
  "2002" = list("year" = 2002, "file" = "2002.xls", "range" = "A19:M86",
                "select" = c(1:4, 7:8, 13), "names" = old_names),
  "2000" = list("year" = 2000, "file" = "2000.xls", "range" = "A22:Q88",
                "select" = c(1:4, 7:8, 13), "names" = old_names),
  "1998" = list("year" = 1998, "file" = "1998.xls", "range" = "B26:N93",
                "select" = c(1:4, 7:8, 13), "names" = old_names)
)
old_d_list <- lapply(old_list, function(f) {
  d <- read_excel(paste0("data/voters/", f[["file"]]),
                  range = f[["range"]], col_names=FALSE) %>%
    select(f[["select"]])
  names(d) <- f[["names"]]
  
d <- d %>%
  mutate(VEP=VAP-n_ctzn, reg_perc=reg_n/VEP, vote_perc=vote_n/VEP) %>%
  select(age, VAP, VEP, reg_n, reg_perc, vote_n, vote_perc, reg_pop_perc, vote_pop_perc) %>%
  add_column(year=f[["year"]], .before="age")
  
  return(d)
})

us_reg <- do.call(rbind, new_d_list) %>%
  add_row(do.call(rbind, old_d_list)) %>%
  group_by(year) %>%
  mutate(age_i = 17+(1:length(age))) %>%
  ungroup() %>% 
  filter(age_i < 80)

## Here's what that looks like:

## plotting VAP, VEP, registrations and votes across elections
us_reg_m <- us_reg %>%
  select(year, age_i, VAP, VEP, reg_n, vote_n) %>%
  pivot_longer(cols=!c(year, age_i))

us_reg_m %>% 
  ggplot(aes(x=age_i, y=value, color=name, group=name)) +
  geom_line(size=0.5) +
  facet_wrap(~year) +
  scale_x_continuous("age") + scale_y_continuous("count") +
  scale_color_manual("",
                     breaks=c("VAP", "VEP", "reg_n", "vote_n"),
                     labels=c("VAP", "VEP", "Registered", "Votes"),
                     values=c("blue", "green4", "black", "red")) +
  theme_bw()

## Reversing the illusion

## Now we can take the US population by age and (a) scale to match the total county
## population, and (b) adjust each age range according to the scaling factor. This latter
## adjustment preserves the relative "shape" of the US distribution (e.g. 73yr olds as more populous
## than 72 or 74yr olds) while accounting for a county a having higher or lower relative
## proportion of individuals in the 70-74yr old age group.

## With the values above, we now use the age pyramid data for a specific age, =us_age_pop=, to
## predict the county population for that same age, =cty_age_pop=, in the following manner:

## ~cty_age_pop = cty_pop_total/us_pop_total * us_age_pop * scale_factor~

## mapping individual age-pyramid data to MI counties
mi_pop <- read_csv("data/census/mi_pop_census.csv") %>%
  select(3:7, 25, 19, 31, 34, 37, 52, 55, 58, 61, 64,
         67, 70, 73, 76, 79, 82, 85, 88, 91) %>%
  filter(YEAR==12, CTYNAME %in% c("Antrim County", "Barry County",
                                  "Charlevoix County", "Grand Traverse County",
                                  "Livingston County", "Kent County",
                                  "Macomb County", "Oakland County", "Wayne County")) %>%
  mutate(AGE1819 = AGE1824_TOT-AGE2024_TOT) %>%
  select(CTYNAME, AGE1819, AGE2024_TOT:AGE85PLUS_TOT) %>%
  pivot_longer(cols=!CTYNAME) %>%
  group_by(CTYNAME) %>%
  mutate(age_i = c(18, seq(20, 80, by=5), 85)) %>%
  mutate(val_i = value/c(2, rep(5, 13), 15)) %>%
  add_column(src="Census")

us_pop_groups <- read_csv("data/census/us_pop_groups.csv", skip=11, n_max=19,
         col_names=c("group", "pop")) %>% 
  filter(!is.na(group),
         !(group %in% c(".Under 15 years", ".15 to 17 years"))) %>%
  mutate(group = recode(group,
         `.20 to 24 years` = "AGE2024_TOT",
         `.25 to 29 years` = "AGE2529_TOT",
         `.30 to 34 years` = "AGE3034_TOT",
         `.35 to 39 years` = "AGE3539_TOT",
         `.40 to 44 years` = "AGE4044_TOT",
         `.45 to 49 years` = "AGE4549_TOT",
         `.50 to 54 years` = "AGE5054_TOT",
         `.55 to 59 years` = "AGE5559_TOT",
         `.60 to 64 years` = "AGE6064_TOT",
         `.65 to 69 years` = "AGE6569_TOT",
         `.70 to 74 years` = "AGE7074_TOT",
         `.75 to 79 years` = "AGE7579_TOT",
         `.80 to 84 years` = "AGE8084_TOT",
         `.85 years and over` = "AGE85PLUS_TOT",
         `.18 to 20 years` = "AGE1820_TOT",
         `.21 to 44 years` = "AGE2144_TOT")) %>%
  pivot_wider(values_from=pop, names_from=group) %>%
  mutate(AGE1819 = sum(AGE1820_TOT, AGE2144_TOT) -
         sum(AGE2024_TOT, AGE2529_TOT, AGE3034_TOT,
             AGE3539_TOT, AGE4044_TOT)) %>% 
  select(AGE1819, AGE2024_TOT:AGE85PLUS_TOT) %>%
  pivot_longer(cols=AGE1819:AGE85PLUS_TOT)

us_pop_m <- read_csv("data/census/us_pop_males.csv", col_names = c("bar", "pop")) %>%
  mutate(age=100:0, sex="m")
us_pop_f <- read_csv("data/census/us_pop_females.csv", col_names = c("bar", "pop")) %>%
  mutate(age=100:0, pop=-pop, sex="f")
us_pop <- rbind(us_pop_m, us_pop_f) %>%
  group_by(age) %>% summarise(pop=sum(pop)) %>%
  filter(age>17, age < 91)

mi_pop_all <- expand.grid(
  age = 18:90,
  CTYNAME = mi_pop %>% pull(CTYNAME) %>% unique()) %>%
  merge(mi_pop %>% merge(us_pop_groups, by="name"),
        by.x = c("CTYNAME", "age"), 
        by.y = c("CTYNAME", "age_i"),  all.x=T) %>%
  fill(val_i, name, value.x, value.y, src) %>%
  group_by(CTYNAME) %>%
  mutate(perc = (value.x/sum(value.x)) / (value.y/sum(value.y))) %>%
  ungroup()

## How does this look in practice for Antrim County?

## showing this plot for Antrim vs. smoothed population used by Dr. Frank
mi_pop_all %>%
  merge(us_pop, by="age", all.x=T) %>% 
  arrange(CTYNAME) %>% 
  filter(CTYNAME=="Antrim County") %>%
  mutate(pred = sum(val_i)/sum(pop) * pop * perc) %>%
  ggplot(aes(x=age, y=pred, color=src)) + geom_line() + facet_wrap(~CTYNAME) +
  geom_line(aes(x=age_i, y=val_i, color=var),
            data=df %>% filter(CTYNAME=="Antrim County", var=="pop")) +
  geom_line(aes(x=age_i, y=val_i, color=var), 
            data=df %>% filter(CTYNAME=="Antrim County", var=="vote")) +
  geom_line(aes(x=age_i, y=val_i, color=var), 
            data=df %>% filter(CTYNAME=="Antrim County", var=="reg")) +
  scale_color_manual("", breaks=c("pop", "Census", "reg", "vote"),
                     labels=c("Dr. Frank pop", "Census model", "registered", "vote"),
                     values=c("blue", "green3", "black", "red")) +
  scale_y_continuous("count") +
  theme_bw()


## For starters, the statement is patently false: it's only /apparently/ true in
## Antrim, Charlevoix, and Grand Traverse, which happen to be among the smallest counties
## (more sensitive to error). Even so, we can see that this derived population removed the
## more prominent of these occurrences in Antrim. The same is true when
## we replicate across all counties in Dr. Frank's data set. Note the superior fit to both
## registrations and votes vs. the smoothed population curve.

## all counties shown with this derived population
mi_pop_all %>% 
  merge(us_pop, by="age", all.x=T) %>% 
  arrange(CTYNAME) %>% 
  group_by(CTYNAME) %>%
  mutate(pred = sum(val_i)/sum(pop) * pop * perc) %>% ungroup() %>%
  ggplot(aes(x=age, y=pred, color=src)) + geom_line() +
  facet_wrap(~CTYNAME, scales="free_y") +
  geom_line(aes(x=age_i, y=val_i, color=var), alpha=0.5,
            data=df %>% filter(var=="pop")) +
  geom_line(aes(x=age_i, y=val_i, color=var), 
            data=df %>% filter(var=="vote")) +
  geom_line(aes(x=age_i, y=val_i, color=var), 
            data=df %>% filter(var=="reg")) +
  scale_color_manual("", breaks=c("pop", "Census", "reg", "vote"),
                     labels=c("Dr. Frank pop", "Census model", "registered", "vote"),
                     values=c("blue", "green4", "black", "red")) +
  scale_y_continuous("count") +
  theme_bw()



## With more accurate data I'm confident we would see that in no cases did "votes exceed the
## population" as is claimed. Moreover, by using this population model we can see that
## these "spikes" and "notches" are just people who exist in the population,
## who register to vote, and who vote. 

## zooming in on a notable "spike" to show derived populations for all counties also have it
mi_pop_all %>% 
  merge(us_pop, by="age", all.x=T) %>% 
  arrange(CTYNAME) %>% 
  group_by(CTYNAME) %>%
  mutate(pred = sum(val_i)/sum(pop) * pop * perc) %>% ungroup() %>%
  filter(age > 69, age < 81) %>%
  ggplot(aes(x=age, y=pred, color=src, alpha=src)) + geom_line() +
  facet_wrap(~CTYNAME, scales="free_y") +
  geom_line(aes(x=age_i, y=val_i, color=var, alpha=var),
            data=df %>% filter(var=="pop", age_i < 81, age_i > 69)) +
  geom_line(aes(x=age_i, y=val_i, color=var, alpha=var), 
            data=df %>% filter(var=="vote", age_i < 81, age_i > 69)) +
  geom_line(aes(x=age_i, y=val_i, color=var, alpha=var), 
            data=df %>% filter(var=="reg", age_i < 81, age_i > 69)) +
  scale_color_manual("", breaks=c("pop", "Census", "reg", "vote"),
                     labels=c("Dr. Frank pop", "Census model", "registered", "vote"),
                     values=c("blue", "green4", "black", "red")) +
  scale_alpha_manual(breaks=c("pop", "Census", "reg", "vote"),
                     values=c(0.6, 1, 1, 1), guide=F) + 
  scale_y_continuous("count") +
  theme_bw()

## The student becomes the teacher

## Here is another statistic you have never checked: the united states algorithmically controls the
## issuance of drivers' licenses across age groups according to the proportion of female
## smokers.[fn:28][fn:29] I used data from 2009 on smokers by age, and data from 2010 on
## drivers by age to discover that a 6th degree polynomial is being used to regulate the 
## issuance of licenses in the United States with 99.3% \xcancel{correlation} absolute
## precision.  

## reading in US population by age group, smoking data, and data on age of drivers
us_pop_2010 <- read_csv("data/census/us_pop_groups.csv", skip=10, n_max=20,
         col_names=c("group", "all_n", "all_p", "male", "m_p", "female", "f_p")) %>%
  select(group, male, female)  %>%
  filter(!is.na(group)) %>%
  pivot_longer(cols=c(male, female)) %>%
  filter(name=="female") %>%
  mutate(group = recode(group,
         `.20 to 24 years` = "a2024", `.25 to 29 years` = "a2529",
         `.30 to 34 years` = "a3034", `.35 to 39 years` = "a3539",
         `.40 to 44 years` = "a4044", `.45 to 49 years` = "a4549",
         `.50 to 54 years` = "a5054", `.55 to 59 years` = "a5559",
         `.60 to 64 years` = "a6064", `.65 to 69 years` = "a6569",
         `.70 to 74 years` = "a7074", `.18 to 20 years` = "a1820",
         `.21 to 44 years` = "a2144")) %>% 
  pivot_wider(id_cols=name, values_from=value, names_from=group) %>% 
  mutate(a1819 = sum(a1820, a2144) -
           sum(a2024, a2529, a3034, a3539, a4044),
         a1824 = a1819 + a2024) %>%
  select(name, a1824, a2529:a7074) %>%
  rename(sex=name) %>%
  pivot_longer(cols=!sex)


smk <- read_csv("data/misc/gallup_smoking_males_2009.csv",
         col_names=c("age", "perc")) %>%
  add_column(sex="male") %>%
  add_row(read_csv("data/misc/gallup_smoking_females_2009.csv",
                   col_names=c("age", "perc")) %>%
          add_column(sex="female")) %>%
  filter(sex == "female") %>%
  group_by(sex) %>%
  summarise(age_i = approx(age, perc, 18:74)$x,
            perc_i = approx(age, perc, 18:74)$y) %>%
  mutate(age_grp = cut(age_i, include.lowest=T,
                       breaks=c(18, seq(24, 74, by=5)),
                       labels=c("a1824", "a2529", "a3034", "a3539", "a4044",
                                "a4549", "a5054", "a5559", "a6064", "a6569",
                                "a7074"))) %>%
  group_by(sex, age_grp) %>%
  summarise(smk = mean(perc_i)) %>%
  ungroup() %>%
  merge(us_pop_2010, by.x=c("sex", "age_grp"), by.y=c("sex", "name")) %>%
  mutate(smk_n = smk*value*1000) %>%
  group_by(age_grp) %>%
  summarise(smk = sum(smk_n)) %>%
  ungroup()

drv <- read_excel("data/misc/drivers-by-age_2010.xls", range="A14:J32", col_names=F) %>%
  select(1, 8) %>% rename(age = "...1", drv = "...8") %>%
  pivot_wider(names_from=age, values_from=drv) %>%
  mutate(`a1824` = `18` + `19` + `(20-24)`) %>%
  select(`a1824`, `25-29`:`70-74`) %>%
  pivot_longer(cols=`a1824`:`70-74`) %>%
  mutate(age_grp = recode(name,
                          `25-29` = "a2529", `30-34` = "a3034", `35-39` = "a3539",
                          `40-44` = "a4044", `45-49` = "a4549", `50-54` = "a5054",
                          `55-59` = "a5559", `60-64` = "a6064", `65-69` = "a6569",
                          `70-74` = "a7074")) %>%
  rename(drv = "value")


## After reading in the raw data (smoking data extracted from the plot using
## WebPlotDigitizer), I aligned age groups, merged the data, and applied a 6th degree
## polynomial model to find the following correlation value:

## merging data sets, fitting polynomial scaling factor, finding correlation
smk_drv <- smk %>% select(age_grp, smk) %>%
  merge(drv %>% select(age_grp, drv),
        by="age_grp")

fit <- lm(drv ~ poly(smk, 6), data=smk_drv)
smk_drv <- smk_drv %>%
  mutate(pred = predict(fit, smk_drv))

cor(smk_drv$pred, smk_drv$drv)

## #+RESULTS:
## : [1] 0.9926732

## computing RMSE
smk_drv %>%
  mutate(error = (pred-drv)/drv) %>%
  summarise(rmse_perc = sqrt(sum(error**2)/length(error))

## #+RESULTS:
## :    rmse_perc
## : 1 0.02659507

## How does our prediction look (root mean squared error = 2.7%, btw)?

## plotting the predicted drivers vs. drivers
smk_drv %>%
  mutate(x = 1:length(age_grp)) %>%
  pivot_longer(cols = !c(age_grp, x)) %>%
  ggplot(aes(x=x, y=value, group=name, color=name, size=name)) + geom_line(size=1) +
  scale_x_continuous("age group",
                     breaks=1:11,
                     labels=smk_drv %>% pull(age_grp)) +
  scale_color_discrete("", breaks=c("smk", "drv", "pred"),
                       labels=c("smokers", "drivers", "predicted drivers")) +
  scale_y_continuous("population") +
  theme_bw()
