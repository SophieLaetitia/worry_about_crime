#Weighted Analysis

#### Visualisation #### 
rm(list = ls())
setwd("~/Desktop/Dissertation/Weighted_Data")

library(readxl)
final <- read_excel("final_weighted_data20240421.xlsx")
library(Kendall)
library(xts)
library(collapse)
library(modifiedmk)
library(ggplot2)
library(tidyverse)
library(jtools)


ggplot(final, aes(x = year, group = 1)) +
  geom_line(aes(y = mean_wburgl, colour = "Worry about burglary")) +
  geom_errorbar(aes(ymin = `CI_lo_wburgl`, ymax = `CI_up_wburgl`), colour = "black", width = .1) +
  geom_line (aes(y=mean_wmugged, colour = "Worry about being mugged")) +
  geom_errorbar(aes(ymin = `CI_lo_wmugged`, ymax = `CI_up_wmugged`), colour = "black", width = .1) + 
  geom_line(aes(y = mean_wcarstol, colour = "Worry about car theft")) +
  geom_errorbar(aes(ymin = `CI_lo_wcarstol`, ymax = `CI_up_wcarstol`), colour = "black", width = .1) +
  geom_line (aes(y=mean_wfromcar, colour = "Worry about theft from car")) +
  geom_errorbar(aes(ymin = `CI_lo_wfromcar`, ymax = `CI_hi_wfromcar`), colour = "black", width = .1) + 
  geom_line(aes(y = mean_wraped, colour = "Worry about rape")) +
  geom_errorbar(aes(ymin = `CI_lo_wraped`, ymax = `CI_hi_wraped`), colour = "black", width = .1) +
  geom_line (aes(y=mean_wattack, colour = "Worry about assault")) +
  geom_errorbar(aes(ymin = `CI_lo_wattack`, ymax = `CI_hi_wattack`), colour = "black", width = .1)+
  geom_line (aes(y=mean_wraceatt, colour = "Worry about being a victim of a hate crime")) +
  geom_errorbar(aes(ymin = `CI_lo_wraceatt`, ymax = `CI_hi_wraceatt`), colour = "black", width = .1) +
  geom_line (aes(y=mean_wcyber, colour = "Worry about cybercrime")) +
  geom_errorbar(aes(ymin = `CI_lo_wcyber`, ymax = `CI_up_wcyber`), colour = "black", width = .1) +
  geom_line (aes(y=mean_wident, colour = "Worry about identity fraud")) +
  geom_errorbar(aes(ymin = `CI_lo_wident`, ymax = `CI_up_wident`), colour = "black", width = .1) +
  geom_line (aes(y=mean_wfraud, colour = "Worry about fraud")) +
  geom_errorbar(aes(ymin = `CI_lo_wfraud`, ymax = `CI_up_wfraud`), colour = "black", width = .1) +
  geom_line (aes(y=overall_worry_score, colour = "Overall worry about crime")) +
  scale_color_manual(name = "Different types of worry", values = c("Worry about burglary" = "lightblue", "Worry about being mugged" = "darkblue", "Worry about car theft" = "lightgreen", "Worry about theft from car" = "darkgreen", "Worry about rape" = "yellow", "Worry about assault" = "orange", "Worry about being a victim of a hate crime" = "red", "Worry about cybercrime" = "pink", "Worry about identity fraud" = "purple", "Worry about fraud" = "gray", "Overall worry about crime" = "Black" )) +
  theme_apa() + 
  ylim(1.5,3) +
  ggtitle("Worry about different crimes from 1998 to 2019/20") +
  labs(x = "Year",
       y = "Worry about crime")


#Gender differences 

#Worry of burglary
ggplot(final, aes(x = year, group = 1)) +
  geom_line(aes(y = mean_wburgl, colour = "Overall average")) +
  geom_errorbar(aes(ymin = `CI_lo_wburgl`, ymax = `CI_up_wburgl`), colour = "black", width = .1) +
  geom_line (aes(y=female_mean_wburgl, colour = "Women")) +
  geom_errorbar(aes(ymin = `female_CI_lo_wburgl`, ymax = `female_CI_up_wburgl`), colour = "black", width = .1) + 
  geom_line(aes(y = male_mean_wburgl, colour = "Men")) +
  geom_errorbar(aes(ymin = `male_CI_lo_wburgl`, ymax = `male_CI_up_wburgl`), colour = "black", width = .1) +
  scale_color_manual(name = "Groups", values = c("Women" = "indianred", "Men" = "cornflowerblue", "Overall average" = "black")) +
  theme_apa() + 
  ggtitle("Worry about burglary among men and women from 1998 to 2019/20") +
  labs(x = "Year",
       y = "Worry about burglary")

#Worry of being mugged

ggplot(final, aes(x = year, group = 1)) +
  geom_line(aes(y = mean_wmugged, colour = "Overall average")) +
  geom_errorbar(aes(ymin = `CI_lo_wmugged`, ymax = `CI_up_wmugged`), colour = "black", width = .1) +
  geom_line (aes(y=female_mean_wmugged, colour = "Women")) +
  geom_errorbar(aes(ymin = `female_CI_lo_wmugged`, ymax = `female_CI_up_wmugged`), colour = "black", width = .1) + 
  geom_line(aes(y = male_mean_wmugged, colour = "Men")) +
  geom_errorbar(aes(ymin = `male_CI_lo_wmugged`, ymax = `male_CI_up_wmugged`), colour = "black", width = .1) +
  scale_color_manual(name = "Groups", values = c("Women" = "indianred", "Men" = "cornflowerblue", "Overall average" = "black")) +
  ylim(1.2,2.8) +
  theme_apa() + 
  ggtitle("Worry about being mugged among men and women from 1998 to 2019/20") +
  labs(x = "Year",
       y = "Worry about being mugged")

#Worry of car getting stolen

ggplot(final, aes(x = year, group = 1)) +
  geom_line(aes(y = mean_wcarstol, colour = "Overall average")) +
  geom_errorbar(aes(ymin = `CI_lo_wcarstol`, ymax = `CI_up_wcarstol`), colour = "black", width = .1) +
  geom_line (aes(y=female_mean_wcarstol, colour = "Women")) +
  geom_errorbar(aes(ymin = `female_CI_lo_wcarstol`, ymax = `female_CI_up_wcarstol`), colour = "black", width = .1) + 
  geom_line(aes(y = male_mean_wcarstol, colour = "Men")) +
  geom_errorbar(aes(ymin = `male_CI_lo_wcarstol`, ymax = `male_CI_up_wcarstol`), colour = "black", width = .1) +
  scale_color_manual(name = "Groups", values = c("Women" = "indianred", "Men" = "cornflowerblue", "Overall average" = "black")) +
  ylim(1.2,2.8) +
  theme_apa() + 
  ggtitle("Worry about car theft among men and women from 1998 to 2019/20") +
  labs(x = "Year",
       y = "Worry about getting car stolen")

#Worry of getting something stolen from their car
ggplot(final, aes(x = year, group = 1)) +
  geom_line(aes(y = mean_wfromcar, colour = "Overall average")) +
  geom_errorbar(aes(ymin = `CI_lo_wfromcar`, ymax = `CI_hi_wfromcar`), colour = "black", width = .1) +
  geom_line (aes(y=female_mean_wfromcar, colour = "Women")) +
  geom_errorbar(aes(ymin = `female_CI_lo_wfromcar`, ymax = `female_CI_hi_wfromcar`), colour = "black", width = .1) + 
  geom_line(aes(y = male_mean_wfromcar, colour = "Men")) +
  geom_errorbar(aes(ymin = `male_CI_lo_wfromcar`, ymax = `male_CI_hi_wfromcar`), colour = "black", width = .1) +
  scale_color_manual(name = "Groups", values = c("Women" = "indianred", "Men" = "cornflowerblue", "Overall average" = "black")) +
  ylim(1.2,2.8) +
  theme_apa() + 
  ggtitle("Worry about something getting stolen from the car among men and women from \n1998 to 2019/20") +
  labs(x = "Year",
       y = "Worry about something getting stolen from the car")


#Worry of rape
ggplot(final, aes(x = year, group = 1)) +
  geom_line(aes(y = mean_wraped, colour = "Overall average")) +
  geom_errorbar(aes(ymin = `CI_lo_wraped`, ymax = `CI_hi_wraped`), colour = "black", width = .1) +
  geom_line (aes(y=female_mean_wraped, colour = "Women")) +
  geom_errorbar(aes(ymin = `female_CI_lo_wraped`, ymax = `female_CI_hi_wraped`), colour = "black", width = .1) + 
  geom_line(aes(y = male_mean_wraped, colour = "Men")) +
  geom_errorbar(aes(ymin = `male_CI_lo_wraped`, ymax = `male_CI_hi_wraped`), colour = "black", width = .1) +
  scale_color_manual(name = "Groups", values = c("Women" = "indianred", "Men" = "cornflowerblue", "Overall average" = "black")) +
  ylim(1.2,2.8) +
  theme_apa() + 
  ggtitle("Worry about rape among men and women from 1998 to 2019/20") +
  labs(x = "Year",
       y = "Worry about rape")

#Worry of being attacked by a stranger 
ggplot(final, aes(x = year, group = 1)) +
  geom_line(aes(y = mean_wattack, colour = "Overall average")) +
  geom_errorbar(aes(ymin = `CI_lo_wattack`, ymax = `CI_hi_wattack`), colour = "black", width = .1) +
  geom_line (aes(y=female_mean_wattack, colour = "Women")) +
  geom_errorbar(aes(ymin = `female_CI_lo_wattack`, ymax = `female_CI_hi_wattack`), colour = "black", width = .1) + 
  geom_line(aes(y = male_mean_wattack, colour = "Men")) +
  geom_errorbar(aes(ymin = `male_CI_lo_wattack`, ymax = `male_CI_hi_wattack`), colour = "black", width = .1) +
  scale_color_manual(name = "Groups", values = c("Women" = "indianred", "Men" = "cornflowerblue", "Overall average" = "black")) +
  ylim(1.2,2.8) +
  theme_apa() + 
  ggtitle("Worry about being attacked by a stranger among men and women from 1998 to \n2019/20") +
  labs(x = "Year",
       y = "Worry about being attacked by a stranger")

#Worry of being attacked due to skin colour, religion, etc.

ggplot(final, aes(x = year, group = 1)) +
  geom_line(aes(y = mean_wraceatt, colour = "Overall average")) +
  geom_errorbar(aes(ymin = `CI_lo_wraceatt`, ymax = `CI_hi_wraceatt`), colour = "black", width = .1) +
  geom_line (aes(y=female_mean_wraceatt, colour = "Women")) +
  geom_errorbar(aes(ymin = `female_CI_lo_wraceatt`, ymax = `female_CI_hi_wraceatt`), colour = "black", width = .1) + 
  geom_line(aes(y = male_mean_wraceatt, colour = "Men")) +
  geom_errorbar(aes(ymin = `male_CI_lo_wraceatt`, ymax = `male_CI_hi_wraceatt`), colour = "black", width = .1) +
  scale_color_manual(name = "Groups", values = c("Women" = "indianred", "Men" = "cornflowerblue", "Overall average" = "black")) +
  ylim(1.2,2.8) +
  theme_apa() + 
  ggtitle("Worry about being victim of racially motivated attack among men and women \nfrom 1998 to 2019/20") +
  labs(x = "Year",
       y = "Worry about racially motivated attack")

#Worry of being a victim of online crime
ggplot(final, aes(x = year, group = 1)) +
  geom_line(aes(y = mean_wcyber, colour = "Overall average")) +
  geom_errorbar(aes(ymin = `CI_lo_wcyber`, ymax = `CI_up_wcyber`), colour = "black", width = .1) +
  geom_line (aes(y=female_mean_wcyber, colour = "Women")) +
  geom_errorbar(aes(ymin = `female_CI_lo_wcyber`, ymax = `female_CI_up_wcyber`), colour = "black", width = .1) + 
  geom_line(aes(y = male_mean_wcyber, colour = "Blue")) +
  geom_errorbar(aes(ymin = `male_CI_lo_wcyber`, ymax = `male_CI_up_wcyber`), colour = "black", width = .1) +
  scale_color_manual(name = "Groups", values = c("Women" = "indianred", "Men" = "cornflowerblue", "Overall average" = "black")) +
  ylim(1.2,2.8) +
  theme_apa() + 
  ggtitle("Worry about being a victim of online crime among men and women \nfrom 2014 to 2018") +
  labs(x = "Year",
       y = "Worry about online crime")

#Worry of having personal data used without permission
ggplot(final, aes(x = year, group = 1)) +
  geom_line(aes(y = mean_wident, colour = "Overall average")) +
  geom_errorbar(aes(ymin = `CI_lo_wident`, ymax = `CI_up_wident`), colour = "black", width = .1) +
  geom_line (aes(y=female_mean_wident, colour = "Women")) +
  geom_errorbar(aes(ymin = `female_CI_lo_wident`, ymax = `female_CI_up_wident`), colour = "black", width = .1) + 
  geom_line(aes(y = male_mean_wident, colour = "Men")) +
  geom_errorbar(aes(ymin = `male_CI_lo_wident`, ymax = `male_CI_up_wident`), colour = "black", width = .1) +
  scale_color_manual(name = "Groups", values = c("Women" = "indianred", "Men" = "cornflowerblue", "Overall average" = "black")) +
  ylim(1.2,3.0) +
  theme_apa() + 
  ggtitle("Worry about having personal data stolen and/or used without permission among men \nand women from 2015 to 2020") +
  labs(x = "Year",
       y = "Worry about having personal data stolen")

#Worry of being a victim of fraud
ggplot(final, aes(x = year, group = 1)) +
  geom_line(aes(y = mean_wfraud, colour = "Overall average")) +
  geom_errorbar(aes(ymin = `CI_lo_wfraud`, ymax = `CI_up_wfraud`), colour = "black", width = .1) +
  geom_line (aes(y=female_mean_wfraud, colour = "Women")) +
  geom_errorbar(aes(ymin = `female_CI_lo_wfraud`, ymax = `female_CI_up_wfraud`), colour = "black", width = .1) + 
  geom_line(aes(y = male_mean_wfraud, colour = "Men")) +
  geom_errorbar(aes(ymin = `male_CI_lo_wfraud`, ymax = `male_CI_up_wfraud`), colour = "black", width = .1) +
  scale_color_manual(name = "Groups", values = c("Women" = "indianred", "Men" = "cornflowerblue", "Overall average" = "black")) +
  ylim(1.2,2.8) +
  theme_apa() + 
  ggtitle("Worry about being a victim of fraud among men and women from 2015 to 2020") +
  labs(x = "Year",
       y = "Worry about fraud")

#Overall score of worry

ggplot(final, aes(x = year, group = 1)) +
  geom_line(aes(y = overall_worry_score, color = "Overall average")) +
  geom_line (aes(y=female_overall_worry_score, color = "Women")) +
  geom_line(aes(y = male_overall_worry_score, color = "Men")) +
  scale_color_manual(name = "Groups", values = c("Women" = "indianred", "Men" = "cornflowerblue", "Overall average" = "black")) +
  ylim(1.2,2.8) +
  theme_apa() + 
  ggtitle("Overall worry about crime among men and women from 1998 to 2019/20") +
  labs(x = "Year",
       y = "Overall worry about crime")


#### Analysis ####

library(readxl)
final <- read_excel("final_dataset20230625.xlsx")
library(Kendall)
library(xts)
library(collapse)
library(modifiedmk)


#### Percentage change
burglary <- select(final, year, mean_wburgl)
mugging <- select(final, year, mean_wmugged)
cartheft <- select(final, year, mean_wcarstol)
theftfromcar <- select(final, year, mean_wfromcar)
rape <- select(final, year, mean_wraped)
assault <- select(final, year, mean_wattack)
hatecrime <- select(final, year, mean_wraceatt)
identity <- select(final, year, mean_wident)
overall <- select(final, year, overall_worry_score)

pct_change_burgl <- burglary |> fmutate(growth = fgrowth(mean_wburgl))
pct_change_mugg <- mugging |> fmutate(growth = fgrowth(mean_wmugged))
pct_change_cartheft <- cartheft |> fmutate(growth = fgrowth(mean_wcarstol))
pct_change_theftfromcar <- theftfromcar |> fmutate(growth = fgrowth(mean_wfromcar))
pct_change_rape <- rape |> fmutate(growth = fgrowth(mean_wraped))
pct_change_assault <- assault |> fmutate(growth = fgrowth(mean_wattack))
pct_change_hatecrime <- hatecrime |> fmutate(growth = fgrowth(mean_wraceatt))
pct_change_identity <- identity |> fmutate(growth = fgrowth(mean_wident))
pct_change_overall <- overall |> fmutate(growth = fgrowth(overall_worry_score))


# Percentage change from 1998 to 2020
((2.729628 - 2.286031) / 2.286031) * 100 #Burglary
((2.507253 - 2.029903) / 2.029903) * 100 #Mugging
((2.680157 - 2.038665) / 2.038665) * 100 #Car theft
((2.581617 - 2.038223) / 2.038223) * 100 #Theft from car
((2.171452 - 1.626909) / 1.626909) * 100 #Rape
((2.453380 - 2.006910) / 2.006910) * 100 #Assault
((1.676227 - 1.513141) / 1.513141) * 100 #Hate crime

# Percentage change from 1998 to 2005
((2.399938 - 2.729628) / 2.729628) * 100 #Percent change 1998 to 2005 in burglary
((2.507253 - 2.209005) / 2.209005) * 100 #Percent change 1998 to 2005 in mugging
((2.680157 - 2.333363) / 2.209005) * 100 #Percent change 1998 to 2005 in car theft
((2.581617 - 2.273968) / 2.273968) * 100 #Percent change 1998 to 2005 in theft from car
((2.171452 - 1.871337) / 1.871337) * 100 #Percent change 1998 to 2005 in rape
((2.453380 - 2.158696) / 2.158696) * 100 #Percent change 1998 to 2005 in assault
((1.676227 - 1.553425) / 1.553425) * 100 #Percent change 1998 to 2005 in hate crime


#### Mann-Kendall trend test

time <- data.frame(date = c("1998-01-01", "2000-01-01", "2002-01-01", "2003-01-01", "2004-01-01", "2005-01-01", "2006-01-01", "2007-01-01", "2008-01-01", "2009-01-01", "2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01"))
time$date<-as.Date(time$date)  

final$burglary <- xts(final$mean_wburgl, time$date)
final$mugging <- xts(final$mean_wmugged, time$date)
final$cartheft <- xts(final$mean_wcarstol, time$date)
final$theftfromcar <- xts(final$mean_wfromcar, time$date)
final$rape <- xts(final$mean_wraped, time$date)
final$assault <- xts(final$mean_wattack, time$date)
final$hatecrime <- xts(final$mean_wraceatt, time$date)
final$identity <- xts(final$mean_wident, time$date)
final$overall <- xts(final$overall_worry_score, time$date)

MannKendall(final$burglary)
MannKendall(final$mugging)
MannKendall(final$cartheft)
MannKendall(final$theftfromcar)
MannKendall(final$rape)
MannKendall(final$assault)
MannKendall(final$hatecrime)
MannKendall(final$identity)
MannKendall(final$overall)


#### Modified Mann-Kendall trend test because of autocorrelation due to time
bbsmk(final$mean_wburgl, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$mean_wmugged, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$mean_wcarstol, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$mean_wfromcar, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$mean_wraped, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$mean_wattack, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$mean_wraceatt, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$overall_worry_score, ci=0.95, nsim=2000, eta=1, bl.len=NULL)


#### Gender gap 
final$gap_burgl <- (final$female_mean_wburgl - final$male_mean_wburgl)
final$gap_mugg <- (final$female_mean_wmugged - final$male_mean_wmugged)
final$gap_carstol <- (final$female_mean_wcarstol - final$male_mean_wcarstol)
final$gap_fromcar <- (final$female_mean_wfromcar - final$male_mean_wfromcar)
final$gap_rape <- (final$female_mean_wraped - final$male_mean_wraped)
final$gap_assault <- (final$female_mean_wattack - final$male_mean_wattack)
final$gap_hate <- (final$female_mean_wraceatt - final$male_mean_wraceatt)
final$gap_cyber <- (final$female_mean_wcyber - final$male_mean_wcyber)
final$gap_ident <- (final$female_mean_wident - final$male_mean_wident)
final$gap_fraud <- (final$female_mean_wfraud - final$male_mean_wfraud)
final$gap_overall <- (final$female_overall_worry_score - final$male_overall_worry_score)

#Burglary
ggplot(data=final, aes(x=year, y=gap_burgl, group=1)) +
  geom_line()+
  geom_point()
#Mugging
ggplot(data=final, aes(x=year, y=gap_mugg, group=1)) +
  geom_line()+
  geom_point()
#Car theft
ggplot(data=final, aes(x=year, y=gap_carstol, group=1)) +
  geom_line()+
  geom_point()
#Theft from car
ggplot(data=final, aes(x=year, y=gap_fromcar, group=1)) +
  geom_line()+
  geom_point()
#Rape
ggplot(data=final, aes(x=year, y=gap_rape, group=1)) +
  geom_line()+
  geom_point()
#Assault
ggplot(data=final, aes(x=year, y=gap_assault, group=1)) +
  geom_line()+
  geom_point()
#Hate crime
ggplot(data=final, aes(x=year, y=gap_hate, group=1)) +
  geom_line()+
  geom_point()
#Cyber crime
ggplot(data=final, aes(x=year, y=gap_cyber, group=1)) +
  geom_line()+
  geom_point()
#Identity fraud
ggplot(data=final, aes(x=year, y=gap_ident, group=1)) +
  geom_line()+
  geom_point()
#Fraud
ggplot(data=final, aes(x=year, y=gap_fraud, group=1)) +
  geom_line()+
  geom_point()
#Overall
ggplot(data=final, aes(x=year, y=gap_overall, group=1)) +
  geom_line()+
  geom_point()

## Adding confidence intervals to those gender gap values
#Burglary
final$SE_burgl_male <- (final$male_CI_up_wburgl - final$male_CI_lo_wburgl) / (2 * 1.96)
final$SE_burgl_female <- (final$female_CI_up_wburgl - final$female_CI_lo_wburgl) / (2 * 1.96)
final$SED_burgl <- sqrt(final$SE_burgl_female ^2 + final$SE_burgl_male^2) # Calculate Standard Error of the Difference
final$gap_burgl_CI_lo  <- final$gap_burgl - (1.96 * final$SED_burgl) # Calculate 95% Confidence Interval for the Difference
final$gap_burgl_CI_up  <- final$gap_burgl + (1.96 * final$SED_burgl)

#Mugging
final$SE_mugg_male <- (final$male_CI_up_wmugged - final$male_CI_lo_wmugged) / (2 * 1.96)
final$SE_mugg_female <- (final$female_CI_up_wmugged - final$female_CI_lo_wmugged) / (2 * 1.96)
final$SED_mugg <- sqrt(final$SE_mugg_female ^2 + final$SE_mugg_male^2) # Calculate Standard Error of the Difference
final$gap_mugg_CI_lo  <- final$gap_mugg - (1.96 * final$SED_mugg) # Calculate 95% Confidence Interval for the Difference
final$gap_mugg_CI_up  <- final$gap_mugg + (1.96 * final$SED_mugg)

#Car theft
final$SE_carstol_male <- (final$male_CI_up_wcarstol - final$male_CI_lo_wcarstol) / (2 * 1.96)
final$SE_carstol_female <- (final$female_CI_up_wcarstol - final$female_CI_lo_wcarstol) / (2 * 1.96)
final$SED_carstol <- sqrt(final$SE_carstol_female ^2 + final$SE_carstol_male^2) # Calculate Standard Error of the Difference
final$gap_carstol_CI_lo  <- final$gap_carstol - (1.96 * final$SED_carstol) # Calculate 95% Confidence Interval for the Difference
final$gap_carstol_CI_up  <- final$gap_carstol + (1.96 * final$SED_carstol)

#Theft from car
final$SE_fromcar_male <- (final$male_CI_hi_wfromcar - final$male_CI_lo_wfromcar) / (2 * 1.96)
final$SE_fromcar_female <- (final$female_CI_hi_wfromcar - final$female_CI_lo_wfromcar) / (2 * 1.96)
final$SED_fromcar <- sqrt(final$SE_fromcar_female ^2 + final$SE_fromcar_male^2) # Calculate Standard Error of the Difference
final$gap_fromcar_CI_lo  <- final$gap_fromcar - (1.96 * final$SED_fromcar) # Calculate 95% Confidence Interval for the Difference
final$gap_fromcar_CI_up  <- final$gap_fromcar + (1.96 * final$SED_fromcar)

#Rape
final$SE_rape_male <- (final$male_CI_hi_wraped - final$male_CI_lo_wraped) / (2 * 1.96)
final$SE_rape_female <- (final$female_CI_hi_wraped - final$female_CI_lo_wraped) / (2 * 1.96)
final$SED_rape <- sqrt(final$SE_rape_female ^2 + final$SE_rape_male^2) # Calculate Standard Error of the Difference
final$gap_rape_CI_lo  <- final$gap_rape - (1.96 * final$SED_rape) # Calculate 95% Confidence Interval for the Difference
final$gap_rape_CI_up  <- final$gap_rape + (1.96 * final$SED_rape)

#Assault
final$SE_assault_male <- (final$male_CI_hi_wattack - final$male_CI_lo_wattack) / (2 * 1.96)
final$SE_assault_female <- (final$female_CI_hi_wattack - final$female_CI_lo_wattack) / (2 * 1.96)
final$SED_assault <- sqrt(final$SE_assault_female ^2 + final$SE_assault_male^2) # Calculate Standard Error of the Difference
final$gap_assault_CI_lo  <- final$gap_assault - (1.96 * final$SED_assault) # Calculate 95% Confidence Interval for the Difference
final$gap_assault_CI_up  <- final$gap_assault + (1.96 * final$SED_assault)

#Hate crime
final$SE_hate_male <- (final$male_CI_hi_wraceatt - final$male_CI_lo_wraceatt) / (2 * 1.96)
final$SE_hate_female <- (final$female_CI_hi_wraceatt - final$female_CI_lo_wraceatt) / (2 * 1.96)
final$SED_hate <- sqrt(final$SE_hate_female ^2 + final$SE_hate_male^2) # Calculate Standard Error of the Difference
final$gap_hate_CI_lo  <- final$gap_hate - (1.96 * final$SED_hate) # Calculate 95% Confidence Interval for the Difference
final$gap_hate_CI_up  <- final$gap_hate + (1.96 * final$SED_hate)

#Cyber crime
final$SE_cyber_male <- (final$male_CI_up_wcyber - final$male_CI_lo_wcyber) / (2 * 1.96)
final$SE_cyber_female <- (final$female_CI_up_wcyber - final$female_CI_lo_wcyber) / (2 * 1.96)
final$SED_cyber <- sqrt(final$SE_cyber_female ^2 + final$SE_cyber_male^2) # Calculate Standard Error of the Difference
final$gap_cyber_CI_lo  <- final$gap_cyber - (1.96 * final$SED_cyber) # Calculate 95% Confidence Interval for the Difference
final$gap_cyber_CI_up  <- final$gap_cyber + (1.96 * final$SED_cyber)

#Identity fraud
final$SE_ident_male <- (final$male_CI_up_wident - final$male_CI_lo_wident) / (2 * 1.96)
final$SE_ident_female <- (final$female_CI_up_wident - final$female_CI_lo_wident) / (2 * 1.96)
final$SED_ident <- sqrt(final$SE_ident_female ^2 + final$SE_ident_male^2) # Calculate Standard Error of the Difference
final$gap_ident_CI_lo  <- final$gap_ident - (1.96 * final$SED_ident) # Calculate 95% Confidence Interval for the Difference
final$gap_ident_CI_up  <- final$gap_ident + (1.96 * final$SED_ident)

#Fraud
final$SE_fraud_male <- (final$male_CI_up_wfraud - final$male_CI_lo_wfraud) / (2 * 1.96)
final$SE_fraud_female <- (final$female_CI_up_wfraud - final$female_CI_lo_wfraud) / (2 * 1.96)
final$SED_fraud <- sqrt(final$SE_fraud_female ^2 + final$SE_fraud_male^2) # Calculate Standard Error of the Difference
final$gap_fraud_CI_lo  <- final$gap_fraud - (1.96 * final$SED_fraud) # Calculate 95% Confidence Interval for the Difference
final$gap_fraud_CI_up  <- final$gap_fraud + (1.96 * final$SED_fraud)


ggplot(final, aes(x = year, group = 1)) +
  geom_line(aes(y = gap_burgl, colour = "Gender gap in worry about burglary")) +
  geom_errorbar(aes(ymin = `gap_burgl_CI_lo`, ymax = `gap_burgl_CI_up`), colour = "black", width = .1) +
  geom_line (aes(y=gap_mugg, colour = "Gender gap in worry about being mugged")) +
  geom_errorbar(aes(ymin = `gap_mugg_CI_lo`, ymax = `gap_mugg_CI_up`), colour = "black", width = .1) +
  geom_line(aes(y = gap_carstol, colour = "Gender gap in worry about car theft")) +
  geom_errorbar(aes(ymin = `gap_carstol_CI_lo`, ymax = `gap_carstol_CI_up`), colour = "black", width = .1) +
  geom_line (aes(y=gap_fromcar, colour = "Gender gap in worry about theft from car")) +
  geom_errorbar(aes(ymin = `gap_fromcar_CI_lo`, ymax = `gap_fromcar_CI_up`), colour = "black", width = .1) +
  geom_line(aes(y = gap_rape, colour = "Gender gap in worry about rape")) +
  geom_errorbar(aes(ymin = `gap_rape_CI_lo`, ymax = `gap_rape_CI_up`), colour = "black", width = .1) +
  geom_line (aes(y=gap_assault, colour = "Gender gap in worry about assault")) +
  geom_errorbar(aes(ymin = `gap_assault_CI_lo`, ymax = `gap_assault_CI_up`), colour = "black", width = .1) +
  geom_line (aes(y=gap_hate, colour = "Gender gap in worry about being a victim of a hate crime")) +
  geom_errorbar(aes(ymin = `gap_assault_CI_lo`, ymax = `gap_assault_CI_up`), colour = "black", width = .1) +
  geom_line (aes(y=gap_cyber, colour = "Gender gap in worry about cybercrime")) +
  geom_errorbar(aes(ymin = `gap_cyber_CI_lo`, ymax = `gap_cyber_CI_up`), colour = "black", width = .1) +
  geom_line (aes(y=gap_ident, colour = "Gender gap in worry about identity fraud")) +
  geom_errorbar(aes(ymin = `gap_ident_CI_lo`, ymax = `gap_ident_CI_up`), colour = "black", width = .1) +
  geom_line (aes(y=gap_fraud, colour = "Gender gap in worry about fraud")) +
  geom_errorbar(aes(ymin = `gap_fraud_CI_lo`, ymax = `gap_fraud_CI_up`), colour = "black", width = .1) +
  geom_line (aes(y=gap_overall, colour = "Overall gender gap in worry about crime")) +
  scale_color_manual(name = "Gender gap in different types of worry", values = c("Gender gap in worry about burglary" = "lightblue", "Gender gap in worry about being mugged" = "darkblue", "Gender gap in worry about car theft" = "lightgreen", "Gender gap in worry about theft from car" = "darkgreen", "Gender gap in worry about rape" = "yellow", "Gender gap in worry about assault" = "orange", "Gender gap in worry about being a victim of a hate crime" = "red", "Gender gap in worry about cybercrime" = "pink","Gender gap in worry about identity fraud" = "purple","Gender gap in worry about fraud" = "gray","Overall gender gap in worry about crime" = "Black")) +
  theme_apa() + 
  geom_hline(yintercept=0,linetype=3) +
  ggtitle("Gender gap in worry about different crimes from 1998 to 2019/20") +
  labs(x = "Year",
       y = "Size of gender gap in worry about crime")

#### Mann-Kendall trend test for gender gap 

MannKendall(final$gap_burgl)
MannKendall(final$gap_mugg)
MannKendall(final$gap_carstol)
MannKendall(final$gap_fromcar)
MannKendall(final$gap_rape)
MannKendall(final$gap_assault)
MannKendall(final$gap_hate)
MannKendall(final$gap_cyber)
MannKendall(final$gap_ident)
MannKendall(final$gap_fraud)
MannKendall(final$gap_overall)

#### Modified MK trend test for gender gap

bbsmk(final$gap_burgl, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$gap_mugg, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$gap_carstol, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$gap_fromcar, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$gap_rape, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$gap_assault, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$gap_hate, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$gap_cyber, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$gap_ident, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$gap_cfraud, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$gap_overall, ci=0.95, nsim=2000, eta=1, bl.len=NULL)


#Percentage difference gender gap
burglary_gap <- select(final, year, gap_burgl)
mugging_gap <- select(final, year, gap_mugg)
cartheft_gap <- select(final, year, gap_carstol)
theftfromcar_gap <- select(final, year, gap_fromcar)
rape_gap <- select(final, year, gap_rape)
assault_gap <- select(final, year, gap_assault)
hatecrime_gap <- select(final, year, gap_hate)
overall_gap <- select(final, year, gap_overall)

pct_change_burgl_gap <- burglary_gap |> fmutate(growth = fgrowth(gap_burgl))
pct_change_mugg_gap <- mugging_gap |> fmutate(growth = fgrowth(gap_mugg))
pct_change_cartheft_gap <- cartheft_gap |> fmutate(growth = fgrowth(gap_carstol))
pct_change_theftfromcar_gap <- theftfromcar_gap |> fmutate(growth = fgrowth(gap_fromcar))
pct_change_rape_gap <- rape_gap |> fmutate(growth = fgrowth(gap_rape))
pct_change_assault_gap <- assault_gap |> fmutate(growth = fgrowth(gap_assault))
pct_change_hatecrime_gap <- hatecrime_gap |> fmutate(growth = fgrowth(gap_hate))
pct_change_overall_gap <- overall_gap |> fmutate(growth = fgrowth(gap_overall))

# Percentage change from 1998 to 2020
((2.71 - 2.29) / 2.29) * 100 #Burglary
((2.477 - 2.037) / 2.037) * 100 #Mugging
((2.673 - 2.062) / 2.062) * 100 #Car theft
((2.585 - 2.058) / 2.058) * 100 #Theft from car
((2.160 - 1.623) / 1.623) * 100 #Rape
((2.427 - 2.010) / 2.010) * 100 #Assault
((1.669 - 1.537) / 1.537) * 100 #Hate crime

#Looking at significant worries in more detail
#Is there a change in men's and women's worries or just among one gender?

#### MK, MMK, and Sen's Slope for Women: 
MannKendall(final$female_mean_wmugged)
MannKendall(final$female_mean_wfromcar)
MannKendall(final$female_mean_wraped)
MannKendall(final$female_mean_wattack)
bbsmk(final$female_mean_wmugged, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$female_mean_wfromcar, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$female_mean_wraped, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$female_mean_wattack, ci=0.95, nsim=2000, eta=1, bl.len=NULL)

#### MK, MMK, and Sen's Slope for Men:
MannKendall(final$male_mean_wmugged)
MannKendall(final$male_mean_wfromcar)
MannKendall(final$male_mean_wraped)
MannKendall(final$male_mean_wattack)
bbsmk(final$male_mean_wmugged, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$male_mean_wfromcar, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$male_mean_wraped, ci=0.95, nsim=2000, eta=1, bl.len=NULL)
bbsmk(final$male_mean_wattack, ci=0.95, nsim=2000, eta=1, bl.len=NULL)


