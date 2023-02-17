###################################################################################
# This script generates figures and tables from the Open Data questionnaire results
# __author__ = 'Sandrine Soeharjono' / 'Dominique Roche'
# __date__ = 'September 2020'
##################################################################################

# prepare environment and load data
library(afex)
library(dplyr)
library(data.table)
library(car)
require(effects)
library(extrafont)
library(formattable)
library(flextable)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(grid)
require(ggpubr)
require(nnet)
library(scales)
require(DHARMa)
require(ResourceSelection)

data <- read.csv("CB_OD_data.csv", header = TRUE)
glimpse(data)
summary(data)

# let's subset male, female and non-binary data
fem <- data %>%
  filter(gender == "F")
mal <- data %>%
  filter(gender == "M")
nb <- data %>%
  filter(gender == "NB")




########################################################
# TABLE 1
########################################################


############################### DESCRIPTIVE STATS - TABLE FOR BINARY DATA #################################

x = 1 # number of digits after the decimal
p = nrow(data) # number of participants

#beneficial (1=yes, 2=no)
yes_beneficial = (table(data$beneficial) / p)[2]
no_beneficial = (table(data$beneficial) / p)[1]
yes_beneficialCI = 1.96 * sqrt(yes_beneficial * (1 - yes_beneficial) / p) #so % yes +/- .95CI
no_beneficialCI = 1.96 * sqrt(no_beneficial * (1 - no_beneficial) / p) #so % no +/- .95CI

#mandatory_time (1=yes, 2=no)
yes_mandatory_time = (table(data$mandatory_time) / p)[2]
no_mandatory_time = (table(data$mandatory_time) / p)[1]
yes_mandatory_timeCI = 1.96 * sqrt(yes_mandatory_time * (1 - yes_mandatory_time) / p) #so % yes +/- .95CI
no_mandatory_timeCI = 1.96 * sqrt(no_mandatory_time * (1 - no_mandatory_time) / p) #so % no +/- .95CI

#effort_decrease (1=yes, 2=no)
yes_effort_decrease = (table(data$effort_decrease) / p)[2]
no_effort_decrease = (table(data$effort_decrease) / p)[1]
yes_effort_decreaseCI = 1.96 * sqrt(yes_effort_decrease * (1 - yes_effort_decrease) / p) #so % yes +/- .95CI
no_effort_decreaseCI = 1.96 * sqrt(no_effort_decrease * (1 - no_effort_decrease) / p) #so % no +/- .95CI

#benefits (1=yes, 2=no)
yes_benefits = (table(data$benefits) / p)[2]
no_benefits = (table(data$benefits) / p)[1]
yes_benefitsCI = 1.96 * sqrt(yes_benefits * (1 - yes_benefits) / p) #so % yes +/- .95CI
no_benefitsCI = 1.96 * sqrt(no_benefits * (1 - no_benefits) / p) #so % no +/- .95CI

#costs (1=yes, 2=no)
yes_costs = (table(data$costs) / p)[2]
no_costs = (table(data$costs) / p)[1]
yes_costsCI = 1.96 * sqrt(yes_costs * (1 - yes_costs) / p) #so % yes +/- .95CI
no_costsCI = 1.96 * sqrt(no_costs * (1 - no_costs) / p) #so % no +/- .95CI

# let's put all those values together in one table
binary_table = data.frame(
  Variable = c("3. Are open data beneficial to society?",
               "4. Do open data require excessive time?",
               "5. Have time requirements decreased?",
               "6. Have you benefitted from sharing data?",
               "7. Have you incurred costs from sharing data?"),
  Yes = c(round(yes_beneficial*100, x),
          round(yes_mandatory_time*100, x),
          round(yes_effort_decrease*100, x),
          round(yes_benefits*100, x),
          round(yes_costs*100, x)),
  Yes_CI = c(paste0("[", round((yes_beneficial - yes_beneficialCI)*100, x), " - ", 
                 round((yes_beneficial + yes_beneficialCI)*100, x), "]"),
             paste0("[", round((yes_mandatory_time - yes_mandatory_timeCI)*100, x), " - ", 
                     round((yes_mandatory_time + yes_mandatory_timeCI)*100, x), "]"),
             paste0("[", round((yes_effort_decrease - yes_effort_decreaseCI)*100, x), " - ", 
                     round((yes_effort_decrease + yes_effort_decreaseCI)*100, x), "]"),
             paste0("[", round((yes_benefits - yes_benefitsCI)*100, x), " - ", 
                    round((yes_benefits + yes_benefitsCI)*100, x), "]"),
             paste0("[", round((yes_costs - yes_costsCI)*100, x), " - ", 
                    round((yes_costs + yes_costsCI)*100, x), "]")),
  No = c(round(no_beneficial*100, x),
          round(no_mandatory_time*100, x),
          round(no_effort_decrease*100, x),
          round(no_benefits*100, x),
          round(no_costs*100, x)),
  No_CI = c(paste0("[", format(round((no_beneficial - no_beneficialCI)*100, x), nsmall = 2), " - ", 
                    round((no_beneficial + no_beneficialCI)*100, x), "]"),
             paste0("[", round((no_mandatory_time - no_mandatory_timeCI)*100, x), " - ", 
                    round((no_mandatory_time + no_mandatory_timeCI)*100, x), "]"),
             paste0("[", round((no_effort_decrease - no_effort_decreaseCI)*100, x), " - ", 
                    round((no_effort_decrease + no_effort_decreaseCI)*100, x), "]"),
             paste0("[", round((no_benefits - no_benefitsCI)*100, x), " - ", 
                    round((no_benefits + no_benefitsCI)*100, x), "]"),
             paste0("[", round((no_costs - no_costsCI)*100, x), " - ", 
                    round((no_costs + no_costsCI)*100, x), "]")))

# visualizing the table
binary_table = formattable(binary_table,
            align = c("l", "c", "l", "c", "l"), 
            list('Variable' = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                 area(col = c(2, 4)) ~ color_tile("#ccf7ff", "deepskyblue2"),
                 Yes_CI = formatter("span", style = x ~ style(color = "#aeb2b8")),
                 No_CI = formatter("span", style = x ~ style(color = "#aeb2b8"))
            ),
            col.names = c("Question", "Yes (%)", "", "No (%)", ""))
binary_table

# save table in HTML format
htmlwidgets::saveWidget(as.htmlwidget(binary_table), "binary_table.html", selfcontained = TRUE)


############################### DESCRIPTIVE STATS - TABLE FOR ORDINAL DATA #################################

#tendency (1=always, 2=on occasion, 3=never)
always_tendency = (table(data$tendency)/p)[1]
occasion_tendency = (table(data$tendency)/p)[2]
never_tendency = (table(data$tendency)/p)[3]
always_tendencyCI = 1.96*sqrt(always_tendency*(1-always_tendency)/p) #so % yes +/- .95CI
occasion_tendencyCI = 1.96*sqrt(occasion_tendency*(1-occasion_tendency)/p) #so % yes +/- .95CI
never_tendencyCI = 1.96*sqrt(never_tendency*(1-never_tendency)/p) #so % no +/- .95CI

tendency_table = data.frame(Variable = "1. Do you share open data?",
                            Always = round(always_tendency*100, x),
                            Always_CI = paste0("[", round((always_tendency - always_tendencyCI)*100, x), " - ", 
                                               round((always_tendency + always_tendencyCI)*100, x), "]"),
                            Occasion = round(occasion_tendency*100, x),
                            Occasion_CI = paste0("[", round((occasion_tendency - occasion_tendencyCI)*100, x), " - ", 
                                                 round((occasion_tendency + occasion_tendencyCI)*100, x), "]"),
                            Never = round(never_tendency*100, x),
                            Never_CI = paste0("[", round((never_tendency - never_tendencyCI)*100, x), " - ", 
                                              round((never_tendency + never_tendencyCI)*100, x), "]"))

# visualizing the table
tendency_table = formattable(tendency_table,
            align = c("l", "c", "l", "c", "l", "c", "l"), 
            list('Variable' = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                 area(col = c(2, 4, 6)) ~ color_tile("#ccf7ff", "deepskyblue2"),
                 Always_CI = formatter("span", style = x ~ style(color = "#aeb2b8")),
                 Occasion_CI = formatter("span", style = x ~ style(color = "#aeb2b8")),
                 Never_CI = formatter("span", style = x ~ style(color = "#aeb2b8"))
            ),
            col.names = c("Question", "Always (%)", "", "Occasionally (%)", "", "Never (%)", ""))
tendency_table

# save table in HTML format
htmlwidgets::saveWidget(as.htmlwidget(tendency_table), "tendency_table.html", selfcontained = TRUE)

#supportive (1=yes, 2=no, 3=indifferent)
yes_supportive = (table(data$supportive)/p)[1]
no_supportive = (table(data$supportive)/p)[2]
indifferent_supportive = (table(data$supportive)/p)[3]
yes_supportiveCI = 1.96*sqrt(yes_supportive*(1-yes_supportive)/p) #so % yes +/- .95CI
no_supportiveCI = 1.96*sqrt(no_supportive*(1-no_supportive)/p) #so % yes +/- .95CI
indifferent_supportiveCI = 1.96*sqrt(indifferent_supportive*(1-indifferent_supportive)/p) #so % no +/- .95CI

supportive_table = data.frame(Variable = "2. Do you support mandatory data sharing?",
                            Yes = round(yes_supportive*100, x),
                            Yes_CI = paste0("[", format(round((yes_supportive - yes_supportiveCI)*100, x), nsmall = 1), " - ", 
                                            format(round((yes_supportive + yes_supportiveCI)*100, x), nsmall = 1), "]"),
                            Indifferent = round(indifferent_supportive*100, x),
                            Indifferent_CI = paste0("[", format(round((indifferent_supportive - indifferent_supportiveCI)*100, x), nsmall = 1), " - ", 
                                                    format(round((indifferent_supportive + indifferent_supportiveCI)*100, x), nsmall = 1), "]"),
                            No = round(no_supportive*100, x),
                            No_CI = paste0("[", 
                                           round((no_supportive - no_supportiveCI)*100, x), " - ", 
                                           format(round((no_supportive + no_supportiveCI)*100, x), nsmall = 1), "]")
                            ) 

# visualizing the table
supportive_table = formattable(supportive_table,
            align = c("l", "c", "l", "c", "l", "c", "l"), 
            list('Variable' = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                 area(col = c(2,4,6)) ~ color_tile("#ccf7ff", "deepskyblue2"),
                 Yes_CI = formatter("span", style = x ~ style(color = "#aeb2b8")),
                 Indifferent_CI = formatter("span", style = x ~ style(color = "#aeb2b8")),
                 No_CI = formatter("span", style = x ~ style(color = "#aeb2b8"))),
            col.names = c("", "Yes (%)", "", "Indifferent (%)", "", "No (%)", ""))
supportive_table

# save table in HTML format
htmlwidgets::saveWidget(as.htmlwidget(supportive_table), "supportive_table.html", selfcontained = TRUE)



########################################################
# FIGURE 1
########################################################

############################### FIGURE 1 A: PERCENTAGE OF OPEN DATA PUBLICATIONS ############################### 

# 1 = 0%, 2 = 0-10%, 3 = 10-25%, 4 = 25-50%, 5 = 50-75%, 6 = 75-100% 
# proportions of total participants answers for perc_shared
all_shared = data.frame(table(data$perc_shared_data)/nrow(data))
colnames(all_shared) = c("Category", "Percentage")
levels(all_shared$Category) = c('0%', '>0-10%', '>10-25%', '>25-50%', '>50-75%', '>75-100%')
# proportions of female participants answers for perc_shared
f_shared = data.frame(table(fem$perc_shared_data)/nrow(fem))
colnames(f_shared) = c("Category", "Percentage")
levels(f_shared$Category) = c('0%', '>0-10%', '>10-25%', '>25-50%', '>50-75%', '>75-100%')
# proportions of male participants answers for perc_shared
m_shared = data.frame(table(mal$perc_shared_data)/nrow(mal))
colnames(m_shared) = c("Category", "Percentage")
levels(m_shared$Category) = c('0%', '>0-10%', '>10-25%', '>25-50%', '>50-75%', '>75-100%')

shared_graph <- ggplot(all_shared, aes(y = Percentage, x = Category, 
                                       label = round(Percentage, 4) * 100)) + 
  theme_economist() + 
  scale_fill_economist() +
  geom_bar(position = "stack", stat = "identity", fill = 'deepskyblue2') +
  xlab("Percentage of publications with open data") + ylab("Percentage of respondents") + 
  geom_errorbar(data = m_shared, aes(ymin = Percentage, ymax = Percentage), 
                color = "navy", lty = 2, size = 0.5, show.legend = TRUE) + 
  geom_errorbar(data = f_shared, aes(ymin = Percentage, ymax = Percentage), 
                color = "navy", lty = 3, size = 0.5, show.legend = FALSE) + 
  scale_y_continuous(limits = c(0, 0.32), breaks = seq(0, 0.30, 0.1), 
                     labels = scales::percent_format(accuracy=1)) +
  geom_text(aes(label = paste0(round(100 * Percentage, 1), "%")), size = 2.5,
            position = position_stack(vjust = 0.5), colour = "black") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 20),
        axis.text.x = element_text(size = 8, angle = 35, hjust = 0.5, vjust = 1.1),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 10, vjust = 0),
        axis.text.y = element_text(size = 8, hjust = 1),
        axis.title.y = element_text(size = 10, vjust = 3),
        axis.line = element_blank())
shared_graph



############################## FIGURE 1 B: BENEFITS EXPERIENCED FROM DATA SHARING ############################## 

# proportions of total participants answers for benefits
all_benefits <- as.integer(unlist(strsplit(as.character(data$benefits_list), ",")))
all_benefits = data.frame(table(all_benefits)/nrow(data))
colnames(all_benefits) = c("Benefits", "Percentage")
levels(all_benefits$Benefits) <- c("Satisfaction", "Efficiency", "Collaborations", "Co-authorship", 
                                   "Awards", "Accolades", "Reviews", "Other")
# proportions of female participants answers for benefits
f_benefits <- as.integer(unlist(strsplit(as.character(fem$benefits_list), ",")))
f_benefits = data.frame(table(f_benefits)/nrow(fem))
colnames(f_benefits) = c("Benefits", "Percentage")
levels(f_benefits$Benefits) <- c("Satisfaction", "Efficiency", "Collaborations", "Co-authorship", 
                                 "Accolades", "Reviews", "Other")
extra = data.frame(Benefits = "Awards", Percentage = 0)
f_benefits = bind_rows(f_benefits, extra)
# proportions of male participants answers for benefits
m_benefits <- as.integer(unlist(strsplit(as.character(mal$benefits_list), ",")))
m_benefits = data.frame(table(m_benefits)/nrow(mal))
colnames(m_benefits) = c("Benefits", "Percentage")
levels(m_benefits$Benefits) <- c("Satisfaction", "Efficiency", "Collaborations", "Co-authorship", 
                                 "Awards", "Accolades", "Reviews", "Other")

# plot percentage of total participants (bars) + overlaid men/women's data (lines)
benefits_graph <- ggplot(all_benefits, aes(y = Percentage, x = reorder(Benefits, -Percentage),
                                           label = round(Percentage,4) * 100)) + 
  theme_economist() +
  scale_fill_economist() +
  geom_bar(position = "stack", stat = "identity", fill = "deepskyblue2") +
  geom_errorbar(data = m_benefits, aes(ymin = Percentage, ymax = Percentage), 
                color = "navy", lty = 2, size = 0.5) + 
  geom_errorbar(data = f_benefits, aes(ymin = Percentage, ymax = Percentage), 
                color = "navy", lty = 3, size = 0.5) +
  xlab("Benefits of sharing open data") + ylab("Percentage of respondents") + 
  scale_y_continuous(limits = c(0,0.5), breaks = seq(0,0.5,0.1), 
                     labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = paste0(round(100*Percentage,1),"%")), size = 2.5,
            position = position_stack(vjust = 0.5), colour = "black") +
  theme(plot.title = element_text(hjust = 0, size = 20),
        axis.text.x = element_text(size = 8, angle = 35, hjust = 1, vjust = 1.3),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8, hjust = 1),
	      axis.title.y = element_text(size = 10, vjust = 3),
        axis.line = element_blank())
benefits_graph


################################ FIGURE 1 C: COSTS EXPERIENCED FROM DATA SHARING ################################ 

# proportions of total participants answers for costs
all_costs <- as.integer(unlist(strsplit(as.character(data$costs_list), ",")))
all_costs = data.frame(table(all_costs)/nrow(data))
colnames(all_costs) = c("Costs", "Percentage")
levels(all_costs$Costs) <- c("Frustration", "Scooping", "Misuse", "Privacy", 
                             "Recognition", "Shaming", "Other")
# proportions of female participants answers for costs
f_costs <- as.integer(unlist(strsplit(as.character(fem$costs_list), ",")))
f_costs = data.frame(table(f_costs)/nrow(fem))
colnames(f_costs) = c("Costs", "Percentage")
levels(f_costs$Costs) <- c("Frustration", "Scooping", "Misuse", "Privacy", 
                           "Recognition", "Other")
extra = data.frame(Costs = "Shaming", Percentage = 0)
f_costs = bind_rows(f_costs, extra)
# proportions of male participants answers for costs
m_costs <- as.integer(unlist(strsplit(as.character(mal$costs_list), ",")))
m_costs = data.frame(table(m_costs)/nrow(mal))
colnames(m_costs) = c("Costs", "Percentage")
levels(m_costs$Costs) <- c("Frustration", "Scooping", "Misuse", 
                           "Recognition", "Shaming", "Other")
extra = data.frame(Costs = "Privacy", Percentage = 0)
m_costs = bind_rows(m_costs, extra)

# plot percentage of total participants (bars) + overlaid men/women's data (lines)
linetypes <- c("Men"=2,"Women"=3)
costs_graph <- ggplot(all_costs, aes(y = Percentage, x = Costs)) +
  theme_economist() +
  scale_fill_economist() +
  geom_bar(aes(y = Percentage, x = reorder(Costs, -Percentage)), 
           position = "stack", stat = "identity", fill = "deepskyblue2") +
  geom_errorbar(data = m_costs, aes(ymin = Percentage, ymax = Percentage, lty = "Men"), color = "navy", size = 0.5) + 
  geom_errorbar(data = f_costs, aes(ymin = Percentage, ymax = Percentage, lty = "Women"), colour = "navy", size = 0.5) + 
  xlab("Costs of sharing open data") + ylab("Percentage of respondents") +
  scale_y_continuous(limits = c(0,0.15), breaks = seq(0,0.15,0.05), 
                     labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = paste0(round(100*Percentage,1),"%")), size = 2.5,
            position = position_stack(vjust = 0.5), colour = "black") +
  scale_linetype_manual("Genders", values = linetypes) + 
  theme(legend.position = "none") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 20),
        axis.text.x = element_text(size = 8, angle = 35, hjust = 1, vjust = 1.3),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8, hjust = 1),
	      axis.title.y = element_text(size = 10, vjust = 3),
        axis.line = element_blank())
costs_graph

############################### SETTING UP ALL 3 PANELS INTO ONE FIGURE #################################
# vertically 
ggarrange(shared_graph, benefits_graph, costs_graph, labels = c("A", "B", "C"), ncol = 1, nrow = 3)




########################################################
# FIGURE 2 (including statistical analyses)
########################################################

# define color palette 
 economist <- c("#01a2d9","#014d64")
 
# select variables used for GLMs
 data2 <- data %>%
  dplyr::select(phd_year, gender, supportive, benefits, costs) %>%
  dplyr::filter(gender != "NB")
 glimpse(data2)
 
#remove level NB from data2$gender
 data2$gender <- factor(data2$gender)
 levels(data2$gender)

# what's the average phd_year for men & women?
 data2 %>%
  group_by(gender) %>%
  summarize(avg = mean(phd_year))
# how is phd_year distributed across men & women?
 boxplot(phd_year ~ gender, data = data2, main = "PhD completion year by gender", col = "light blue",
        xlab = "Gender", ylab = "phd_year") 

# box plots comparing the distribution of phd_year for each response variable
par(mfrow = c(3,1))
boxplot(phd_year ~ supportive, data = data2, main = "PhD completion year by supportive", col = "light blue",
        xlab = "Supportive Answers", ylab = "phd_year") 
boxplot(phd_year ~ benefits, data = data2, main = "PhD completion year by benefits", col = "light blue",
        xlab = "Benefits Answers", ylab = "phd_year") 
boxplot(phd_year ~ costs, data = data2, main = "PhD completion year by costs", col = "light blue",
        xlab = "Costs Answers", ylab = "phd_year") 
par(mfrow = c(1,1))

# frequency of each gender with regards to the response variables
xtabs(~supportive + gender, data = data2)
xtabs(~benefits + gender, data = data2)
xtabs(~costs + gender, data = data2)

# create supportiveO as an ordered factor
data2$supportive <- as.factor(data2$supportive)
levels(data2$supportive) <- c("Yes", "No", "Indifferent")
data2$supportiveO <- factor(data2$supportive, ordered = TRUE, levels = c("No", "Indifferent", "Yes"))



######################## 'SUPPORTIVE' MULTINOMIAL LOGISTIC REGRESSION ##########################

# analyze the distribution of supportive across gender and phd_year
ggplot(data2, aes(x = supportive, y = phd_year, fill = supportive)) + 
  geom_boxplot(size = .75) +
  facet_grid(~ gender, margins = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#tables
 with(data2, table(gender, supportive))
 with(data2, do.call(rbind, tapply(phd_year, supportive, function(x) c(M = mean(x), SD = sd(x)))))

# multinomial logistic regression with "nnet" package
 #see https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
 data2$supportive2 <- relevel(data2$supportive, ref = "No") 
 suppM <- multinom(supportive2 ~ phd_year * gender, data = data2)
 suppM.2 <- multinom(supportive2 ~ phd_year + gender, data = data2)
 anova(suppM,suppM.2) #interaction NS
 AIC(suppM); AIC(suppM.2) #supports retaining the simple model as above
 #qqnorm(residuals(suppM.2), col="blue")
 #plot(residuals(suppM.2)[,3]~data2$phd_year)
 summary(suppM.2)
 set_sum_contrasts();
 Anova(suppM.2, Type="III") # no effect of gender or PhD year, see https://stats.stackexchange.com/questions/63222/getting-p-values-for-multinom-in-r-nnet-package
 exp(coef(suppM.2))
 dgender <- data.frame(gender = c("M", "F"), phd_year = mean(data2$phd_year))
 predict(suppM.2, newdata = dgender, "probs")
 head(pp <- fitted(suppM.2)) # calculates predicted probabilities 
 plot(effect("gender", suppM.2), style = "stacked", colors = c("green", "blue", "red"))
 plot(effect("phd_year", suppM.2), style = "stacked", colors = c("green", "blue", "red"))
 plot(effect("phd_year:gender", suppM), style = "stacked", colors = c("green", "blue", "red"))


############################# 'BENEFITS' BINOMIAL GLM ###############################

### Figure ###

#Graph of p(benefits) ~ gender and PhDyear
 pBenefits<-ggplot(data2, aes(x = phd_year, y = benefits, colour = factor(gender))) +
  geom_point() +
  geom_point(data = nb, aes(x = phd_year, y = benefits), colour = "tomato3", inherit.aes = FALSE) +
  stat_smooth(method ="glm", method.args = list(family="binomial"), se = TRUE) +
  labs(x = "PhD year", y = "p(benefits)", colour = "gender") +
  theme_economist() +
  scale_fill_economist() +
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 4),
        axis.text.x = element_text(size = 10, vjust = 1),
        axis.ticks.x = element_blank(), 
        axis.line.x = element_line(colour = "black", size = 0.8),
        axis.line.y = element_line(colour = "black", size = 0.8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "none") +
  scale_x_continuous(limits = c(1970,2020)) +
  scale_colour_manual(values = economist)
 pBenefits

### Statistical model ###

# see articles 1 to 3 here https://www.theanalysisfactor.com/r-tutorial-glm1/ for interpreting model outputs
 benefits_model = glm(benefits ~ phd_year * gender, family = binomial(link = "logit"), data = data2)
 #diagnostics with DHARMa::simulateResiduals
  ben_simulationOutput <- simulateResiduals(fittedModel = benefits_model, n = 250)
  plot(ben_simulationOutput)
 #output
  summary(benefits_model) # coefficients indicate how 1 unit of X produces Y units of change in the log odds (which is pretty meaningless) - need to convert these logits to odds ratios 
 benefits_model.2 = glm(benefits ~ phd_year + gender, family = binomial(link = "logit"), data = data2)
  summary(benefits_model.2) #none of the predictors are significant and the explained deviance is low
  #Anova(benefits_model.2)
 #exponentiate model coefficients to convert logits to odds ratios
  exp(coef(benefits_model.2)) # odds ratios 1.006 for phd_year and 1.71 for genderM, so no change for phd_year and relatively small (ns) change for genderM
 #goodness of fit with ResourceSelection::hoslem.test
  hoslem.test(data2$benefits, fitted(benefits_model.2)) #model fits well, no sign diff between model predictions and observed data (p=0.12)


############################### 'COSTS' BINOMIAL GLM ################################

### Figure ###

#Graph of p(costs) ~ gender and PhDyear
 pCosts<-ggplot(data2, aes(x = phd_year, y = costs, colour = factor(gender))) +
  geom_point() +
  geom_point(data = nb, aes(x = phd_year, y = costs), colour = "tomato3", inherit.aes = FALSE) +
  stat_smooth(method="glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(x = "PhD year", y = "p(costs)", colour = "gender") +
  theme_economist() +
  scale_fill_economist() +
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 4),
        axis.text.x = element_text(size = 10, vjust = 1),
        axis.ticks.x= element_blank(), 
        axis.line.x = element_line(colour="black", size = 0.8),
        axis.line.y = element_line(colour="black", size = 0.8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "none")+
  scale_x_continuous(limits = c(1970,2020)) +
  scale_colour_manual(values = economist)
pCosts

### Statistical model ###

 costs_model = glm(costs ~ phd_year * gender, family = binomial, data = data2)
 #diagnostics
  cos_simulationOutput <- simulateResiduals(fittedModel = costs_model, n = 250)
  plot(cos_simulationOutput)
 #outputs
  summary(costs_model)
  #Anova(costs_model)
 #remove interaction
  costs_model.2 = glm(costs ~ phd_year + gender, family = binomial, data = data2)
  summary(costs_model.2)
  #Anova(costs_model.2)
 #exponentiate model coefficients to convert logits to odds ratios
  exp(coef(costs_model.2)) # odds ratios ~1.04 for phd_year and ~2.92 for genderM
 #goodness of fit with ::ResourceSelection
  hoslem.test(data2$costs, fitted(costs_model.2)) #model fits well, no sign diff between model predictions and observed data (p=0.85)


############################### SETUP 2 PANELS INTO ONE FIGURE #################################

# vertically 
ggarrange(pBenefits, pCosts, labels = c("A", "B"), ncol = 1, nrow = 2)

