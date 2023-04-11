library(tidyverse)
library(readr)
library(data.table)
library(ggplot2)
library(Hmisc)
library(gridExtra)

options(scipen=999)

# Set working directory Agatha
setwd("C:/Users/agath/Desktop/Masterthesis/Data")

# Load data - LCS
carbon_pricing    <- read_csv("TBD/LCS/Carbon_Pricing_Incidence_South Africa.csv")
fuel_expenditures <- read_csv("TBD/LCS/fuel_expenditures_South Africa.csv") 
hh_info           <- read_csv("TBD/LCS/household_information_South Africa.csv")

# Create one dataframe with all information
hh_fuel_carbon <- left_join(carbon_pricing, fuel_expenditures)
hh_final <- left_join(hh_fuel_carbon, hh_info)

# save dataframe 
write.csv(hh_final, "TBD/hh_final_LCS.csv")

rm(list = setdiff(ls(), "hh_final"))


#---------------------------------------------------------------------------------------------------------

# Farben

# rot      organge     türkis    blau
#"#9b2226","#ca6702","#94d2bd","#194A84"


## Share of expenditure categories over expenditure deciles ####
summary_expenditures_categories <- hh_final %>%
  group_by(Income_Group_10) %>%
  summarise(Services = mean(share_services), Goods = mean(share_goods), Food = mean(share_food), Energy = mean(share_energy)  ) %>%
  pivot_longer(cols=2:5, names_to = "category", values_to = "percentage")

summary_expenditures_categories$percentage <- summary_expenditures_categories$percentage * 100

ggplot(summary_expenditures_categories, aes(x = Income_Group_10, y = percentage, fill = category)) +
  geom_bar(position="stack", stat="identity") +
  theme_bw()+
  labs (x = "Expenditure Deciles", y = "Expenditure Share (%)", title = "Share of Expenditures for different Categories", fill = "Expenditure Category") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_x_discrete(limits = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10 \n Richest \n 10 Percent")) +
  scale_fill_manual(values=c("#9b2226","#ca6702","#94d2bd","#194A84"))+
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))

# dataframe
pivoted_df = pivot_wider(summary_expenditures_categories, names_from = category, values_from = percentage)

# check and write
print(pivoted_df)
write.csv(pivoted_df, "pivoted_categories_share.csv", row.names = FALSE)



## Distribution CO2 footprint ####

# Barplot
ggplot(hh_final, aes(x = CO2_t_national)) +
  geom_histogram(aes(y = (..density..)*1000), binwidth = 10, fill = "#ca6702", color ="black", position = position_nudge(x=5))+
  geom_vline(xintercept = mean(hh_final$CO2_t_national), 
             color = "black", linetype = "dashed", size = 0.5) +
  labs(x = "CO2 emissions (t/household)", 
       y = "Share of all households (%)", 
       title = "Distribution of CO2 Footprint") +
  scale_x_continuous(breaks = seq(0, 200, by = 10), minor_breaks = seq(0, 200, by = 10)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        panel.grid.major = element_line(colour = "lightgray", linetype = "dotted"),
        panel.grid.minor = element_blank())


# Boxplot
ggplot(hh_final, aes(x = CO2_t_national)) +
  geom_boxplot(fill = "#ca6702", color = "#4C4C4C", alpha = 0.5, size = 0.4, width=0.1) +
  stat_boxplot(geom = "errorbar", width = 0.02, coef = 1.5, color = "black") +
  geom_point(aes(y=0, x = mean(hh_final$CO2_t_national)), shape = 23, size = 1.5, fill = "black")+
  labs(y = "", 
       x = "CO2 emissions (t/household)", 
       title = "Distribution of CO2 Footprint") +
  coord_trans(y = "reverse") +
  scale_x_continuous(breaks = seq(0, 200, by = 10), minor_breaks = seq(0, 200, by = 10)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        panel.grid.major = element_line(colour = "lightgray", linetype = "dotted"),
        panel.grid.minor = element_blank())


## BAR CHART national carbon footprint ####
CO2_tons <- hh_final %>% 
  group_by(Income_Group_10) %>%
  summarise(t_national = mean(CO2_t_national))

ggplot(CO2_tons, aes(x = Income_Group_10, y = t_national)) +
  geom_bar(stat = "identity", fill="#ca6702", color="black") +
  theme_bw()+
  labs(title = "Carbon Footprint per Household", x = "Expenditure Deciles", y = "Mean Carbon Footprint (tCO2/household)") +
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10 \n Richest \n 10 Percent")) + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))

summary(hh_final$CO2_t_national)

# dataframe
write.csv(CO2_tons, "CO2_tons.csv", row.names = FALSE)



# ## DOT PLOT Average carbon price burden for a global carbon price (red) and a national carbon price (blue) over deciles ####
# # not used #
# income_C02_averages_national <- hh_final %>%
#   group_by(Income_Group_10) %>%
#   summarise(national = mean(burden_CO2_national))
# 
# income_C02_averages_global <- hh_final %>%
#   group_by(Income_Group_10) %>%
#   summarise(global = mean(burden_CO2_global))
# 
# average_carbon_expenses <- left_join(income_C02_averages_global, income_C02_averages_national) %>% 
#   pivot_longer(-Income_Group_10) %>% 
#   rename("region" = "name",
#          "burden" = "value") %>%
#   mutate(income_group = factor(Income_Group_10, levels = as.character(1:10)))
# 
# ggplot(average_carbon_expenses, aes(x = income_group, y = burden, color = region)) +
#   geom_point() +
#   scale_y_continuous(labels = scales::label_percent(), "Carbon Price Burden (% of total budget)") +
#   labs(title = "Income Deciles versus Global Carbon Pricing Burden",
#        x = "Expenditure Decile")


## BOX PLOT Distribution of a national carbon price burden within each income decile ####
hh_final$Income_Group_10 <- as.factor(hh_final$Income_Group_10)

hh_final_1 <- hh_final %>%
  group_by(Income_Group_10)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

ggplot(hh_final_1, aes(x = factor(Income_Group_10), fill=""))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5) +
  theme_bw()+
  xlab("Expenditure Deciles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "black")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10 \n Richest \n 10 Percent"))+
  coord_cartesian(ylim = c(0,0.2))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2")+
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  scale_fill_manual(values=c("#ca6702"))+
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))

# dataframe
write.csv(hh_final_1, "Additional_costs_all.csv", row.names = FALSE)


## Carbon price burden in different social groups - Urban/Rural ####
hh_final$urban_1 <- as.factor(hh_final$urban_1)

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5, urban_1)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

ggplot(hh_final_2, aes(x = factor(Income_Group_5), fill = urban_1))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group=urban_1), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.2))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Residential Areas")+
  scale_fill_manual(values=c("#F8766D", "#00BFC4", "#E69F00", "#009E73"), labels = c("Urban formal", "Urban informal", "Rural traditional", "Rural formal")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
   theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")

# dataframe
write.csv(hh_final_2, "Additional_costs_urb.csv", row.names = FALSE)

## Carbon price burden in different social groups - Province #####
hh_final$province <- as.factor(hh_final$province)

hh_final_2 <- hh_final %>%
  group_by(Income_Group_3, province)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

ggplot(hh_final_2, aes(x = factor(Income_Group_3), fill = province))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Tertiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group=province), shape = 23, size = 1, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 33 Percent", "2", "3 \n Richest \n 33 Percent"))+
  coord_cartesian(ylim = c(0,0.2))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Provinces")+
  scale_fill_manual(values=c("#F8766D", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#006147"), labels = c("Western Cape", "Eastern Cape","Northern Cape","Free State","KwaZulu-Natal","North West","Gauteng","Mpumalanga","Limpopo")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")

# dataframe
write.csv(hh_final_2, "Additional_costs_pro.csv", row.names = FALSE)

# Table
#nrow(hh_final[hh_final$province == '5' & hh_final$Income_Group_5 == '1', ]) 


# # province on x
# hh_final$province <- as.factor(hh_final$province)
# 
# hh_final_pr <- hh_final %>%
#   group_by(province)%>%
#   summarise(
#     y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
#     y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
#     y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
#     y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
#     y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
#     mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
#   ungroup()
# 
# ggplot(hh_final_pr, aes(x = factor(province), color = province))+
#   geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
#   theme_bw()+
#   xlab("Provinces")+
#   ylab("Additional Costs (% of total expenditures)")+
#   geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "white", position=position_dodge(width=0.5))+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
#   coord_cartesian(ylim = c(0,0.2))+
#   ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Provinces")+
#   scale_color_manual(values=c("#F8766D", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#006147"), labels = c("Western Cape", "Eastern Cape","Northern Cape","Free State","KwaZulu-Natal","North West","Gauteng","Mpumalanga","Limpopo")) + 
#   theme(axis.text = element_text(size = 8), 
#         axis.title = element_text(size = 8),
#         plot.title = element_text(size = 11),
#         plot.subtitle = element_text(size = 10),
#         legend.position = "bottom",
#         plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
#   labs(colour = "")


## Carbon price burden in different social groups - Ethnicity ####
hh_final$ethnicity_hhh <- as.factor(hh_final$ethnicity_hhh)

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5, ethnicity_hhh)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

ggplot(hh_final_2, aes(x = factor(Income_Group_5), fill = ethnicity_hhh))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group=ethnicity_hhh), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.31))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Ethnicity")+
  scale_fill_manual(values=c("#F8766D", "#00BFC4", "#E69F00", "#009E73"), labels = c("African Black", "Coloured", "Indian/Asian", "White")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")

  # dataframe
write.csv(hh_final_2, "Additional_costs_eth.csv", row.names = FALSE)

# # not over income groups - 
# hh_final$ethnicity_hhh <- as.factor(hh_final$ethnicity_hhh)
# 
# hh_final_et <- hh_final %>%
#   group_by(ethnicity_hhh)%>%
#   summarise(
#     y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
#     y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
#     y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
#     y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
#     y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
#     mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
#   ungroup()
# 
# ggplot(hh_final_et, aes(x = factor(ethnicity_hhh), color = ethnicity_hhh))+
#   geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
#   theme_bw()+
#   xlab("")+
#   ylab("Additional Costs (% of Total Expenditures)")+
#   geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "white", position=position_dodge(width=0.5))+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
#   coord_cartesian(ylim = c(0,0.21))+
#   ggtitle("Distributional Effects of a national Carbon Tax of US-$ 30/tCO2 on Population in South Africa")+
#   scale_color_manual(values=c("#F8766D", "#00BFC4", "#E69F00", "#009E73"), labels = c("African Black", "Coloured", "Indian/Asian", "White")) + 
#   theme(axis.text = element_text(size = 8), 
#         axis.title = element_text(size = 8),
#         plot.title = element_text(size = 11),
#         plot.subtitle = element_text(size = 10),
#         legend.position = "bottom",
#         plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
#   labs(colour = "")


################## Carbon price burden in different social groups - Gender ##################
hh_final$gender_hhh <- as.factor(hh_final$gender_hhh)

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5, gender_hhh)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

ggplot(hh_final_2, aes(x = factor(Income_Group_5), fill = gender_hhh))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group=gender_hhh), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.21))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Gender")+
  scale_fill_manual(values=c("#F8766D", "#009E73"), labels = c("Male", "Female")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")

  # dataframe
  write.csv(hh_final_2, "Additional_costs_gender.csv", row.names = FALSE)
  
  female <- sum(hh_final$gender_hhh == 2)
  print(female)
  
  male <- sum(hh_final$gender_hhh == 1)
  print(male)

# Table
#nrow(hh_final[hh_final$gender_hhh == '3' & hh_final$Income_Group_5 == '1', ]) 

# # 2 income groups
# hh_final_3 <- hh_final %>%
#   group_by(Income_Group_10, gender_hhh)%>%
#   summarise(
#     y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
#     y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
#     y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
#     y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
#     y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
#     mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
#   ungroup()
# 
# ggplot(hh_final_3, aes(x = factor(Income_Group_10), color = gender_hhh))+
#   geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
#   theme_bw()+
#   xlab("Expenditure Quintiles")+
#   ylab("Additional Costs (% of Total Expenditures)")+
#   geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "white", position=position_dodge(width=0.5))+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
#   scale_x_discrete(labels = c("1 \n Poorest \n 50 Percent", "2 \n Richest \n 50 Percent"))+
#   coord_cartesian(ylim = c(0,0.31))+
#   ggtitle("Distributional Effects of a national Carbon Tax of US-$ 30/tCO2 on Population in South Africa")+
#   scale_color_manual(values=c("#F8766D", "#00BFC4", "#E69F00", "#009E73"), labels = c("male", "female")) + 
#   theme(axis.text = element_text(size = 8), 
#         axis.title = element_text(size = 8),
#         plot.title = element_text(size = 11),
#         plot.subtitle = element_text(size = 10),
#         legend.position = "bottom",
#         plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
#   labs(colour = "")
# 
# # gender on x 
# 
# hh_final_et <- hh_final %>%
#   group_by(gender_hhh)%>%
#   summarise(
#     y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
#     y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
#     y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
#     y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
#     y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
#     mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
#   ungroup()
# 
# 
# ggplot(hh_final_et, aes(x = factor(gender_hhh), color = gender_hhh))+
#   geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
#   theme_bw()+
#   xlab("Expenditure Quintiles")+
#   ylab("Additional Costs (% of Total Expenditures)")+
#   geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "white", position=position_dodge(width=0.5))+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
#   coord_cartesian(ylim = c(0,0.3))+
#   ggtitle("Distributional Effects of a national Carbon Tax of US-$ 30/tCO2 on Population in South Africa")+
#   scale_color_manual(values=c("#F8766D", "#00BFC4", "#E69F00", "#009E73"), labels = c("male", "female")) + 
#   theme(axis.text = element_text(size = 8), 
#         axis.title = element_text(size = 8),
#         plot.title = element_text(size = 11),
#         plot.subtitle = element_text(size = 10),
#         legend.position = "bottom",
#         plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
#   labs(colour = "")



## Access to Electricity ####

hh_final$electricitycon <- as.factor(hh_final$electricitycon)

hh_final_4 <- hh_final %>%
  group_by(Income_Group_5, electricitycon)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()%>%
  na.omit()

hh_final_4 <- hh_final_4 %>%
  filter(!electricitycon == 9)

ggplot(hh_final_4, aes(x = factor(Income_Group_5), fill = electricitycon))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group=electricitycon), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.2))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Electricity Access")+
  scale_fill_manual(values=c("#ca6702","#194A84"), labels = c("Access", "No Access")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")
  
  # dataframe
  write.csv(hh_final_4, "Additional_costs_elec_ac.csv", row.names = FALSE)

  
  
## Connected to MAINS ####

hh_final_4 <- hh_final %>%
  group_by(Income_Group_5, electricitymains)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()%>%
  na.omit()

hh_final_4 <- hh_final_4 %>%
  filter(!electricitymains > 2)

hh_final_4$electricitymains <- as.factor(hh_final_4$electricitymains)

ggplot(hh_final_4, aes(x = factor(Income_Group_5), fill = electricitymains))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group = electricitymains), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.2))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Electricity Source")+
  scale_fill_manual(values=c("#ca6702","#194A84"), labels = c("MAINS", "Other")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")
  
  # dataframe
  write.csv(hh_final_4, "Additional_costs_elec_mains.csv", row.names = FALSE)






## Burden CO2 - electricity / transport ####

hh_final_1 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

hh_final_1$category <- "burden_CO2_national"

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_electricity, weights = hh_weights))%>%
  ungroup()

hh_final_2$category <- "burden_CO2_electricity"

hh_final_3 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_transport, weights = hh_weights))%>%
  ungroup()

hh_final_3$category <- "burden_CO2_transport"


hh_burden <- merge(hh_final_1, hh_final_2, all=TRUE)
hh_burden2 <- merge(hh_burden, hh_final_3, all=TRUE)

hh_burden2$category <- factor(hh_burden2$category, levels = c("burden_CO2_national","burden_CO2_electricity","burden_CO2_transport"))


ggplot(hh_burden2, aes(x = factor(Income_Group_5), fill = category))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group = category), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.151))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Energy Sources")+
  scale_fill_manual(values=c("#9b2226","#ca6702","#194A84"), labels = c("Burden (total)", "Burden Electricity", "Burden Transport")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")
  
  # dataframe
  write.csv(hh_burden2, "Additional_costs_ener.csv", row.names = FALSE)


## Revenue Recycling ####

# 
# ### half of revenues equally
# 
# CP_per_capita = sum(hh_final$exp_CO2_national) / nrow(hh_final)
# CP_per_capita_half = (sum(hh_final$exp_CO2_national) / nrow(hh_final)) / 2
# 
# #hh_final$burden_CO2_national_revenue <- (hh_final$exp_CO2_national - CP_per_capita) / hh_final$hh_expenditures_USD_2014
# hh_final$burden_CO2_national_revenue2 <- ((hh_final$exp_CO2_national - CP_per_capita_half) / hh_final$hh_expenditures_USD_2014) *(-1)
# 
# 
# # Distributional effects - Deciles
# hh_final_1 <- hh_final %>%
#   group_by(Income_Group_10)%>%
#   summarise(
#     y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
#     y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
#     y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
#     y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
#     y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
#     mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
#   ungroup()
# 
# ggplot(hh_final_1, aes(x = factor(Income_Group_10)))+
#   geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5) +
#   theme_bw()+
#   xlab("Expenditure Deciles")+
#   ylab("Additional Costs (% of Total Expenditures)")+
#   geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "white")+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1 ), expand = c(0,0))+
#   scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10 \n Richest \n 10 Percent"))+
#   coord_cartesian(ylim = c(0,0.2))+
#   ggtitle("Distributional Effects of a national Carbon Price of US-$ 30/tCO2 in South Africa")+
#   theme(axis.text = element_text(size = 8), 
#         axis.title = element_text(size = 8),
#         plot.title = element_text(size = 11),
#         plot.subtitle = element_text(size = 10),
#         legend.position = "bottom",
#         plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))
# 
# 
# 
# # Distributional effects - Quintiles
# 
# hh_final_2 <- hh_final %>%
#   group_by(Income_Group_5)%>%
#   summarise(
#     y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
#     y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
#     y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
#     y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
#     y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
#     mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
#   ungroup()
# 
# ggplot(hh_final_2, aes(x = factor(Income_Group_5)))+
#   geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
#   theme_bw()+
#   xlab("Expenditure Quintiles")+
#   ylab("Additional Costs (% of Total Expenditures)")+
#   geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "white", position=position_dodge(width=0.5))+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
#   scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
#   coord_cartesian(ylim = c(0,0.2))+
#   ggtitle("Distributional Effects of a national Carbon Price of US-$ 30/tCO2 on Population in South")+
#   scale_color_manual(values=c("#F8766D")) + 
#   theme(axis.text = element_text(size = 12), 
#         axis.title = element_text(size = 12),
#         plot.title = element_text(size = 12),
#         plot.subtitle = element_text(size = 12),
#         legend.position = "bottom",
#         plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
#   labs(colour = "")
# 
# 
# 
# 
# ###### WICHTIG!!!! Burden - Additional Costs negativ
# 
# 
# hh_final_2n <- hh_final_2 *(-1)
# hh_final_2n$Income_Group_5 <- hh_final_2n$Income_Group_5 *(-1)
# 
# 
# ggplot(hh_final_2n, aes(x = factor(Income_Group_5)))+
#   geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
#   theme_bw()+
#   xlab("Expenditure Quintiles")+
#   ylab("Additional Costs (% of Total Expenditures)")+
#   geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "white", position=position_dodge(width=0.5))+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
#   scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
#   coord_cartesian(ylim = c(-0.2,0))+
#   ggtitle("Distributional Effects of a national Carbon Price of US-$ 30/tCO2 on Population in South Africa")+
#   scale_color_manual(values=c("#F8766D")) + 
#   theme(axis.text = element_text(size = 12), 
#         axis.title = element_text(size = 12),
#         plot.title = element_text(size = 12),
#         plot.subtitle = element_text(size = 12),
#         legend.position = "bottom",
#         plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
#   labs(colour = "")
# 
# 
# 
# #### Net Income half revenues
# 
# hh_final_3 <- hh_final %>%
#   group_by(Income_Group_5)%>%
#   summarise(
#     y5  = wtd.quantile(burden_CO2_national_revenue2, weights = hh_weights, probs = 0.05),
#     y25 = wtd.quantile(burden_CO2_national_revenue2, weights = hh_weights, probs = 0.25),
#     y50 = wtd.quantile(burden_CO2_national_revenue2, weights = hh_weights, probs = 0.5),
#     y75 = wtd.quantile(burden_CO2_national_revenue2, weights = hh_weights, probs = 0.75),
#     y95 = wtd.quantile(burden_CO2_national_revenue2, weights = hh_weights, probs = 0.95),
#     mean = wtd.mean(burden_CO2_national_revenue2, weights = hh_weights))%>%
#   ungroup()
# 
# ggplot(hh_final_3, aes(x = factor(Income_Group_5)))+
#   geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
#   theme_bw()+
#   xlab("Expenditure Quintiles")+
#   ylab("Net Income Gains (% of Total Expenditures)")+
#   geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "white", position=position_dodge(width=0.5))+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.3, 0.3))+
#   scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
#   coord_cartesian(ylim = c(-0.08,0.27))+
#   ggtitle("Redistribution of half of the revenue")+
#   scale_color_manual(values=c("#F8766D", "#00BFC4")) + 
#   theme(axis.text = element_text(size = 8), 
#         axis.title = element_text(size = 8),
#         plot.title = element_text(size = 11),
#         plot.subtitle = element_text(size = 10),
#         legend.position = "bottom",
#         plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
#   labs(colour = "")
# 
# #### Burden/Additional costs und net income revenues nebeneinander
# 
# #hh_final_2n = burden
# #hh_final_3 = net income
# 
# # burden=1 -> burden      burden=2 -> income
# 
# hh_final_2n$burden <- c(1)
# hh_final_3$burden <- c(2)
# 
# hh_final_f <- rbind(hh_final_2n,hh_final_3)
# 
# hh_final_f <- hh_final_f %>%
#   relocate (burden, .before = y5)
# 
# hh_final_f [order(hh_final_f$Income_Group_5),]
# 
# str(hh_final_f)
# 
# str(hh_final_2)
# 
# 
# hh_final_f$burden <- as.factor(hh_final_f$burden)
# 
# 
# 
# vs <- ggplot(hh_final_f, aes(x = factor(Income_Group_5), color = burden))+
#   geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
#   theme_bw()+
#   xlab("Expenditure Quintiles")+
#   ylab("Additional Costs vs. Net Income Gain \n (% of Total Expenditures)")+
#   geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "white", position=position_dodge(width=0.5))+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
#   scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
#   coord_cartesian(ylim = c(-0.2,0.3))+
#   ggtitle("Distributional Effects of a national Carbon Tax of US-$ 30/tCO2 vs. Net Income Gains from half redistribution")+
#   scale_color_manual(values=c("#F8766D", "#00BFC4"), labels = c("Additional Costs", "Net Income Gains")) + 
#   theme(axis.text = element_text(size = 8), 
#         axis.title = element_text(size = 8),
#         plot.title = element_text(size = 8),
#         plot.subtitle = element_text(size = 8),
#         legend.position = "bottom",
#         legend.text = element_text(size=8),
#         plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
#   labs(colour = "")
# 
# vs + geom_hline(yintercept=0)



#### Lump Sum ##

### 25%, 50%, 75%, 100% equally

CP_per_capita = sum(hh_final$exp_CO2_national) / nrow(hh_final)
CP_per_capita_50 = (sum(hh_final$exp_CO2_national) / nrow(hh_final)) / 2
CP_per_capita_25 = (sum(hh_final$exp_CO2_national) / nrow(hh_final)) / 4
CP_per_capita_75 = (sum(hh_final$exp_CO2_national) / nrow(hh_final)) / 4*3


#hh_final$burden_CO2_national_revenue <- (hh_final$exp_CO2_national - CP_per_capita) / hh_final$hh_expenditures_USD_2014
hh_final$burden_CO2_national_revenue50 <- ((hh_final$exp_CO2_national - CP_per_capita_50) / hh_final$hh_expenditures_USD_2014) *(-1)
hh_final$burden_CO2_national_revenue100 <- ((hh_final$exp_CO2_national - CP_per_capita) / hh_final$hh_expenditures_USD_2014) *(-1)
hh_final$burden_CO2_national_revenue75 <- ((hh_final$exp_CO2_national - CP_per_capita_75) / hh_final$hh_expenditures_USD_2014) *(-1)
hh_final$burden_CO2_national_revenue25 <- ((hh_final$exp_CO2_national - CP_per_capita_25) / hh_final$hh_expenditures_USD_2014) *(-1)


hh_final_0 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

#negativ
hh_final_0n <- hh_final_0 *(-1)
hh_final_0n$Income_Group_5 <- hh_final_0n$Income_Group_5 *(-1)

hh_final_0n$category <- "burden_CO2_national"


hh_final_1 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national_revenue50, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national_revenue50, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national_revenue50, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national_revenue50, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national_revenue50, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national_revenue50, weights = hh_weights))%>%
  ungroup()

hh_final_1$category <- "burden_CO2_revenue50"

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national_revenue100, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national_revenue100, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national_revenue100, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national_revenue100, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national_revenue100, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national_revenue100, weights = hh_weights))%>%
  ungroup()

hh_final_2$category <- "burden_CO2_revenue100"

hh_final_3 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national_revenue75, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national_revenue75, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national_revenue75, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national_revenue75, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national_revenue75, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national_revenue75, weights = hh_weights))%>%
  ungroup()

hh_final_3$category <- "burden_CO2_revenue75"

hh_final_4 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national_revenue25, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national_revenue25, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national_revenue25, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national_revenue25, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national_revenue25, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national_revenue25, weights = hh_weights))%>%
  ungroup()

hh_final_4$category <- "burden_CO2_revenue25"


hh_revenue0 <- merge(hh_final_1, hh_final_0n, all=TRUE)
hh_revenue1 <- merge(hh_revenue0, hh_final_2, all=TRUE)
hh_revenue2 <- merge(hh_revenue1, hh_final_3, all=TRUE)
hh_revenue <- merge(hh_revenue2, hh_final_4, all=TRUE)

hh_revenue$category <- factor(hh_revenue$category, levels = c("burden_CO2_national" ,"burden_CO2_revenue25","burden_CO2_revenue50","burden_CO2_revenue75","burden_CO2_revenue100"))


vs <- ggplot(hh_revenue, aes(x = factor(Income_Group_5), fill = category))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Net Income Gain (% of total expenditures)")+
  geom_point(aes(y = mean, group=category), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(-0.15,0.65))+
  ggtitle("Redistribution Mechanism - Lump Sum with a National Carbon Tax of US$ 30/tCO2")+
  scale_fill_manual(values=c("#9b2226","#F8766D", "#00BFC4", "#E69F00", "#669999"), labels = c("Total Burden","Revenue 25%","Revenue 50%","Revenue 75%","Revenue 100%")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")

vs + geom_hline(yintercept=0)

  
  # dataframe
  write.csv(hh_revenue, "Additional_costs_rr_ls.csv", row.names = FALSE)

### check revenues ####
sum_revenues <- sum(hh_final[["exp_CO2_national"]])




## Vat reduction on all food items ####

hh_final$expenditures_food <- hh_final$hh_expenditures_USD_2014*hh_final$share_food
hh_final$vat_15 <- hh_final$expenditures_food *0.15
hh_final$vat_15_2 <- hh_final$expenditures_food *0.15/2

sum_vat <- sum(hh_final[["vat_15"]])
sum_vat2 <- sum(hh_final[["vat_15_2"]])



# change in budget
hh_final$change_exp_food_vat <- ((hh_final$hh_expenditures_USD_2014 - hh_final$vat_15)-hh_final$hh_expenditures_USD_2014)/hh_final$hh_expenditures_USD_2014*(-1)
hh_final$change_exp_food_vat_2 <- ((hh_final$hh_expenditures_USD_2014 - hh_final$vat_15_2)-hh_final$hh_expenditures_USD_2014)/hh_final$hh_expenditures_USD_2014*(-1)

# plot
hh_final_1 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(change_exp_food_vat, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(change_exp_food_vat, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(change_exp_food_vat, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(change_exp_food_vat, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(change_exp_food_vat, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(change_exp_food_vat, weights = hh_weights))%>%
  ungroup()

hh_final_1$category <- "change_exp_food_vat"

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(change_exp_food_vat_2, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(change_exp_food_vat_2, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(change_exp_food_vat_2, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(change_exp_food_vat_2, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(change_exp_food_vat_2, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(change_exp_food_vat_2, weights = hh_weights))%>%
  ungroup()

hh_final_2$category <- "change_exp_food_vat_2"


hh_food_vat <- merge(hh_final_1, hh_final_2, all=TRUE)


hh_food_vat$category <- factor(hh_food_vat$category, levels = c("change_exp_food_vat","change_exp_food_vat_2"))


ggplot(hh_food_vat, aes(x = factor(Income_Group_5), color = category))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Income Gain (% of Total Expenditures)")+
  geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "white", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(-0.02,0.15))+
  ggtitle("Redistribution Mechanism VAT Removal for food - National Carbon Tax of US$30")+
  scale_color_manual(values=c("#F8766D", "#00BFC4"), labels = c("Full VAT Removal for food","50% VAT Removal for food")) + 
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")


# Net income gain VAT removal
hh_final$burden_vat <- ((hh_final$exp_CO2_national - hh_final$vat_15) / hh_final$hh_expenditures_USD_2014) *(-1)
hh_final$burden_vat2 <- ((hh_final$exp_CO2_national - hh_final$vat_15_2) / hh_final$hh_expenditures_USD_2014) *(-1)         

# plot
hh_final_1 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_vat, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_vat, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_vat, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_vat, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_vat, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_vat, weights = hh_weights))%>%
  ungroup()

hh_final_1$category <- "burden_vat"

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_vat2, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_vat2, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_vat2, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_vat2, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_vat2, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_vat2, weights = hh_weights))%>%
  ungroup()

hh_final_2$category <- "burden_vat2"

# Burden all
hh_final_3 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

hh_final_3$category <- "burden"

hh_final_3$y5 <- hh_final_3$y5 *(-1)
hh_final_3$y25 <- hh_final_3$y25 *(-1)
hh_final_3$y50 <- hh_final_3$y50 *(-1)
hh_final_3$y75 <- hh_final_3$y75 *(-1)
hh_final_3$y95 <- hh_final_3$y95 *(-1)
hh_final_3$mean <- hh_final_3$mean *(-1)

hh_food_vat <- merge(hh_final_1, hh_final_2, all=TRUE)
hh_food_vat2 <- merge(hh_food_vat, hh_final_3, all=TRUE)


hh_food_vat2$category <- factor(hh_food_vat2$category, levels = c("burden","burden_vat2", "burden_vat"))


vs <- ggplot(hh_food_vat2, aes(x = factor(Income_Group_5), fill = category))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Net Income Gain (% of total expenditures)")+
  geom_point(aes(y = mean, group=category), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(-0.15,0.1))+
  ggtitle("Redistribution Mechanism - VAT Removal Food with National Carbon Tax of US$ 30/tCO2")+
  scale_fill_manual(values=c("#9b2226", "#00BFC4", "#669999"), labels = c("Total Burden", "50% VAT Removal","100% VAT Removal")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")   

vs + geom_hline(yintercept=0)
  
  # dataframe
  write.csv(hh_food_vat2, "Additional_costs_rr_food.csv", row.names = FALSE)


  
## Electricity Subsidy Carbon tax ####

hh_final$exp_CO2_electricity2 <- hh_final$exp_CO2_electricity/2

sum_electricity_co2 <- sum(hh_final[["exp_CO2_electricity"]])
sum_electricity_co22 <- sum(hh_final[["exp_CO2_electricity2"]])


# Net income gain elec sub
hh_final$burden_elec_sub <- ((hh_final$exp_CO2_national - hh_final$exp_CO2_electricity) / hh_final$hh_expenditures_USD_2014) *(-1)
hh_final$burden_elec_sub2 <- ((hh_final$exp_CO2_national - hh_final$exp_CO2_electricity2) / hh_final$hh_expenditures_USD_2014) *(-1)         



# plot
hh_final_1 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_elec_sub, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_elec_sub, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_elec_sub, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_elec_sub, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_elec_sub, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_elec_sub, weights = hh_weights))%>%
  ungroup()

hh_final_1$category <- "burden_elec_sub"

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_elec_sub2, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_elec_sub2, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_elec_sub2, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_elec_sub2, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_elec_sub2, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_elec_sub2, weights = hh_weights))%>%
  ungroup()

hh_final_2$category <- "burden_elec_sub2"


# Burden all
hh_final_3 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

hh_final_3$category <- "burden"

hh_final_3$y5 <- hh_final_3$y5 *(-1)
hh_final_3$y25 <- hh_final_3$y25 *(-1)
hh_final_3$y50 <- hh_final_3$y50 *(-1)
hh_final_3$y75 <- hh_final_3$y75 *(-1)
hh_final_3$y95 <- hh_final_3$y95 *(-1)
hh_final_3$mean <- hh_final_3$mean *(-1)


hh_elec_sub <- merge(hh_final_1, hh_final_2, all=TRUE)
hh_elec_sub2 <- merge(hh_elec_sub, hh_final_3, all=TRUE)


hh_elec_sub2$category <- factor(hh_elec_sub2$category, levels = c("burden","burden_elec_sub2", "burden_elec_sub"))


vs <- ggplot(hh_elec_sub2, aes(x = factor(Income_Group_5), fill = category))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Net Income Gain (% of total expenditures)")+
  geom_point(aes(y = mean, group=category), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(-0.15,0.05))+
  ggtitle("Redistribution Mechanism - Electricity Subsidy with National Carbon Tax of US$ 30t/CO2")+
  scale_fill_manual(values=c("#9b2226", "#00BFC4", "#669999"), labels = c("Total Burden", "50% CO2 Tax Electricity Subsidy","100%  CO2 Tax Electricity Subsidy")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")   

vs + geom_hline(yintercept=0)


# dataframe
write.csv(hh_elec_sub2, "Additional_costs_rr_elecsub.csv", row.names = FALSE)

## Expenditure / Income Deciles ####

hh_final_dec <- hh_final %>%
  group_by(Income_Group_10) %>%
  summarise(
  mean_hh_expendituresUSD = mean(hh_expenditures_USD_2014)) %>%
  ungroup()

# dataframe
write.csv(hh_final_dec, "Expenditure Deciles.csv", row.names = FALSE)
  