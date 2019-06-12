source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")
source("Identify_Populism/Identify_PopulistParties.R")
source("Setup/MergeDatasets.R")
#source("Analysis/Plots_Populism.R")

TSCS_data = DMX_populist %>% 
  mutate(populist_in_cabinet_lag = dplyr::lag(if_else(populist_cabinet > 0, 1, 0), 1),
         populist_in_cabinet = if_else(populist_cabinet > 0, 1, 0),
         populist_party_exists_lag = dplyr::lag(if_else(populist > 0, 1, 0), 1),
         
         populist_perc_seats_lag = dplyr::lag(if_else(populist_perc_seats > 1, 1, populist_perc_seats),1),
         populist_perc_seats = if_else(populist_perc_seats > 1, 1, populist_perc_seats),
         
         populist_l = dplyr::lag(if_else(populist_l > 0, 1, 0), 1),
         populist_r = dplyr::lag(if_else(populist_r > 0, 1, 0), 1),
         
         
         populist_vote_share_tr = if_else(populist_vote_share > 10, 10, populist_vote_share),
         populist_vote_share_tr_lag = dplyr::lag(populist_vote_share_tr, 1),
         
         
         gdp_growth_lag = dplyr::lag(gdp_growth, 1),
         v2csantimv_lag = dplyr::lag(v2csantimv, 1),
         
         
         populist_is_prime_minister_lag = dplyr::lag(if_else(populist_prime_minister > 0, 1, 0), 1),
         populist_is_prime_minister = if_else(populist_prime_minister > 0, 1, 0),
         
         total_index_context_lag = dplyr::lag(total_index_context, 1),
         
         decision_inst_index_context_lag = dplyr::lag(decision_inst_index_context, 1),
         intermediate_inst_index_context_lag = dplyr::lag(intermediate_inst_index_context, 1),
         communication_inst_index_context_lag = dplyr::lag(communication_inst_index_context, 1),
         rights_inst_index_context_lag = dplyr::lag(rights_inst_index_context, 1),
         rule_settlement_inst_index_context_lag = dplyr::lag(rule_settlement_inst_index_context, 1),
         
         freedom_dim_index_context_lag = dplyr::lag(freedom_dim_index_context, 1),
         equality_dim_index_context_lag = dplyr::lag(equality_dim_index_context, 1),
         control_dim_index_context_lag = dplyr::lag(control_dim_index_context, 1),
         
         decision_freedom_context_lag = dplyr::lag(decision_freedom_context, 1),
         decision_equality_context_lag = dplyr::lag(decision_equality_context, 1),
         decision_control_context_lag = dplyr::lag(decision_control_context, 1),
         
         intermediate_freedom_context_lag = dplyr::lag(intermediate_freedom_context, 1),
         intermediate_equality_context_lag = dplyr::lag(intermediate_equality_context, 1),
         intermediate_control_context_lag = dplyr::lag(intermediate_control_context, 1),
         
         communication_freedom_context_lag = dplyr::lag(communication_freedom_context, 1),
         communication_equality_context_lag = dplyr::lag(communication_equality_context, 1),
         communication_control_context_lag = dplyr::lag(communication_control_context, 1),
         
         rights_freedom_context_lag = dplyr::lag(rights_freedom_context, 1),
         rights_equality_context_lag = dplyr::lag(rights_equality_context, 1),
         rights_control_context_lag = dplyr::lag(rights_control_context, 1),
         
         rule_settlement_freedom_context_lag = dplyr::lag(rule_settlement_freedom_context, 1),
         rule_settlement_equality_context_lag = dplyr::lag(rule_settlement_equality_context, 1),
         rule_settlement_control_context_lag = dplyr::lag(rule_settlement_control_context, 1),
        
         ) %>% 
  filter(year >= 1990) %>% 
  mutate(trend = year - min(year))
  # filter(country_name != "Turkey") %>% 
  # filter(country_name != "Hungary") %>% 
  # filter(country_name != "Poland")


# Create Country Dummies
mydummies = onehot(data.frame(TSCS_data$country_name), max_levels = 40)
TSCS_data = TSCS_data %>% 
  bind_cols(data.frame(predict(mydummies, data.frame(TSCS_data$country_name))))
dummies_string = TSCS_data %>% 
  ungroup() %>% 
  select_at(vars(matches("country_name."))) %>% 
  names() %>% 
  paste(collapse = " + ")

# See NAs
data.frame(variables=names(TSCS_data), missings = dim(TSCS_data)[1]-describe(TSCS_data)$n) %>% 
  filter(missings != 0) %>% 
  mutate(variables = fct_reorder(variables, -missings)) %>% 
  ggplot(aes(x=variables, y=missings)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=90))


# Correlation
library(corrplot)
TSCS_data %>% 
  ungroup() %>% 
  select_at(vars(ends_with("lag"), -matches("context"))) %>% 
  cor(use="pairwise") %>% 
  corrplot(method="shade", order="hclust")

# Overview Data

table(TSCS_data$country_name,
      TSCS_data$populist_is_prime_minister_lag)
table(TSCS_data$country_name,
      TSCS_data$populist_in_cabinet_lag)



## VIF
my_VIF_formula = as.formula(paste("total_index_context ~ 1 + 
          total_index_context_lag + 
          populist_in_cabinet_lag + 
          populist_percent_seats_lag  + 
          populist_is_prime_minister_lag +
          populist_party_exists_lag +
          gdp_growth_lag"))

vif_test = lm(my_VIF_formula, TSCS_data)
vif(vif_test)


# Total Value Index
my_tscs_formula = as.formula(paste("control_dim_index_context_df ~ -1 + 
          control_dim_index_context_lag +
          trend +
          populist_in_cabinet_lag + 
          populist_percent_seats_lag  + 
          populist_is_prime_minister_lag + 
          gdp_growth_lag + ", dummies_string, collapse=" + "))


total_value_m = plm(my_tscs_formula, 
           index=c("country_name", "year"), 
           as.data.frame(TSCS_data),
           model="pooling")
summary(total_value_m)
coeftest(total_value_m, vcov=vcovBK)


total_value_m_w = plm(my_tscs_formula, 
                    index=c("country_name", "year"), 
                    as.data.frame(TSCS_data),
                    model="within")
summary(total_value_m_w)
coeftest(total_value_m_w, vcov=vcovBK)

# Dynsim

my_tscs_formula = as.formula(paste("communication_control_context ~ -1 + 
          communication_control_context_lag + 
          populist_perc_seats_lag  + 
          gdp_growth_lag +", dummies_string, collapse=" + "))


total_value_m_w_dynsim = plm(my_tscs_formula, 
                             index=c("country_name", "year"), 
                             as.data.frame(TSCS_data),
                             model="pooling")
coeftest(total_value_m_w_dynsim, vcov=vcovBK)
quantile(TSCS_data$populist_perc_seats_lag, c(0.05, 0.95), na.rm=T)
sc1 = dynamicSim(total_value_m_w_dynsim, 10, 
                 "communication_control_context_lag", 0.5384615, 
                 "populist_perc_seats_lag",
                 "populist_perc_seats_lag high")
sc2 = dynamicSim(total_value_m_w_dynsim, 10, 
                 "communication_control_context_lag", 0, 
                 "populist_perc_seats_lag",
                 "populist_perc_seats_lag low")

sc1 %>% 
  bind_rows(sc2) %>% 
  ggplot(aes(x = x, y=y_mean, ymin=y_min, ymax=y_max, fill=Scenario)) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  scale_y_continuous(name = "Total Value Index (Context)", limits = c(0,1), breaks=seq(0,1,0.2)) +
  scale_x_continuous(name="t", breaks=seq(0,10, 1)) +
  ggtitle("Dynamic Simulation")

#

# Freedom Value Index
my_tscs_formula = as.formula(paste("freedom_dim_index_context ~ -1 + 
          freedom_dim_index_context_lag + 
          trend +
          populist_in_cabinet_lag + 
          populist_percent_seats_lag  + 
          populist_is_prime_minister_lag +
          populist_party_exists_lag  +
          gdp_growth_lag + ", dummies_string, collapse=" + "))

freedom_value_m = plm(my_tscs_formula, 
           index=c("country_name", "year"), 
           as.data.frame(TSCS_data),
           model="pooling")
summary(freedom_value_m)
coeftest(freedom_value_m, vcov=vcovBK)

# vif(test)

freedom_value_m_w = plm(my_tscs_formula, 
           index=c("country_name", "year"), 
           as.data.frame(TSCS_data),
           model="within")
summary(freedom_value_m_w)
coeftest(freedom_value_m_w, vcov=vcovBK)


sc1 = dynamicSim(freedom_value_m_w, 10, 
                 "populist_is_prime_minister_lag", 1, 
                 "freedom_dim_index_context_lag",
                 "Prime Minister 1")
sc2 = dynamicSim(freedom_value_m_w, 10, 
                 "populist_is_prime_minister_lag", 0, 
                 "freedom_dim_index_context_lag",
                 "Prime Minister 0")

sc1 %>% 
  bind_rows(sc2) %>% 
  ggplot(aes(x = x, y=y_mean, ymin=y_min, ymax=y_max, fill=Scenario)) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  theme_bw()


# Equality Value Index
my_tscs_formula = as.formula(paste("equality_dim_index_context ~ -1 + 
          equality_dim_index_context_lag + 
          populist_in_cabinet_lag + 
          populist_percent_seats_lag  + 
          populist_is_prime_minister_lag +
          populist_party_exists_lag  +
          gdp_growth_lag + ", dummies_string, collapse=" + "))

equality_value_m = plm(my_tscs_formula, 
           index=c("country_name", "year"), 
           as.data.frame(TSCS_data),
           model="pooling")
summary(equality_value_m)
coeftest(equality_value_m, vcov=vcovBK)

equality_value_m_w = plm(my_tscs_formula, 
                       index=c("country_name", "year"), 
                       as.data.frame(TSCS_data),
                       model="within")
summary(equality_value_m_w)
coeftest(equality_value_m_w, vcov=vcovBK)


# vif(test)

# Control Value Index
my_tscs_formula = as.formula(paste("control_dim_index_context ~ -1 + 
          control_dim_index_context_lag + 
          trend +
          populist_in_cabinet_lag + 
          populist_percent_seats_lag  + 
          populist_is_prime_minister_lag +
          populist_party_exists_lag  +
          gdp_growth_lag + ", dummies_string, collapse=" + "))

control_value_m = plm(my_tscs_formula, 
           index=c("country_name", "year"), 
           as.data.frame(TSCS_data),
           model="pooling")
summary(control_value_m)
coeftest(control_value_m, vcov=vcovBK)

# vif(test)

control_value_m_w = plm(my_tscs_formula, 
                      index=c("country_name", "year"), 
                      as.data.frame(TSCS_data),
                      model="within")
summary(control_value_m_w)
coeftest(control_value_m_w, vcov=vcovBK)


# Rule Settlement Value Index
my_tscs_formula = as.formula(paste("rule_settlement_inst_index_context ~ -1 + 
          rule_settlement_inst_index_context_lag + 
          trend +
          populist_in_cabinet_lag + 
          populist_percent_seats_lag  + 
          populist_is_prime_minister_lag +
          populist_party_exists_lag +
          gdp_growth_lag +",  dummies_string, collapse=" + "))

rs_value_m = plm(my_tscs_formula, 
                      index=c("country_name", "year"), 
                      as.data.frame(TSCS_data),
                      model="pooling")
summary(rs_value_m)
coeftest(rs_value_m, vcov=vcovBK)

# vif(test)

rs_value_m_w = plm(my_tscs_formula, 
                        index=c("country_name", "year"), 
                        as.data.frame(TSCS_data),
                        model="within")
summary(rs_value_m_w)
coeftest(rs_value_m_w, vcov=vcovBK)
