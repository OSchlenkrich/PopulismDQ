# Predicitions

get_prediction = function(model, effect, lagvar, lagvar2 = NULL, upper = T, cred = 0.95, time = 5) {
  library(HDInterval)
  
  pred_frame = model$data %>% 
    #filter(country_name == "Hungary") %>% 
    summarise_if(is.numeric, funs(mean))
  
  # filter(country_name == "Hungary") %>% 
  # slice(1) %>% 
  #mutate_at(vars(matches("caus_wi")), funs(. -.) ) %>%
  #mutate_at(vars(matches("caus_cbw")), funs(. -.) ) %>%
  #mutate_at(vars(matches("caus_ybw")), funs(. -.) ) %>%
  
  if (upper == T) {
    pop_effect = 1 - pred_frame$pop_HOG_caus_cbw - pred_frame$pop_HOG_caus_ybw
    
  }  else {
    pop_effect = 0 - pred_frame$pop_HOG_caus_cbw - pred_frame$pop_HOG_caus_ybw
  }
  
  pred_frame  =  pred_frame   %>% 
    mutate(country_name = factor("Germany", levels = levels(as.factor(model$data$country_name))),
           year = factor("1994", levels = levels(as.factor(model$data$year)))) %>% 
    slice(rep(1:n(), each = time)) %>% 
    mutate(pop_HOG_caus_wi = c(pred_frame$pop_HOG_caus_wi, rep(pop_effect, time-1))) %>% 
    mutate(pop_HOG_caus_wi_lag = c(pred_frame$pop_HOG_caus_wi, pred_frame$pop_HOG_caus_wi, rep(pop_effect, time-2)))
  
  print(pred_frame)
  
  # substract to get wi
  cvariable = gsub("_wi_lag", "_lag_depcbw", lagvar)
  yvariable = gsub("_wi_lag", "_lag_depybw", lagvar)
  
  test = brms_ml_data %>% 
    select(country_name, year, depcbw = cvariable, depybw = yvariable) %>% 
    #filter(country_name == "Germany") %>% 
    summarise(depcbw = mean(depcbw, na.rm=T), 
              depybw = mean(depybw, na.rm=T))
  
  #mean_sub = 500 - test$cbw - test$ybw 
  
  res = data.frame(mean = rep(NA, time),
                   lower = rep(NA, time),
                   upper = rep(NA, time))
  
  for (i in 2:time) {
    draws = fitted(model, newdata = pred_frame[i-1,], nsamples = 2000, summary = F, re_formula = NA)
    estimate = mean(draws) - test$depcbw - test$depybw
    
    
    pred_frame[i,lagvar] = estimate
    if(is.null(lagvar2) == F) {
      pred_frame[i,lagvar2] = pred_frame[i-1,lagvar]
    }
    
    res$mean[i] =  mean(draws)
    res$lower[i] = hdi(draws, credMass = cred)[1]
    res$upper[i] = hdi(draws, credMass = cred)[2]
    
    
  }
  
  # transform values
  print(res)
  # lambda_sel = gsub("_wi_lag", "", lagvar)
  # res = res %>% 
  #   mutate_all(funs(ifelse( . < 0, 1, .)))
  # res$mean = reverse_transformation(res$mean, lambda_sel)
  # res$lower = reverse_transformation(res$lower, lambda_sel)
  # res$upper = reverse_transformation(res$upper, lambda_sel)
  
  return(res%>% 
           mutate(time = 1:time))
}

get_prediction_posterior = function(model, lagvar, lagvar2 = NULL, upper = T, cred = 0.95, time = 5) {
  library(HDInterval)
  
  pred_frame = model$data %>% 
    #filter(country_name == "Hungary") %>% 
    summarise_if(is.numeric, funs(mean))
  
  # filter(country_name == "Hungary") %>% 
  # slice(1) %>% 
  #mutate_at(vars(matches("caus_wi")), funs(. -.) ) %>%
  #mutate_at(vars(matches("caus_cbw")), funs(. -.) ) %>%
  #mutate_at(vars(matches("caus_ybw")), funs(. -.) ) %>%
  
  if (upper == T) {
    pop_effect = 1 - pred_frame$pop_HOG_caus_cbw - pred_frame$pop_HOG_caus_ybw
    
  }  else {
    pop_effect = 0 - pred_frame$pop_HOG_caus_cbw - pred_frame$pop_HOG_caus_ybw
  }
  
  pred_frame  =  pred_frame   %>% 
    mutate(country_name = factor("Germany", levels = levels(as.factor(model$data$country_name))),
           year = factor("1994", levels = levels(as.factor(model$data$year)))) %>% 
    slice(rep(1:n(), each = time)) %>% 
    mutate(pop_HOG_caus_wi = c(pred_frame$pop_HOG_caus_wi, rep(pop_effect, time-1))) %>% 
    mutate(pop_HOG_caus_wi_lag = c(pred_frame$pop_HOG_caus_wi, pred_frame$pop_HOG_caus_wi, rep(pop_effect, time-2)))
  
  print(pred_frame)
  
  # substract to get wi
  cvariable = gsub("_wi_lag", "_lag_depcbw", lagvar)
  yvariable = gsub("_wi_lag", "_lag_depybw", lagvar)
  
  test = brms_ml_data %>% 
    select(country_name, year, depcbw = cvariable, depybw = yvariable) %>% 
    #filter(country_name == "Germany") %>% 
    summarise(depcbw = mean(depcbw, na.rm=T), 
              depybw = mean(depybw, na.rm=T))
  
  #mean_sub = 500 - test$cbw - test$ybw 
  
  res = list()
  
  for (i in 2:time) {
    draws = fitted(model, newdata = pred_frame[i-1,], nsamples = 2000, summary = F, re_formula = NA)
    estimate = mean(draws) - test$depcbw - test$depybw
    
    
    pred_frame[i,lagvar] = estimate
    if(is.null(lagvar2) == F) {
      pred_frame[i,lagvar2] = pred_frame[i-1,lagvar]
    }
    
    res[[i]] =  data.frame(draws = draws, time = i)

  }
  
  return(res)
}


plot_prediction = function(model, variablel1, variablel2 = NULL, cred=0.95, time=9) {
  testu = get_prediction(model, 1, variablel1, variablel2, upper=T, cred=cred, time = time) %>% 
    mutate(Scen = "upper") 
  
  testl = get_prediction(model, -1, variablel1, variablel2, upper=F, cred=cred, time = time) %>% 
    mutate(Scen = "lower") 
  
  #
  title = gsub("_wi_lag","",variablel1)

  p1 = testu %>% 
    bind_rows(testl) %>%
    mutate(time = time - 1) %>%
    na.omit() %>% 
    ggplot(aes(x=time, y= mean, ymin =lower, ymax = upper,  col=Scen,  fill=Scen)) +
    geom_line() +
    geom_ribbon(alpha = 0.5) +
    ggtitle(title) +
    theme_bw() +
    scale_x_continuous(breaks=seq(0,30,2)) +
    scale_y_continuous(name = "DQ")
  
  return(list(p1, testu %>% 
                bind_rows(testl)))
}

plot_prediction(dec_f, "decision_freedom_context_wi_lag", "decision_freedom_context_wi_lag_lag2")
plot_prediction(dec_e, "decision_equality_context_wi_lag")
plot_prediction(dec_c, "decision_control_context_wi_lag")
plot_prediction(int_f, "intermediate_freedom_context_wi_lag", "intermediate_freedom_context_wi_lag_lag2")
plot_prediction(int_e, "intermediate_equality_context_wi_lag")
plot_prediction(int_c, "intermediate_control_context_wi_lag", "decision_control_context_wi_lag_lag2")
plot_prediction(com_f, "communication_freedom_context_wi_lag", "communication_freedom_context_wi_lag_lag2")
plot_prediction(com_e, "communication_equality_context_wi_lag")
plot_prediction(com_c, "communication_control_context_wi_lag", "communication_control_context_wi_lag_lag2")
plot_prediction(rights_f, "rights_freedom_context_wi_lag", "rights_freedom_context_wi_lag_lag2")
plot_prediction(rights_e, "rights_equality_context_wi_lag", "rights_equality_context_wi_lag_lag2")
plot_prediction(rights_c, "rights_control_context_wi_lag", "rights_control_context_wi_lag_lag2")
plot_prediction(rs_f, "rule_settlement_freedom_context_wi_lag", "rule_settlement_freedom_context_wi_lag_lag2")
plot_prediction(rs_e, "rule_settlement_equality_context_wi_lag")
plot_prediction(rs_c, "rule_settlement_control_context_wi_lag", "rule_settlement_control_context_wi_lag_lag2")

# Subset
plot_prediction(dec_f_sub, "decision_freedom_context_wi_lag", "decision_freedom_context_wi_lag_lag2")
plot_prediction(dec_e_sub, "decision_equality_context_wi_lag")
plot_prediction(dec_c_sub, "decision_control_context_wi_lag")
plot_prediction(int_f_sub, "intermediate_freedom_context_wi_lag", "intermediate_freedom_context_wi_lag_lag2")
plot_prediction(int_e_sub, "intermediate_equality_context_wi_lag")
plot_prediction(int_c_sub, "intermediate_control_context_wi_lag", "decision_control_context_wi_lag_lag2")
plot_prediction(com_f_sub, "communication_freedom_context_wi_lag", "communication_freedom_context_wi_lag_lag2")
plot_prediction(com_e_sub, "communication_equality_context_wi_lag")
plot_prediction(com_c_sub, "communication_control_context_wi_lag", "communication_control_context_wi_lag_lag2")
plot_prediction(rights_f_sub, "rights_freedom_context_wi_lag", "rights_freedom_context_wi_lag_lag2")
plot_prediction(rights_e_sub, "rights_equality_context_wi_lag", "rights_equality_context_wi_lag_lag2")
plot_prediction(rights_c_sub, "rights_control_context_wi_lag", "rights_control_context_wi_lag_lag2")
plot_prediction(rs_f_sub, "rule_settlement_freedom_context_wi_lag", "rule_settlement_freedom_context_wi_lag_lag2")
plot_prediction(rs_e_sub, "rule_settlement_equality_context_wi_lag")
plot_prediction(rs_c_sub, "rule_settlement_control_context_wi_lag", "rule_settlement_control_context_wi_lag_lag2")

# Matrix 

getdifferences = function(model, variablel1, variablel2 = NULL, cred=0.95, time=9) {
  testu = get_prediction_posterior(model, variablel1, variablel2, upper=T, cred=cred, time = time)
  testl = get_prediction_posterior(model, variablel1, variablel2, upper=F, cred=cred, time = time)

  label = gsub("_wi_lag","",variablel1)
  # # Difference between No Populist and Populsit
  df = data.frame(variable = label,
             mean = colMeans(testu[[time]] - testl[[time]])[1],
             lower = hdi(testu[[time]] - testl[[time]])[1],
             upper = hdi(testu[[time]] - testl[[time]])[2],
             row.names = NULL)
  
  # df = data.frame(variable = label,
  #            mean = colMeans(testu[[time]] - testl[[2]])[1],
  #            lower = hdi(testu[[time]] - testl[[2]])[1],
  #            upper = hdi(testu[[time]] - testl[[2]])[2],
  #            row.names = NULL)

  return(df)
}

diff_dec_f = getdifferences(dec_f, "decision_freedom_context_wi_lag", "decision_freedom_context_wi_lag_lag2")
diff_dec_e = getdifferences(dec_e, "decision_equality_context_wi_lag")
diff_dec_c = getdifferences(dec_c, "decision_control_context_wi_lag")
diff_int_f = getdifferences(int_f, "intermediate_freedom_context_wi_lag", "intermediate_freedom_context_wi_lag_lag2")
diff_int_e = getdifferences(int_e, "intermediate_equality_context_wi_lag")
diff_int_c = getdifferences(int_c, "intermediate_control_context_wi_lag", "decision_control_context_wi_lag_lag2")
diff_com_f = getdifferences(com_f, "communication_freedom_context_wi_lag", "communication_freedom_context_wi_lag_lag2")
diff_com_e = getdifferences(com_e, "communication_equality_context_wi_lag")
diff_com_c = getdifferences(com_c, "communication_control_context_wi_lag", "communication_control_context_wi_lag_lag2")
diff_gr_f = getdifferences(rights_f, "rights_freedom_context_wi_lag", "rights_freedom_context_wi_lag_lag2")
diff_gr_e = getdifferences(rights_e, "rights_equality_context_wi_lag", "rights_equality_context_wi_lag_lag2")
diff_gr_c = getdifferences(rights_c, "rights_control_context_wi_lag", "rights_control_context_wi_lag_lag2")
diff_rs_f = getdifferences(rs_f, "rule_settlement_freedom_context_wi_lag", "rule_settlement_freedom_context_wi_lag_lag2")
diff_rs_e = getdifferences(rs_e, "rule_settlement_equality_context_wi_lag")
diff_rs_c = getdifferences(rs_c, "rule_settlement_control_context_wi_lag", "rule_settlement_control_context_wi_lag_lag2")


# Subset
diff_dec_f = getdifferences(dec_f_sub, "decision_freedom_context_wi_lag", "decision_freedom_context_wi_lag_lag2")
diff_dec_e = getdifferences(dec_e_sub, "decision_equality_context_wi_lag")
diff_dec_c = getdifferences(dec_c_sub, "decision_control_context_wi_lag")
diff_int_f = getdifferences(int_f_sub, "intermediate_freedom_context_wi_lag", "intermediate_freedom_context_wi_lag_lag2")
diff_int_e = getdifferences(int_e_sub, "intermediate_equality_context_wi_lag")
diff_int_c = getdifferences(int_c_sub, "intermediate_control_context_wi_lag", "decision_control_context_wi_lag_lag2")
diff_com_f = getdifferences(com_f_sub, "communication_freedom_context_wi_lag", "communication_freedom_context_wi_lag_lag2")
diff_com_e = getdifferences(com_e_sub, "communication_equality_context_wi_lag")
diff_com_c = getdifferences(com_c_sub, "communication_control_context_wi_lag", "communication_control_context_wi_lag_lag2")
diff_gr_f = getdifferences(rights_f_sub, "rights_freedom_context_wi_lag", "rights_freedom_context_wi_lag_lag2")
diff_gr_e = getdifferences(rights_e_sub, "rights_equality_context_wi_lag", "rights_equality_context_wi_lag_lag2")
diff_gr_c = getdifferences(rights_c_sub, "rights_control_context_wi_lag", "rights_control_context_wi_lag_lag2")
diff_rs_f = getdifferences(rs_f_sub, "rule_settlement_freedom_context_wi_lag", "rule_settlement_freedom_context_wi_lag_lag2")
diff_rs_e = getdifferences(rs_e_sub, "rule_settlement_equality_context_wi_lag")
diff_rs_c = getdifferences(rs_c_sub, "rule_settlement_control_context_wi_lag", "rule_settlement_control_context_wi_lag_lag2")


matrix = diff_dec_f %>% 
  bind_rows(diff_dec_e,
            diff_dec_c,
            diff_int_f,
            diff_int_e,
            diff_int_c,
            diff_com_f,
            diff_com_e,
            diff_com_c,
            diff_gr_f,
            diff_gr_e,
            diff_gr_c,
            diff_rs_f,
            diff_rs_e,
            diff_rs_c) %>% 
  mutate(Dim = str_extract(variable, "(?<=_).*(?=_)"),
         Dim = gsub("settlement_", "", Dim),
         Int = str_extract(variable, ".*(?=_)"),
         Int = str_extract(Int, ".*(?=_)")) %>% 
  mutate(Dim = ifelse(Dim == "freedom", "Freiheit", Dim),
         Dim = ifelse(Dim == "equality", "Gleichheit", Dim),
         Dim = ifelse(Dim == "control", "Kontrolle", Dim)) %>% 
  mutate(Int = ifelse(Int == "decision", "Entscheidungs-\nverfahren", Int),
         Int = ifelse(Int == "intermediate", "Intermediäre \nVermittlung", Int),
         Int = ifelse(Int == "communication", "Kommunikation", Int),
         Int = ifelse(Int == "rights", "Rechtsgarantie", Int),
         Int = ifelse(Int == "rule_settlement", "Regelsetzung und \n-anwendung", Int)) %>% 
  mutate(text_label = paste(round(mean,3), "\n", "(", round(lower,3 ), " \u2013 ", round(upper, 3),")", sep=""))

matrix_complete = matrix %>% 
  bind_rows(matrix %>% 
              group_by(Dim) %>% 
              summarise(mean = mean(mean)) %>% 
              mutate(Int = "Dimension")  %>% 
              mutate(text_label = paste(round(mean,3), sep=""))) %>% 
  bind_rows(matrix %>% 
              group_by(Int) %>% 
              summarise(mean = mean(mean)) %>% 
              mutate(Dim = "Institution") %>% 
              mutate(text_label = paste(round(mean,3), sep="")))


yaxis <- factor(c("Dimension", "Regelsetzung und \n-anwendung", "Rechtsgarantie", "Kommunikation", "Intermediäre \nVermittlung", "Entscheidungs-\nverfahren"), 
                levels=c("Entscheidungs-\nverfahren", "Intermediäre \nVermittlung", "Kommunikation", "Rechtsgarantie", "Regelsetzung und \n-anwendung", "Dimension"))
xaxis <- factor(c("Freiheit", "Gleichheit", "Kontrolle", "Institution"), levels=c("Freiheit", "Gleichheit", "Kontrolle", "Institution"))

high = "#E6E6E6"
mid = "#AEAEAE"
low= "#4D4D4D"
midpoint = -0.15
limit=c(-0.302,0.01)
gray.colors(3)

ggplot(data = matrix_complete, aes(x = Dim, y = Int, fill=mean)) +  
  geom_raster()  + 
  geom_text(aes(Dim, Int, label = text_label), color = "black", size = 4, hjust=0.5, vjust=0.5)  + 
  scale_y_discrete(limits=yaxis, expand = c(0, 0)) +  
  scale_x_discrete(limits=xaxis, expand = c(0, 0), position = "top") +  
  theme(legend.position = "none", axis.title = element_blank(), plot.title = element_text(size=10, hjust=0.5), axis.text.x = element_text(face="bold"), axis.text.y = element_text(face="bold")) + 
  scale_fill_gradient2(low = low, high = high, mid=mid, midpoint = midpoint, limit = limit, space = "Lab", name="DQ")  +
  geom_rect(aes(xmin=0.5, xmax=4.5, ymin=0.5, ymax=1.5), fill=NA, color="black", size=1.5) + 
  geom_rect(aes(xmin=3.5, xmax=4.5, ymin=0.5, ymax=6.5), fill=NA, color="black", size=1.5) + 
  theme(panel.background = element_rect(fill = "white",  colour = "white"))

