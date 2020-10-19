##

make_table = function(model, modellabel, credmass = 0.95) {
  
  table = tidy(model, conf.level = credmass, conf.method = "HPDinterval")  %>% 
    mutate(term = ifelse(grepl("context_wi_lag$", term), "Lag", term),
           term = ifelse(grepl("_lag2$", term), "Lag2", term)) %>% 
    mutate(term = gsub("pop_Opp", "Pop.Opp.", term),
           term = gsub("pop_cab", "Pop.Kab.", term),
           term = gsub("pop_HOG", "Pop.Reg.", term),
           term = gsub("gdp_growth", "GDP Gr.", term),
           term = gsub("socequal", "Soz.Glei.", term),
           term = gsub("_caus", "", term),
           term = gsub("_lag", ", L1", term),
           term = gsub("_wi", "<sub>wi", term),
           term = gsub("_cbw", "<sub>J; bw", term),
           term = gsub("_ybw", "<sub>T; bw", term)) %>% 
    mutate(estimate = paste(round(estimate, 2), " (", round(conf.low, 2), " \u2013 ", round(conf.high, 2), ")", sep=""),
           term = if_else(effect == "ran_pars" & group == "country_name", "&tau;" , term),
           term = if_else(effect == "ran_pars" & group == "year", "&upsilon;" , term),
           term = if_else(effect == "ran_pars" & group == "Residual", " &sigma;" , term)) %>%
    mutate(estimate = ifelse((conf.low < 0 &  conf.high < 0) | (conf.low > 0 &  conf.high > 0), paste("<b>", estimate, sep=""), estimate)) %>% 
    mutate_if(is.numeric, funs(round(.,3))) %>% 
    select(Variable = term, !!modellabel := estimate) 
  
  credmass = credmass
  var = rownames(fixef(model))[2]
  var = gsub("_wi_lag", "", var)
  LRM_Opp = lag_distribution_both(model, var, 
                                  IndV_label = "pop_Opp_caus" , 
                                  dep_label = var, 
                                  unit = 1, ci=credmass, ecm = F, justHDI = T)  
  LRM_Cab = lag_distribution_both(model, var, 
                                  IndV_label = "pop_cab_caus" , 
                                  dep_label = var, 
                                  unit = 1, ci=credmass, ecm = F, justHDI = T)  
  LRM_HOG = lag_distribution_both(model, var, 
                                  IndV_label = "pop_HOG_caus" , 
                                  dep_label = var, 
                                  unit = 1, ci=credmass, ecm = F, justHDI = T)  
  
  LRMs = data.frame(LRM_Opp. = mean(LRM_Opp),
                    LRM_Kab. = mean(LRM_Cab),
                    LRM_Reg. = mean(LRM_HOG)) %>% 
    pivot_longer( cols =everything()) %>% 
    bind_cols(data.frame(lower = c(hdi(LRM_Opp)[1,1],
                                   hdi(LRM_Cab)[1,1],
                                   hdi(LRM_HOG)[1,1]))) %>% 
    bind_cols(data.frame(upper = c(hdi(LRM_Opp)[2,1],
                                   hdi(LRM_Cab)[2,1],
                                   hdi(LRM_HOG)[2,1])))
  LRMs = LRMs %>% 
    mutate(estimate =  paste(round(value, 2), " (", round(lower, 2), " \u2013 ", round(upper, 2), ")", sep=""))  %>%
    mutate(estimate = ifelse((lower < 0 &  upper < 0) | (lower > 0 &  upper > 0), paste("<b>", estimate, sep=""), estimate)) %>% 
    mutate(name = gsub("_", "<sub>", name)) %>% 
    select(Variable = name, !!modellabel := estimate) 
  
  obs = data.frame(name = c("N", "J", "T"), estimate = c(nrow(model$data), n_distinct(model$data$country_name), n_distinct(model$data$year)))  %>% 
    mutate(estimate = as.character(estimate)) %>% 
    select(Variable = name, !!modellabel := estimate) 
  
  table = table %>% 
    bind_rows(LRMs) %>% 
    bind_rows(obs)
  return(table)
}
print_table = function(modellist, credmass = 0.95) {
  modellabels = list("EV F", "EV G", "EV K",
                     "IV F", "IV G", "IV K",
                     "KO F", "KO G", "KO K",
                     "RG F", "RG G", "RG K",
                     "RS F", "RS G", "RS K")
  
  dust_1 = make_table(modellist[[1]], modellabels[[1]], credmass = credmass)
  for (i in 2:length(modellist)) {
    dust_app = make_table(modellist[[i]], modellabels[[i]], credmass = credmass)
    
    dust_1 = dust_1 %>% 
      left_join(dust_app, by="Variable")
  }
  
  dust_1 %>% 
    dust() %>% 
    sprinkle_na_string(na_string = "") %>% 
    
    sprinkle(rows = 1, border="bottom", part = "head") %>% 
    sprinkle(cols = 1, border="right", part = "body") %>% 
    sprinkle(cols = 2:16, halign = "center", part = "body") %>% 
    sprinkle_print_method(print_method = "html")
  
  
  
}

modellist = list(dec_f, dec_e, dec_c, int_f, int_e, int_c, com_f, com_e, com_c, rights_f, rights_e, rights_c, rs_f, rs_e, rs_c)
print_table(modellist, credmass = 0.95)
print_table(modellist, credmass = 0.9)

# Subset
modellist_sub = list(dec_f_sub, dec_e_sub, dec_c_sub, int_f_sub, int_e_sub, int_c_sub, com_f_sub, com_e_sub, com_c_sub, rights_f_sub, rights_e_sub, rights_c_sub, rs_f_sub, rs_e_sub, rs_c_sub)
print_table(modellist_sub, credmass = 0.95)
print_table(modellist_sub, credmass = 0.9)

