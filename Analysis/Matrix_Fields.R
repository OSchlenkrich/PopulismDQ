source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")

# Plot Function
Create_15_Matrix_Populist_Plots = function(Sel_Country, Begin_Gov, End_Gov, top_title = "", min_value = T) {
  # Year: Minimum: Total Value Index
  Minimum = DMX_context %>% 
    filter(country_name == Sel_Country) %>%
    filter(year >= Begin_Gov, year <= End_Gov) %>% 
    select(-classification_context, -regions) %>% 
    arrange(total_index_context) %>% 
    slice(1) %>% 
    melt(id.vars=c("country_name", "year"), value.name="value_min") 
  
  title_middle = "Differenz zu Minimum"
  
  if (min_value == F) {
    Minimum = DMX_context %>% 
      filter(country_name == Sel_Country) %>%
      filter(year == End_Gov) %>% 
      select(-classification_context, -regions) %>% 
      melt(id.vars=c("country_name", "year"), value.name="value_min") 
    title_middle = "Differenz"
  }

  plot_data = DMX_context %>% 
    filter(country_name == Sel_Country) %>% 
    filter(year == Begin_Gov) %>% 
    select(-classification_context, -regions) %>% 
    melt(id.vars=c("country_name", "year")) %>% 
    left_join(Minimum, by=c("country_name", "variable")) %>% 
    mutate(difference = value_min - value) %>% 
    group_by(country_name, variable) %>% 
    summarise(change = sum(difference, na.rm=T)) %>%  
    mutate(year = End_Gov) %>% 
    spread(variable, change) 
  
  yaxis <- factor(c("Dimension", "Rules\n settlement", "Rights", "Communication", "Intermediate\n Sphere", "Procedurs\n of \nDecision"), levels=c("Procedurs\n of \nDecision", "Intermediate\n Sphere", "Communication", "Rights", "Rules\n settlement", "Dimension"))
  
  p1 = vis_15_Felder(Sel_Country, Begin_Gov, "Context_Measurement", DMX_context)
  p2 = vis_15_Felder(Sel_Country, End_Gov, "Difference", plot_data) +
    theme(axis.text.y = element_blank()) +
    ggtitle(paste(title_middle, unique(Minimum$year)))
  p3 = vis_15_Felder(Sel_Country, End_Gov, "Context_Measurement", DMX_context) +
    scale_y_discrete(limits=yaxis, expand = c(0, 0), position="right")
  
  return(grid.arrange(p1,p2,p3, ncol=3,
         top = textGrob(top_title,gp=gpar(fontsize=16,font=3)))
         )

}


# Select Country, Begin of Populist Government and End of Populist Government
Create_15_Matrix_Populist_Plots("Bolivia", 2006, 2017, "Bolivia 2006 - \n Evo Morales")
Create_15_Matrix_Populist_Plots("Peru", 1990, 2000, "Peru 1990-2000 \n Alberto Fujimori")
Create_15_Matrix_Populist_Plots("Ecuador", 2007, 2016,  "Ecuador 2007-2016 \n Rafael Correa")
Create_15_Matrix_Populist_Plots("Turkey", 2002, 2017,  "Turkey 2002 -  \n Recep Erdoğan (AKP)")
Create_15_Matrix_Populist_Plots("Hungary", 2010, 2017,  "Hungary 2010 -  \n Viktor Orbán (Fidesz)")
Create_15_Matrix_Populist_Plots("Nicaragua", 2007, 2017, "Nicaragua 2007 - \nDaniel Ortega")
Create_15_Matrix_Populist_Plots("Venezuela", 1999, 2013, "Venezuela 1999 - 2013 \n Hugo Chávez")
Create_15_Matrix_Populist_Plots("Slovakia", 1994, 1999, "Slovakia 1994-1998 \n Vladimír Mečiar", min_value=F) #Vladimír Mečiar 1994-1998; Ministerpräsident
Create_15_Matrix_Populist_Plots("Poland", 2015, 2017, "Poland 2015- \n PiS")
Create_15_Matrix_Populist_Plots("Italy", 2001, 2011, "Italy 2001-2011 \n Silvio Berlusconi")
Create_15_Matrix_Populist_Plots("India", 2014, 2017, "India 2014- \n Narendra Modi")

Create_15_Matrix_Populist_Plots("Greece", 1981, 1989, "Greece 1981-1989 \nAndreas Papandreou", min_value=F)
Create_15_Matrix_Populist_Plots("Philippines", 2016, 2017)




