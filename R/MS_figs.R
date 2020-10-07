# TITLE: MS_figs
# DATE: 9-Mar-20
# AUTHOR: Jessica
#______________________________________________________________________________________________________________
# Load packages
#______________________________________________________________________________________________________________
library(tidyverse)
library(ggplot2)
library(ggalluvial)
library(viridis)
library(RColorBrewer)
library(ggpubr)

#______________________________________________________________________________________________________________
# Load cleaned data
#______________________________________________________________________________________________________________
df <- read.csv("Output/review_data_clean.csv")

#______________________________________________________________________________________________________________
# Food item figures
#______________________________________________________________________________________________________________
# Bar plot of study design 
ggplot(df, aes(Study.design.type)) +
  geom_bar()


# Plot stacked bar of food types
ggplot(df %>% count(Food.item), aes(1, n, fill=Food.item)) +
  geom_bar(stat="identity") +
  theme_minimal()

df %>% 
  group_by(Food.item) %>% 
  tally() %>%
  mutate(prop = n/nrow(df))


# Figure 2a: Stacked bars of food types at each poduction stage

# Reformat data to be long
df_long <- df %>%
  select(c(Food.item, Inputs, Production, Storage.Processing, Distribution.domestic, International.trade,
           Retail.Markets, Consumption)) %>% 
  mutate_all(as.character)


df_long <- df_long %>%  
  gather(key = "Stage", value = "Presence", Inputs:Consumption) %>%
  filter(Presence == "Yes") %>%
  group_by(Food.item, Stage) %>%
  tally()
df_long$Stage <- as.factor(df_long$Stage)

df_long$Stage <- factor(df_long$Stage, 
                                 levels = c("Inputs", "Production", 
                                            "Storage.Processing", 
                                            "Distribution.domestic", "International.trade",
                                            "Retail.Markets", "Consumption"))
df_long$Food.item <- factor(df_long$Food.item, levels =  c("Fruits & Vegetables", "Maize, Wheat & Rice", "Other cereals", 
                                                      "Legumes & nuts", "Roots and tubers", 
                                                      "Seafood", "Beef", "Poultry, Pork & Other meat", "Eggs", 
                                                      "Dairy", "Misc.", "Not specific"))

fig2a <- ggplot(df_long, aes(x =Stage, y = n, fill = Food.item)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.title = element_blank(),
        legend.text = element_text(size = 9)) +
  xlab("") +
  scale_x_discrete(labels = c("Inputs", "Production","Storage & Processing", 
                              "Domestic Distribution", "International Trade",
                              "Retail & Markets", "Consumption")) +
  ylab("Number of Studies") +
  scale_fill_brewer(palette = "RdYlBu")
fig2a

png("Output/fig2a.png", width = 5.5, height = 4, units = 'in', res = 500)
fig2a
dev.off()  

#______________________________________________________________________________________________________________
# Clean shock type data
#______________________________________________________________________________________________________________
df_shocks <- df %>% 
  # Separate out the different shocks listed (repeat rows for each shock)
  separate(Shock.type, into = c("Shock_1", "Shock_2", "Shock_3", "Shock_4", "Shock_5", "Shock_6", "Shock_7", "Shock_8", "Shock_9"), 
           sep = ", ") %>%
  gather(key = Shock.item.number, value = Shock.item, Shock_1:Shock_9) %>% 
  select(-c("Shock.item.number")) %>%
  filter(!is.na(Shock.item)) %>%
  filter(!(Shock.item %in% c("logistic", "", "No specific shock (a risk analysis)",  "Disaster risk index",
                             "n/a"))) %>% # Remove non-shock rows
  mutate(Shock.type = case_when(
    (Shock.item %in% c("Simulated shock", "Simulate a shock", "Simulated", "Simulated supply shock", 
                       "Reduced supply to international markets", "Drop in export quantity",
                       "Multiple", "Multiple shocks", 
                       "Multiple shocks, including climatic, geopolitical, logistic, social causes",
                       "No specific shock (a risk analysis)", "No specific shock", 
                       "Hypothetical", "seasonality")) ~ "Unspecified shock",
    
    (Shock.item %in% c("Household-reported climate related crisis", "Multiple weather variables",
                       "climate shock", "extreme climate events\n", "climate variability\n", 
                       "All climate shocks experienced by communities", "Climate", "Climate variability",
                       "climate disasters", "climate extremes", "climate variability", "including climatic",
                       "weather events", "extreme weather events", "weather anomalies (measured using NDVI)",
                       "Weather shocks", "Weather disturbances", "weather variability", "Weather variability",
                       "weather fluctuations", "weather", "weather fronts",  "extreme-weather events", 
                       "weather extremes",  "Extreme events", "extreme events", "extreme weather")) ~ "General climate/weather",
    
    (Shock.item %in% c("El Nino (flooding)", "El Nino", "El Nino/La Nina", "ENSO", "NAO", "WMO", "PDO", "IOSD",  
                       "AO",  "MEI", "EAM", "AMO", "SOI", "PSV", 
                       # Not sure about: PSV, WMO
                       "Quasi-biennial Oscillation")) ~ "ENSO & other oscillations",
    
    (Shock.item %in% c("Drought", "drought", "Simulated drought", "drouhts", "droughts", "SPEI", "PDSI",
                       "Variability in rainfall", "Rainfall variability",
                       "precipitation variability", "Precipitation variability",
                       "Three hypothetical shocks related to drought/excess precipitation", "extreme precipitation",
                       "precipitation shock", "heavy precipitation\n", "Precipitation", "precipitation anomalies",
                       "Flood (simulated impacts based on real event)", "Floods", "Flood", "flood", "floods", "Flooding",
                       "Heavy rainfall", "Monsoon onset delay", "Rainfall shocks",
                       "Poor rainfall", "Perceived adequecy of rain", "Rainfall", "high/low rainfall", "water scarcity", 
                       "heavy precipitation", "precipitation", "precipitation\n", "flooding", "Extended precipitation",
                       "precipitation variation", "water stress", "low rainfall", "soil moisture",
                       "dry spell", "rainfall", "dry days", "aridity index", "dry stress", "vapor pressure deficit", 
                       "humidity", "excessive moisture", "water logging",
                       "hail", "snow")) ~ "Extreme Precipitation",
    
    (Shock.item %in% c("Typhoon", "typhoon", "Hurricane", "hurricane", "cyclone", "two tropical cyclones",
                       "Rainfall shock (tropical storm)")) ~ "Tropical Storm",
    
    (Shock.item %in% c("Tsunami", "Earthquake", "Earthquake/tsunami")) ~ "Earthquake/Tsunami",
    
    (Shock.item %in% c("temperature", "extreme temperature", "Temperature", "high temperature", "heat shock",
                       "heat waves", "heat", "heat stress",  "extreme heat", "temperature variability",
                       "temparture variation",
                       "sea surface temperature", "sea surface temperatures", "water temperature",
                       "sea surface temperature anomalies", "sea surface temperature variability", "growing degree day",
                       "frost", "chilling injury", "cold", "extreme cold", "cold days", "freeze-thaw", 
                       "low temperature", "frost\n")) ~ "Extreme Temperature",
    
    (Shock.item %in% c("Simulated foot and mouth disease shock", "Fungal pathogen epidemic", "Fungal disease", 
                       "Disease", "disease", "diseases", "animal disease and epidemics", "pests", "weeds",
                       "Salmonella", "mycotoxin", "plant insect and pest infestation")) ~ "Disease & Pests",
    
    (Shock.item %in% c("Price spike", "Price Spike", "Price shock", "Rising rice price", "Price fluctuations regionally/globally",
                       "Maize price spike", "Rice price spike", "price variability", "market fluctuations")) ~ "Price",
    
    (Shock.item %in% c("geopolitical", "Export controls", "export bans", "social causes")) ~ "Geopolitical",
    
    (Shock.item %in% c("acidification", "eutrophication", "harmful algae blooms", "runoff", 
                       "winds", "wind", "strong winds", "wind speed",
                       "solar radiation", "radiation", "sunshine", "sunshine hours", "sunshine duration", 
                       "Volcanic eruption (Mount Bromo)",
                       "landslide", "sea level", "sea level rise")) ~ "Other" # Not sure what these acronyms are
  ))

df_shocks$Shock.type <- factor(df_shocks$Shock.type, levels =  c("Extreme Temperature", "Extreme Precipitation", "ENSO & other oscillations", "Tropical Storm",
                                                                 "General climate/weather", "Earthquake/Tsunami", "Disease & Pests" ,
                                                                 "Price", "Geopolitical", "Other", "Unspecified shock"))
# Figure 2: Heat map of shock types 
df_heatmap <- df_shocks %>% 
  group_by(Shock.type, Food.item) %>% 
  tally() %>%
  filter(!(Shock.type %in% c("Geopolitical", "Price")))

fig3 <- ggplot(data = df_heatmap, aes(Shock.type, 
                                   ordered(Food.item, levels = rev(levels(Food.item))), fill = n)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        panel.background = element_rect(fill = "white", colour = "grey50")) + 
  scale_fill_gradient(low = "white", high = "royalblue4") +
  xlab("") +
  ylab("") +
  labs(fill = "No. of \nstudies")
fig3

png("Output/fig3.png", width = 5, height = 4, units = 'in', res = 500)
fig3
dev.off()

#______________________________________________________________________________________________________________
# Summary stats for text
#______________________________________________________________________________________________________________
n.studies <- max(df$study.id)
# Proportion of studies stopping at production
(df %>% select(study.id, Stop_at_prod) %>% distinct() %>% filter(Stop_at_prod == "Y") %>% tally())/n.studies

# Proportion of studies including propagation
(df  %>% select(study.id, Propagation) %>% distinct() %>% filter(Propagation %in% c("Y", "Yes")) %>% tally())/n.studies

# Proportion of studies including production
(df %>% select(study.id, Production) %>% distinct() %>% filter(Production == "Yes") %>% tally())/n.studies

# Proportion of studies including consumption
(df %>% select(study.id, Consumption) %>% distinct() %>% filter(Consumption == "Yes") %>% tally())/n.studies

# Number of studies looking at multiple stages
sum(rowSums((df %>% select(study.id, Inputs, Production, Storage.Processing, Distribution.domestic, 
                           International.trade, Retail.Markets, Consumption) %>% distinct() %>% 
  select(Inputs, Production, Storage.Processing, Distribution.domestic, 
         International.trade, Retail.Markets, Consumption)) == "Yes") > 1) / n.studies

# Number of studies per spatial scale
df %>% 
  select(study.id, Study.scale) %>% 
  distinct() %>% 
  group_by(Study.scale) %>% 
  tally() %>%
  mutate(prop = n/nrow(df %>% select(study.id, Study.scale) %>% distinct()))

# Number of studies per region
df %>% 
  select(study.id, Study.region) %>% 
  distinct() %>% group_by(Study.region) %>% 
  tally() %>%
  mutate(prop = n/nrow(df %>% select(study.id, Study.region) %>% distinct()))

# Food item summary
df %>% 
  select(study.id, Food.item) %>% 
  distinct() %>%
  group_by(Food.item) %>% 
  tally() %>%
  mutate(prop = n/nrow(df %>% select(study.id, Food.item) %>% distinct()))

# Shock types summary
df_shocks %>% 
  group_by(Shock.type) %>% 
  tally() %>%
  mutate(prop = n/nrow(df_shocks))

#______________________________________________________________________________________________________________
# Shocks by supply chain stage (SI fig)
#______________________________________________________________________________________________________________
# Reformat data to be long
df_long <- df_shocks %>%
  select(c(Shock.type, Inputs, Production, Storage.Processing, Distribution.domestic, International.trade,
           Retail.Markets, Consumption)) %>%
  mutate_all(as.character)


df_long <- df_long %>%  
  gather(key = "Stage", value = "Presence", Inputs:Consumption) %>%
  filter(Presence == "Yes") %>%
  group_by(Shock.type, Stage) %>%
  tally() %>%
  filter(!(Shock.type %in% c("Geopolitical", "Price")))
df_long$Stage <- as.factor(df_long$Stage)

df_long$Stage <- factor(df_long$Stage, 
                        levels = c("Inputs", "Production", 
                                   "Storage.Processing", 
                                   "Distribution.domestic", "International.trade",
                                   "Retail.Markets", "Consumption"))

df_long$Shock.type <- factor(df_long$Shock.type, 
                        levels = c("Extreme Temperature", "Extreme Precipitation", "ENSO & other oscillations", "Tropical Storm",
                                   "General climate/weather", "Earthquake/Tsunami", "Disease & Pests" ,
                                   "Other", "Unspecified shock"))

fig2b <- ggplot(df_long, aes(x =Stage, y = n, fill = Shock.type)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.title = element_blank(),
        legend.text = element_text(size = 9)) +
  xlab("") +
  scale_x_discrete(labels = c("Inputs", "Production","Storage & Processing", 
                              "Domestic Distribution", "International Trade",
                              "Retail & Markets", "Consumption")) +
  ylab("Output/Number of Studies") +
  scale_fill_brewer(palette = "RdYlBu")

fig2b

png("Output/fig2b.png", width = 5.5, height = 4, units = 'in', res = 500)
fig2b
dev.off()  

png("Output/fig2.png", width = 5.5, height = 8, units = 'in', res = 500)
ggarrange(fig2a, fig2b, ncol = 1, labels = c("a", "b"))
dev.off()
