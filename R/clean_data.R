# TITLE: Data cleaning
# DATE: 9-Mar-20
# AUTHOR: Jessica
#______________________________________________________________________________________________________________
# Load packages
#______________________________________________________________________________________________________________
library(tidyverse)

#______________________________________________________________________________________________________________
# Load data
#______________________________________________________________________________________________________________
df_raw <- read.csv("Data/Review26Jan20.csv", header = TRUE)
df_prod_raw <- read.csv("Data/StopsAtProd.csv", header = TRUE)

#______________________________________________________________________________________________________________
# Clean and filter data
#______________________________________________________________________________________________________________
# Remove eliminated studies and make each food type a separate observation
df <- df_raw %>% 
  filter(!(Eliminate %in% c("Y", "Yes"))) %>% 
  # Add study number
  mutate(study.id = row_number()) %>%
  # Separate out the different foods listed (repeat rows for each food)
  separate(Food.item, into = c("Food_1", "Food_2", "Food_3", "Food_4", "Food_5", "Food_6", "Food_7", "Food_8", "Food_9", 
                               "Food_10", "Food_11", "Food_12", "Food_13", "Food_14", "Food_15", "Food_16", "Food_17"),
           sep = ", ") %>%
  gather(key = Food.item.number, value = Food.item, Food_1:Food_17) %>% 
  select(-c("Food.item.number")) %>%
  filter(!is.na(Food.item)) %>%
  filter(!(Food.item %in% c("Wool", "cotton", "Cotton"))) %>% # Drop woll and cotton (not foods)
  
  # Standardize food item names
  mutate(Food.item = case_when(
    # Not specific foods
    (Food.item %in% c("All", "All food", "Whole diet", "All (6 food groups)", "n/a", "na", "", "-", "No specific", 
                      "Agriculture", "agriculture", "Crops", "crops", "Food", "major food and cash crops", 
                      "arable farming systems", "NPP", "NDVI", "other crops", "forage crops", "grassland",
                      "dryland crops", "grain", "grains")) ~ "Not specific",
    
    (Food.item %in% c("Maize", "maize", "corn")) ~ "Maize",
    
    (Food.item %in% c("Wheat", "wheat", "winter wheat", "Winter wheat", "Wheat bread", "spring wheat")) ~ "Wheat",
    
    (Food.item %in% c("Rice", "rice", "Japanese rice")) ~ "Rice",
    
    (Food.item %in% c("Soybeans", "soybeans", "soybean", "soy")) ~ "Soybeans",
    
    (Food.item %in% c("Millet", "millet", "pearl millet", "finger millet", "Barley", "barley", "spring barley", 
                      "winter barley", "sorghum", "teff", "mixed grain",  "oats", "oat", "rye", "four major staples", 
                      "spring and winter cereals", "cereals", "Cereals", "cereal", "grains")) ~ "Other cereals",
    
    (Food.item %in% c("potatoes", "potato", "Potato", "yam", "yams", "cocoyam", "cassava")) ~ "Roots and tubers",
    
    (Food.item %in% c("vegetables", "eggplant", "brinjals", "tomato", "tomatoes", "Tomatoes", "Lettuce", 
                      "mustard", "cabbages", "beet", "chillies", "creepers", "mixed vegetables")) ~ "Vegetables",
    
    (Food.item %in% c("Banana", "Bananas", "matooke", "fruits", "Grapes", "citrus fruit", "pineapples")) ~ "Fruits",
    
    (Food.item %in% c("Cattle", "Beef", "beef cattle", "bovine")) ~ "Beef",
    
    (Food.item %in% c("poultry")) ~ "Poultry",
    
    (Food.item %in% c("swine", "pig")) ~ "Pork",
    
    (Food.item %in% c("sheep", "Water buffalo", "lamb", "livestock", "Livestock")) ~ "Other meat",
    
    (Food.item %in% c("Eggs", "eggs")) ~ "Eggs",
    
    (Food.item %in% c("Seafood", "fish", "Fish", "finfish", "fish stock", "Fisheries", "artisinal fisheries",
                      "pond-based aquaculture", "Aquaculture",
                      "Salmon", "grouper", "swordfish", "bluefin tuna", "Barramundi", "snapper", "albacore", "Tuna",
                      "sardinella", "sardines", "Sardines", "anchovy", "Anchovy",
                      "oysters", "Clams",  "Scallops", "Shrimp", "shrimp", "Lobster", "prawns", "fishmeal")) ~ "Seafood",
    
    (Food.item %in% c("beans", "Beans", "bean", "pulses", "Pulses", "peanuts", "Groundnut", "groundnut", "groundnuts", "cashews", 
                      "areca nut", "cowpea", "pigeonpea", "lentil", "common vetch", "bitter vetch", "chickpea", 
                      "lupine", "broad beans")) ~ "Other pulses, legumes and nuts",
    
    (Food.item %in% c("Dairy", "dairy", "cheese", "butter", "milk", "Milk", 
                      "Dairy cattle")) ~ "Milk and milk products",
    
    (Food.item %in% c("oils", "oilseeds", "canola", "Canola", "winter rapeseed", "rape", "rapeseed",
                      "palm trees")) ~ "Oil and fats",
    
    (Food.item %in% c("sugarcane", "sugar", "Sugar beet", "sugar beet")) ~ "Sugar/honey",
    
    (Food.item %in% c("Beer", "chat", "hops", "coffee", "wine", "Wine", "tea", "Tea", "dates", "cocoa", "coconut", 
                      "coconuts", "Olives", "olives", "black pepper", "cardamom")) ~ "Misc."
  )) 

df <- df %>%
  mutate(Food.item = case_when(
    (Food.item %in% c("Fruits", "Vegetables")) ~ "Fruits & Vegetables",
    (Food.item %in% c("Maize", "Wheat", "Rice")) ~ "Maize, Wheat & Rice",
    (Food.item %in% c("Other cereals")) ~ "Other cereals",
    (Food.item %in% c("Soybeans", "Other pulses, legumes and nuts")) ~ "Legumes & nuts",
    (Food.item %in% c("Roots and tubers")) ~ "Roots and tubers",
    (Food.item %in% c("Seafood")) ~ "Seafood",
    (Food.item %in% c("Beef")) ~ "Beef",
    (Food.item %in% c("Poultry", "Pork", "Other meat")) ~ "Poultry, Pork & Other meat",
    (Food.item %in% c("Eggs")) ~ "Eggs",
    (Food.item %in% c("Milk and milk products")) ~ "Dairy",
    (Food.item %in% c("Oil and fats", "Sugar/honey", "Misc.")) ~ "Misc.",
    (Food.item %in% c("Not specific")) ~ "Not specific"
  ))

# Filter studies stopping at production and select only food and shock type columns
df_prod <- df_prod_raw %>%
  filter(Stop.at.yield..productivity...production...Y.N. %in% c("Y", "y", "Y (but nutrient concentration)")) %>%
  # Add study number
  mutate(study.id = max(df$study.id) + row_number()) %>%
  select(publication.title, publication.journal, publication.doi, study.id, Food.item = "If.stop..which.food.items.", Shock.type = "If.stop..what.type.of.shock.", 
         Study.location = "If.stop..what.scale.location.", Study.scale)

# Make each food type a separate observation
df_prod <- df_prod %>% 
  # Separate out the different foods listed (repeat rows for each food)
  separate(Food.item, into = c("Food_1", "Food_2", "Food_3", "Food_4", "Food_5", "Food_6", "Food_7", "Food_8", "Food_9", 
                               "Food_10", "Food_11", "Food_12", "Food_13", "Food_14", "Food_15", "Food_16", "Food_17"),
           sep = ", ") %>%
  gather(key = Food.item.number, value = Food.item, Food_1:Food_17) %>% 
  select(-c("Food.item.number")) %>%
  filter(!is.na(Food.item)) %>%
  filter(!(Food.item %in% c("Wool", "cotton", "Cotton"))) %>% # Drop woll and cotton (not foods)
  
  # Standardize food item names
  mutate(Food.item = case_when(
    # Not specific foods
    (Food.item %in% c("All", "All food", "Whole diet", "All (6 food groups)", "n/a", "na", "", "-", "No specific", 
                      "Agriculture", "agriculture", "Crops", "crops", "Food", "major food and cash crops", 
                      "arable farming systems", "NPP", "NDVI", "other crops", "forage crops", "grassland",
                      "dryland crops", "grain", "grains")) ~ "Not specific",
    
    (Food.item %in% c("Maize", "maize", "corn")) ~ "Maize",
    
    (Food.item %in% c("Wheat", "wheat", "winter wheat", "Winter wheat", "Wheat bread", "spring wheat")) ~ "Wheat",
    
    (Food.item %in% c("Rice", "rice", "Japanese rice")) ~ "Rice",
    
    (Food.item %in% c("Soybeans", "soybeans", "soybean", "soy")) ~ "Soybeans",
    
    (Food.item %in% c("Millet", "millet", "pearl millet", "finger millet", "Barley", "barley", "spring barley", 
                      "winter barley", "sorghum", "teff", "mixed grain",  "oats", "oat", "rye", "four major staples", 
                      "spring and winter cereals", "cereals", "Cereals", "cereal", "grains")) ~ "Other cereals",
    
    (Food.item %in% c("potatoes", "potato", "Potato", "yam", "yams", "cocoyam", "cassava")) ~ "Roots and tubers",
    
    (Food.item %in% c("vegetables", "eggplant", "brinjals", "tomato", "tomatoes", "Tomatoes", "Lettuce", 
                      "mustard", "cabbages", "beet", "chillies", "creepers", "mixed vegetables")) ~ "Vegetables",
    
    (Food.item %in% c("Banana", "Bananas", "matooke", "fruits", "Grapes", "citrus fruit", "pineapples")) ~ "Fruits",
    
    (Food.item %in% c("Cattle", "Beef", "beef cattle", "bovine")) ~ "Beef",
    
    (Food.item %in% c("poultry")) ~ "Poultry",
    
    (Food.item %in% c("swine", "pig")) ~ "Pork",
    
    (Food.item %in% c("sheep", "Water buffalo", "lamb", "livestock", "Livestock")) ~ "Other meat",
    
    (Food.item %in% c("Eggs", "eggs")) ~ "Eggs",
    
    (Food.item %in% c("Seafood", "fish", "Fish", "finfish", "fish stock", "Fisheries", "artisinal fisheries",
                      "pond-based aquaculture", "Aquaculture",
                      "Salmon", "grouper", "swordfish", "bluefin tuna", "Barramundi", "snapper", "albacore", "Tuna",
                      "sardinella", "sardines", "Sardines", "anchovy", "Anchovy",
                      "oysters", "Clams",  "Scallops", "Shrimp", "shrimp", "Lobster", "prawns", "fishmeal")) ~ "Seafood",
    
    (Food.item %in% c("beans", "Beans", "bean", "pulses", "Pulses", "peanuts", "Groundnut", "groundnut", "groundnuts", "cashews", 
                      "areca nut", "cowpea", "pigeonpea", "lentil", "common vetch", "bitter vetch", "chickpea", 
                      "lupine", "broad beans")) ~ "Other pulses, legumes and nuts",
    
    (Food.item %in% c("Dairy", "dairy", "cheese", "butter", "milk", "Milk", 
                      "Dairy cattle")) ~ "Milk and milk products",
    
    (Food.item %in% c("oils", "oilseeds", "canola", "Canola", "winter rapeseed", "rape", "rapeseed",
                      "palm trees")) ~ "Oil and fats",
    
    (Food.item %in% c("sugarcane", "sugar", "Sugar beet", "sugar beet")) ~ "Sugar/honey",
    
    (Food.item %in% c("Beer", "chat", "hops", "coffee", "wine", "Wine", "tea", "Tea", "dates", "cocoa", "coconut", 
                      "coconuts", "Olives", "olives", "black pepper", "cardamom")) ~ "Misc."
  )) 

df_prod <- df_prod %>%
  mutate(Food.item = case_when(
    (Food.item %in% c("Fruits", "Vegetables")) ~ "Fruits & Vegetables",
    (Food.item %in% c("Maize", "Wheat", "Rice")) ~ "Maize, Wheat & Rice",
    (Food.item %in% c("Other cereals")) ~ "Other cereals",
    (Food.item %in% c("Soybeans", "Other pulses, legumes and nuts")) ~ "Legumes & nuts",
    (Food.item %in% c("Roots and tubers")) ~ "Roots and tubers",
    (Food.item %in% c("Seafood")) ~ "Seafood",
    (Food.item %in% c("Beef")) ~ "Beef",
    (Food.item %in% c("Poultry", "Pork", "Other meat")) ~ "Poultry, Pork & Other meat",
    (Food.item %in% c("Eggs")) ~ "Eggs",
    (Food.item %in% c("Milk and milk products")) ~ "Dairy",
    (Food.item %in% c("Oil and fats", "Sugar/honey", "Misc.")) ~ "Misc.",
    (Food.item %in% c("Not specific")) ~ "Not specific"
  ))

# Combine df and df_prod
df_prod$Stop_at_prod <- factor("Y", levels = c("Y", "N"))
df_prod$Inputs <- factor("No", levels = levels(df$Inputs))
df_prod$Production <- factor("Yes", levels = levels(df$Production))
df_prod$Storage.Processing <- factor("No", levels = levels(df$Storage.Processing))
df_prod$Distribution.domestic <- factor("No", levels = levels(df$Distribution.domestic))
df_prod$International.trade <- factor("No", levels = levels(df$International.trade))
df_prod$Retail.Markets <- factor("No", levels = levels(df$Retail.Markets))
df_prod$Consumption <- factor("No", levels = levels(df$Consumption))
df_prod$Price <- factor("No", levels = levels(df$Price))

df$Stop_at_prod <- factor("N", levels = c("Y", "N"))

df <- df %>% 
  bind_rows(df_prod) %>%
  filter(!is.na(Food.item))

# Set food item levels
df$Food.item <- factor(df$Food.item, levels =  c("Fruits & Vegetables", "Maize, Wheat & Rice", "Other cereals", 
                                                 "Legumes & nuts", "Roots and tubers", 
                                                 "Seafood", "Beef", "Poultry, Pork & Other meat", "Eggs", 
                                                 "Dairy", "Misc.", "Not specific"))

# Clean study scale column
df <- df %>%
  mutate(Study.scale = case_when(
    (Study.scale %in% c("Household", "sub-national", "Sub-national", "local (Marsabit district)",
                        "region affected by the tsunami", "Sub-national in two countries", "City", 
                        "city", "Farm", "Focus groups", "State", "Local markets")) ~ "Sub-national",
    (Study.scale %in% c("National", "national", "National impacts using a global model",
                        "National (data from 419 villages throughout)")) ~ "National", 
    (Study.scale %in% c("International", "Global", "sub-national to global", "Regional", "world regions")) ~ "International" 
  ))

# Clean study location column
df <- df %>%
  mutate(Study.region = case_when(
    (Study.location %in% c("Global", "global", "Chile, Norway, US, Brazil", "Thailand, United States", "Ethiopia and Bangladesh",
                           "15 countries across C America, W Africa, E Africa, and S Asia. 1-10 sites selected in each country, with 140 households/site", 
                           "Argentina, Australia, Canada, Europe, United States", "USA; Russia; eastern Africa; southern Africa; Australia",
                           "France (plus exports from Ecuador, Colombia, Costa Rica, Guadeloupe, Ivory Coast and Cameroon)", 
                           "Argentina (model initially validated) USA, Italy, Japan and South Africa (model used to examine propagation)",
                           "147 countries", "51 countries", "Guatemala, Nicaragua, Panama, Haiti, Ecuador, Peru", "Indian Ocean",
                           "Gibraltar Strait; western Mediterranean", "Mediterranean Basin", "Mediterranean", "Argentina; Brazil; USA",
                           "USA, India, China, Argentina, Australia", "southeastern Pacific", "northwestern Mediterranean",
                           "USA, Argentina, Brazil, China", "northeast Pacific")) ~ "Multi-regional",
    
    (Study.location %in% c("Africa", "Burkina Faso", "northern region of Burkina Faso", "Ethiopia", "Niger, Ethiopia and Somalia", 
                           "Jimma, Ethiopia", "Cote D'Ivoire", "Tanzania", "Malawi", "northern Togo", "Botswana",
                           "Uganda", "Kenya", "Mozambique", "4 districts in Limpopo, Zambezi and Save rivers areas, Mozambique",
                           "4 districts in Limpopo, Zambezi and Save rivers areas, Mozambique", "French Guiana",
                           "Three communities in the African Plains region of Ghana", "Ethiopia and Ghana", "Ghana",
                           "Mpumalanga Province, South Africa", "Niger", "Zambia", "Zimbabwe", "Timor Leste", "Nigeria",
                           "Southern Africa (9 countries)", "South Africa", "Ibadan, Nigeria", "Tunisia", "Togo", 
                           "Senegal", "northern Ethiopia", "sub-Saharan Africa", "Sudan", "Mount Kenya, Kenya",
                           "Uganda; Kenya", "southwestern Nigeria", "Morocco", "Cameroon", "eastern Ivory Coast",
                           "Zambezi River Basin", "Mauritius", "southern Mali")) ~ "Africa",
    
    (Study.location %in% c("Yemen", "Afghanistan", "Turkey", "west and northwest Iran", "northeast Iran", "Lebanon") ~ "Middle East"),
    
    (Study.location %in% c("Japan", "Ishinomaki City, Japan", "China", "Sichuan, China", "Yellow river region, China", 
                           "Indonesia", "Taiwan", "Jagatsinghpur district, Odisha India", "North China Plain", "northeast China",
                           "Earth-quake affected districts (Slukhumba, Rasuwa, Sindhupalchok, Arghakhanchi, Lamjung, Kathmandu, Ramechap), Nepal",
                           "Three scenarios: 1. epidemic in Thailand only; 2. most countries in SEA (Thailand, Myanmar, Laos, Cambodia, Vietnam, Indonesia, Malaysia); 3. it also reduces yields in China",
                           "Vietnam", "Malaysia", "Southern Punjab, Pakistan", "Philippines", "Bangladesh", "Mongolia", 
                           "Quang Nam province, Vietnam", "eastern China", "central India; western India", "southwest China",
                           "Varanasi, India", "India", "Southeast Asia", "Cambodia", "Sri Lanka", "Assam, India", "east Java",
                           "northeast Thailand", "western Himalaya, India", "Nepal", "Rajasthan, India", "Shanxi province, China",
                           "northwest Bangladesh", "Visakhapatnam, India", "Hunan province, China", "central Taiwan",
                           "northwest India", "Sangzhi county, China", "Mekong Delta, Vietnam", "northwest China", 
                           "northern South China Sea", "Bihar, India", "Yuhang county, China", "Andhra Pradesh, India",
                           "Heilongjiang province, China", "Shaoxing region, China", "Ganges Basin", "China; Japan",
                           "Uttar Pradesh, India", "Kerala, India", "northern China")) ~ "Asia",
    
    (Study.location %in% c("Europe", "Brittany region of France", "Russia, Ukraine and Khazakstan", "Russia and Ukraine", "Russia; Kazakhstan",
                           "Spain", "Netherlands", "Scotland", "southern Italy", "Greece", "northern Europe", "Germany", "France",
                           "Italy", "Czech Republic", "Iberian Penisula", "Douro region, Portugal", "Flemish region, Belgium",
                           "Belgium", "northwestern Europe", "UK", "Rheinland-Pfalz, Germany", "Catalonia, Spain", 
                           "Catalan Coast, Spain", "Hungary", "Ukraine", "Flevoland, Netherlands", "Latvia", 
                           "Minho region, Portugal", "northwest Europe")) ~ "Europe",
    
    (Study.location %in% c("US", "USA", "United States", "New Orleans", "Mexico", "Honduras", "Guatemala", "US Gulf of Mexico", 
                           "Delaware Bay, USA", "USA; Canada", "central USA", "southwestern USA", "Kansas, USA", "Hawaii, USA",
                           "Alberta, Canada", "western USA", "Corn Belt, US", "Corn Belt, USA", "Manitoba, Canada", "southeast USA",
                           "Mississippi River, USA", "Midwest, USA", "Midwest USA", "Upper Lerma, Mexico", "Texas, USA",
                           "Costa Rica", "southwest Quebec, Canada", "Illinois, USA", "central Mexico", "Great Plains, USA")) ~ "North America",
    
    (Study.location %in% c("Northern Peru (Tumbes)", "Peru", "Latin America", "Mato Grosso, Brazil", "Lower Amazon Region",
                           "Pampas, Argentina", "Colombia", "Peru; Chile", "northeast Brazil", "Sao Paolo, Brazil",
                           "Brazil")) ~ "South America",
    
    (Study.location %in% c("New South Wales, Australia", "Australia", "Queensland, Australia", "southeast Australia")) ~ "Oceania",
    
    (Study.location %in% c("n/a", "N/A", "")) ~ "Other"
  ))

#______________________________________________________________________________________________________________
# Write out data files
#______________________________________________________________________________________________________________
write.csv(df, "Output/review_data_clean.csv", row.names = FALSE)

# Write out file with review references
refs <- df %>%
  select(publication.title, publication.journal, publication.doi) %>%
  distinct()
write.csv(refs, "Output/review_references.csv", row.names = FALSE)

