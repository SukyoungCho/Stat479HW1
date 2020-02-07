library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)

## Read the data
df17 = read_csv("2017hwybronlyonefile.zip")
df18 = read_csv("2018hwybronefiledel.zip")

# Seems like there is no perfect ID for bridges, but Structure_Number_008 seems to be pretty unique
#df17 %>% group_by(STRUCTURE_NUMBER_008) %>% summarize(number = n()) %>% filter(number>1)
#df18 %>% group_by(STRUCTURE_NUMBER_008) %>% summarize(number = n()) %>% filter(number>1)

## Merge two data into one data table
bridges = rbind(df17,df18)

## One line code that process the data and draw the plot

bridges %>% transmute(bridgeID = paste(RECORD_TYPE_005A,ROUTE_PREFIX_005B,SERVICE_LEVEL_005C,ROUTE_NUMBER_005D,DIRECTION_005E,  sep =""),
                      FIPS = paste(STATE_CODE_001, COUNTY_CODE_003, sep=""),
                      Deck=DECK_COND_058,
                      Super.S=SUPERSTRUCTURE_COND_059,
                      Sub.S=SUBSTRUCTURE_COND_060,
                      Channel=CHANNEL_COND_061,
                      ADT = ADT_029,
                      Total_Imp_Cost=TOTAL_IMP_COST_096,
                      ADT.Truck=PERCENT_ADT_TRUCK_109,
                      National = NATIONAL_NETWORK_110) %>%
  filter(Super.S != "N" & Deck != "N" & Sub.S != "N" & Channel != "N") %>% 
  mutate(Overall.Cond = (as.integer(Deck) + as.integer(Super.S) + as.integer(Sub.S) + as.integer(Channel)) / 4) %>% 
  na.omit(bridges) %>% 
  ggplot(mapping = aes(x = ADT.Truck, y = Overall.Cond)) +
  geom_point(aes(size = ADT), alpha = 1/3) +
  geom_point(aes(color = National)) +
  geom_smooth(se = FALSE)




# Conditions used
#   "DECK_COND_058"           "SUPERSTRUCTURE_COND_059" "SUBSTRUCTURE_COND_060"  "CHANNEL_COND_061" 

# Cols of possible interests 
#"DECK_STRUCTURE_TYPE_107" |   "SURFACE_TYPE_108A"  |   "MEMBRANE_TYPE_108B"
#"DECK_PROTECTION_108C"  |   "NATIONAL_NETWORK_110" 
#"ADT_029",  "TOTAL_IMP_COST_096"  |  "PERCENT_ADT_TRUCK_109"


##### A code to see how many bridges are National and not


# bridges %>% transmute(bridgeID = paste(RECORD_TYPE_005A,ROUTE_PREFIX_005B,SERVICE_LEVEL_005C,ROUTE_NUMBER_005D,DIRECTION_005E,  sep =""),
#                       FIPS = paste(STATE_CODE_001, COUNTY_CODE_003, sep=""),
#                       Deck=DECK_COND_058,
#                       Super.S=SUPERSTRUCTURE_COND_059,
#                       Sub.S=SUBSTRUCTURE_COND_060,
#                       Channel=CHANNEL_COND_061,
#                       ADT = ADT_029,
#                       Total_Imp_Cost=TOTAL_IMP_COST_096,
#                       ADT.Truck=PERCENT_ADT_TRUCK_109,
#                       National = NATIONAL_NETWORK_110) %>%
#   filter(Super.S != "N" & Deck != "N" & Sub.S != "N" & Channel != "N") %>% 
#   mutate(Overall.Cond = (as.integer(Deck) + as.integer(Super.S) + as.integer(Sub.S) + as.integer(Channel)) / 4) %>% 
#   na.omit(bridges) %>% group_by(National) %>% summarise(count = n())