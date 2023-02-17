# Insect lab R & Vegan tutorial


# Load everything
library(vegan)
library(ggplot2)
library(tidyr)
data <- read.csv("Sample_Data.csv")
print(data)





# Shannon + Pielou Evenness

# Use the pivot_wider function to make the data more accessible/clean
family_data <- pivot_wider(data, Family, names_from = Collection, values_from = Collection, values_fn = length, values_fill = 0)
print(family_data)

# Rocky location Shannon and Evenness
R_Shannon <- diversity(family_data$ROCKY)
print(R_Shannon)
R_Pielou <- R_Shannon/log(specnumber(family_data$ROCKY)) 
print(R_Pielou)

#Orchard Location
O_Shannon <- diversity(family_data$ORCHARD)
print(O_Shannon)
O_Pielou <- O_Shannon/log(specnumber(family_data$ORCHARD)) 
print(O_Pielou)
ORCHARD_Val <- c(O_Shannon, O_Pielou)
print(ORCHARD_Val)


# Farm Location
F_Shannon <- diversity(family_data$FARM)
print(F_Shannon)
F_Pielou <- F_Shannon/log(specnumber(family_data$FARM)) 
print(F_Pielou)



#Put values into a matrix bc pretty
SP_Values <- data.frame(Index = c("Shannon's", "Pielou's"),
                        Rocky = c(R_Shannon, R_Pielou),
                        Orchard = c(O_Shannon, O_Pielou),
                        Farm = c(F_Shannon, F_Pielou))
print(SP_Values)







# Jaccard's Index

# do same as before to visualize data
order_data <- pivot_wider(data, Order, names_from = Collection, values_from = Collection, values_fn = length, values_fill = 0)
print(order_data)

#create data frame bc pivot_wider doesn't make the data usable for some reason
order_data <- data.frame(Rocky = order_data$ROCKY,
                         Orchard = order_data$ORCHARD,
                         Farm = order_data$FARM)
print(order_data)

# Use vegidst to calculate index
vegdist(vals, method = "jaccard", binary = T)



# Plot Heights insects were found at (grouped by order)
# Combo Boc and dot plot for insect heights
ggplot(data, aes(x=Order, y=Height)) +
  geom_boxplot()+ 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5)+
  labs(title="Height at which Insects were Caught", 
       x = "Order",
       y = "Hieght (cm)") +
  theme_bw()
