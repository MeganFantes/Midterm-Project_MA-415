library(stringr)
library(reshape2)
library(plyr)

# read the csv file, import it into the environment
vehicles <- read.csv("Vehicles.csv")

# There are many variables that are given for both Fuel Type 1 and Fuel Type 2.
# We want to melt all variables given for both fuel types, so that there is only 1 colvar
# indicating fuel type, and one colvar indicating the specific variable for each fuel type

# melt annual petroleum consumption in barrels for each fuel type
to_melt <- vehicles[c("id", "barrels08", "barrelsA08")]
to_melt <- melt(to_melt, id = "id", na.rm = FALSE)
names(to_melt) <- c("id", "Fuel_Type", "Annual_Petrol_Consumption")
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "barrels08", 1)
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "barrelsA08", 2)
Fuel_Type_Properties <- to_melt

Fuel_Type_Properties <- arrange(Fuel_Type_Properties, id, Fuel_Type)

# melt city MPG for each fuel type
to_melt <- vehicles[c("id", "city08", "cityA08")]
to_melt <- melt(to_melt, id = "id", na.rm = FALSE)
names(to_melt) <- c("id", "Fuel_Type", "City_MPG")
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "city08", 1)
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "cityA08", 2)
Fuel_Type_Properties <- join(Fuel_Type_Properties, to_melt, by = c("id", "Fuel_Type"))

# melt tailpipe CO2 for each fuel type
to_melt <- vehicles[c("id", "co2TailpipeGpm", "co2TailpipeAGpm")]
to_melt <- melt(to_melt, id = "id", na.rm = FALSE)
names(to_melt) <- c("id", "Fuel_Type", "Tailpipe_CO2")
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "co2TailpipeGpm", 1)
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "co2TailpipeAGpm", 2)
Fuel_Type_Properties <- join(Fuel_Type_Properties, to_melt, by = c("id", "Fuel_Type"))

# melt combined MPG for each fuel type
to_melt <- vehicles[c("id", "comb08", "combA08")]
to_melt <- melt(to_melt, id = "id", na.rm = FALSE)
names(to_melt) <- c("id", "Fuel_Type", "Combined_MPG")
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "comb08", 1)
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "combA08", 2)
Fuel_Type_Properties <- join(Fuel_Type_Properties, to_melt, by = c("id", "Fuel_Type"))

# melt annual fuel cost for each fuel type
to_melt <- vehicles[c("id", "fuelCost08", "fuelCostA08")]
to_melt <- melt(to_melt, id = "id", na.rm = FALSE)
names(to_melt) <- c("id", "Fuel_Type", "Fuel_Cost")
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "fuelCost08", 1)
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "fuelCostA08", 2)
Fuel_Type_Properties <- join(Fuel_Type_Properties, to_melt, by = c("id", "Fuel_Type"))

# melt highway MPG for each fuel type
to_melt <- vehicles[c("id", "highway08", "highwayA08")]
to_melt <- melt(to_melt, id = "id", na.rm = FALSE)
names(to_melt) <- c("id", "Fuel_Type", "Highway_MPG")
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "highway08", 1)
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "highwayA08", 2)
Fuel_Type_Properties <- join(Fuel_Type_Properties, to_melt, by = c("id", "Fuel_Type"))

# melt string values for fuel type
to_melt <- vehicles[c("id", "fuelType1", "fuelType2")]
to_melt$fuelType1 <- as.character(to_melt$fuelType1) # convert to character vector for melting
to_melt$fuelType2 <- as.character(to_melt$fuelType2) # convert to character vector for melting
to_melt <- melt(to_melt, id = "id", na.rm = FALSE)
names(to_melt) <- c("id", "Fuel_Type", "Fuel_Type_Name")
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "fuelType2", 2)
to_melt$Fuel_Type <- str_replace(to_melt$Fuel_Type, "fuelType1", 1)
Fuel_Type_Properties <- join(Fuel_Type_Properties, to_melt, by = c("id", "Fuel_Type"))

# rearrange Fuel_Type_Properties data table so that Fuel_Type and Fuel_Type Name are next to each other
names(Fuel_Type_Properties)
Fuel_Type_Properties <- Fuel_Type_Properties[c("id","Fuel_Type","Fuel_Type_Name","Annual_Petrol_Consumption","City_MPG",
                 "Highway_MPG","Combined_MPG","Tailpipe_CO2","Fuel_Cost")]

# convert necessary columns to factors
# if factors levels are "", convert to NA
Fuel_Type_Properties$Fuel_Type <- as.factor(Fuel_Type_Properties$Fuel_Type)

Fuel_Type_Properties$Fuel_Type_Name <- as.factor(Fuel_Type_Properties$Fuel_Type_Name)
not_NA_indices <- which(Fuel_Type_Properties$Fuel_Type_Name %in% levels(Fuel_Type_Properties$Fuel_Type_Name)[-1])
Fuel_Type_Properties$Fuel_Type_Name[-not_NA_indices] <- NA

NA_indices <- which(Fuel_Type_Properties$Annual_Petrol_Consumption == 0)
Fuel_Type_Properties$Annual_Petrol_Consumption[NA_indices] <- NA

NA_indices <- which(Fuel_Type_Properties$City_MPG == 0)
Fuel_Type_Properties$City_MPG[NA_indices] <- NA

NA_indices <- which(Fuel_Type_Properties$Highway_MPG == 0)
Fuel_Type_Properties$Highway_MPG[NA_indices] <- NA

NA_indices <- which(Fuel_Type_Properties$Combined_MPG == 0)
Fuel_Type_Properties$Combined_MPG[NA_indices] <- NA

NA_indices <- which(Fuel_Type_Properties$Tailpipe_CO2 == 0)
Fuel_Type_Properties$Tailpipe_CO2[NA_indices] <- NA

NA_indices <- which(Fuel_Type_Properties$Fuel_Cost == 0)
Fuel_Type_Properties$Fuel_Cost[NA_indices] <- NA

# Now we create a table with all other properties
indices <- match(c("id","year","make","model","atvType","cylinders","charge120","charge240","cityCD","cityE",
                   "cityUF","combE","combinedCD","combinedUF","displ","drive","eng_dscr", "evMotor",
                   "highwayUF","hlv","hpv","lv2","lv4", "phevBlended","pv2","pv4","trany","youSaveSpend",
                   "sCharger","tCharger","c240bDscr","createdOn","modifiedOn","startStop","phevCity","phevHwy","phevComb"), names(vehicles))
Vehicle_Properties <- vehicles[indices]
Vehicle_Properties <- arrange(Vehicle_Properties, id)

# write output CSV files
write.csv(Fuel_Type_Properties, file = "Fuel Type Properties.csv", row.names = FALSE)
write.csv(Vehicle_Properties, file = "Vehicle Properties.csv",row.names = FALSE)