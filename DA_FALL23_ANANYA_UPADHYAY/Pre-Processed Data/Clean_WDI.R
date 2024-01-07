setwd('/Users/ananya/Desktop/DA_FALL23_ANANYA_UPADHYAY')
wdi.data<-read.csv('WDI_data.csv')
View(wdi.data)
wdi.data[wdi.data == ".." | wdi.data == ""] <- NA
attach(wdi.data)

factor_columns <- c("Country_Name", "Country_Code")
wdi.data[factor_columns] <- lapply(wdi.data[factor_columns], as.factor)
numeric_columns <- setdiff(names(wdi.data), factor_columns)
wdi.data[numeric_columns] <- lapply(wdi.data[numeric_columns], as.numeric)

str(wdi.data)

library(dplyr)
columns_to_check <- setdiff(names(wdi.data), c("Country_Code", "Year","Country_Name"))

na_count_df <- wdi.data %>%
  group_by(Country_Name) %>%
  summarise(across(all_of(columns_to_check), ~sum(is.na(.))))

na_count_df <- na_count_df %>%
  mutate(across(all_of(columns_to_check), ~ifelse(is.na(as.numeric(as.character(.))), NA, as.numeric(as.character(.)))))
na_count_df
na_count_df <- t(na_count_df)
View(na_count_df)

write.csv(na_count_df,"Missing Values in WDI.csv")


selected_columns <- c("Year", "Country_Name", "Country_Code",
                      "Agricultural.land....of.land.area.",
                      "Agricultural.machinery..tractors",
                      "Arable.land....of.land.area.",
                      "Fertilizer.consumption..kilograms.per.hectare.of.arable.land.",
                      "CO2.emissions..kt.",
                      "Inflation..consumer.prices..annual...",
                      "GDP.growth..annual...")

# Creating a new data frame with only the selected columns
new_wdi.data <- wdi.data[selected_columns]
View(new_wdi.data)

zoo_data <- zoo(new_wdi.data$Agricultural.land....of.land.area.)
interpolated_data <- na.approx(zoo_data)
interpolated_data <- c(interpolated_data, NA)
new_wdi.data$Agricultural.land....of.land.area. <- as.numeric(interpolated_data)


zoo_data <- zoo(new_wdi.data$Agricultural.machinery..tractors)
interpolated_data <- na.approx(zoo_data)
new_wdi.data$Agricultural.machinery..tractors[is.na(new_wdi.data$Agricultural.machinery..tractors)] <- as.numeric(interpolated_data)


zoo_data <- zoo(new_wdi.data$Arable.land....of.land.area.)
interpolated_data <- na.approx(zoo_data)
interpolated_data <- c(interpolated_data, NA)
new_wdi.data$Arable.land....of.land.area. <- as.numeric(interpolated_data)


zoo_data <- zoo(new_wdi.data$Fertilizer.consumption..kilograms.per.hectare.of.arable.land.)
interpolated_data <- na.approx(zoo_data)
interpolated_data <- c(interpolated_data, NA)
new_wdi.data$Fertilizer.consumption..kilograms.per.hectare.of.arable.land. <- as.numeric(interpolated_data)

zoo_data <- zoo(new_wdi.data$CO2.emissions..kt.)
interpolated_data <- na.approx(zoo_data)
new_wdi.data$CO2.emissions..kt.[is.na(new_wdi.data$CO2.emissions..kt.)] <- as.numeric(interpolated_data)

zoo_data <- zoo(new_wdi.data$Inflation..consumer.prices..annual...)
interpolated_data <- na.approx(zoo_data)
new_wdi.data$Inflation..consumer.prices..annual...[is.na(new_wdi.data$Inflation..consumer.prices..annual...)] <- as.numeric(interpolated_data)

zoo_data <- zoo(new_wdi.data$GDP.growth..annual...)
interpolated_data <- na.approx(zoo_data)
new_wdi.data$GDP.growth..annual...[is.na(new_wdi.data$GDP.growth..annual...)] <- as.numeric(interpolated_data)


write.csv(new_wdi.data,'Cleaned WDI Data.csv')

