source("HelperFunctions/AutoReadUSBRData/AutoReadUSBRData.R")

#Pulls unregulated inflow data
hydrodata <- fReadReclamationHydroData(FALSE)
unreg_inflow_raw <- filter(hydrodata$dfResMonthly, ResName == "Lake Powell" & FieldName == "Unregulated Inflow Volume")

unreg_inflow_wy <- unreg_inflow_raw%>% 
  dplyr::group_by(WaterYear)%>%
  dplyr::summarise(unreg_inflow = sum(MonthlyValue, na.rm = TRUE), .groups = "drop")


#Pulls Natural flow data
nat_inflow_raw <- read_xlsx("HelperFunctions/Data/LFnatFlow1906-2024.2024.9.12.xlsx")
nat_inflow <- data.frame(WaterYear = as.numeric(nat_inflow_raw$`Last Updated 9/12/2024`[-(1:2)]), nat_inflow = as.numeric(nat_inflow_raw$...2[-(1:2)]) / 1000000)
                         

#Pulls Consumptive Use Data
ub_use_raw <- read.csv("HelperFunctions/Data/UpperBasinConsumptiveUse.csv")
ub_use_raw_t <- as.data.frame(t(ub_use_raw))
ub_use_raw_t$V5<- gsub(",", "", ub_use_raw_t$V5)
ub_use <- data.frame(ub_use = as.numeric(ub_use_raw_t$V5[-(1:6)]) / 1000000, WaterYear = as.numeric(ub_use_raw_t$V3[-(1:6)]))

inflow<-merge(unreg_inflow_wy, nat_inflow, by = "WaterYear")
df <- merge(inflow, ub_use, by = "WaterYear")
df_long <- df %>%
  pivot_longer(
    cols = c(unreg_inflow, nat_inflow, ub_use),
    names_to = "FlowType",
    values_to = "Value"
  )

# Plot
ggplot(df_long, aes(x = WaterYear, y = Value, color = Flow Type)) + 
    geom_line()+
    labs(x = "Water Year", y = "Flow (MAF)", color = "Flow Type") +
    theme_minimal()
  
