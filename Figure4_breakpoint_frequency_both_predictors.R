#Breakpoint frequency with both variables
library(tidyverse)
library(readxl)

#Get new dataset
df_phen_event_final <- read_xlsx("Data/df_phen_event_final.xlsx")

df_phen_event_final %>%
  mutate(Habitat = case_when(
    Plot == "Art1" ~ "Wet fen",
    Plot == "Art2" ~ "Wet fen",
    Plot == "Art3" ~ "Mesic heath",
    Plot == "Art4" ~ "Mesic heath",
    Plot == "Art5" ~ "Arid heath",
    Plot == "Art7" ~ "Arid heath")) -> df_phen_event_final

df_phen_event_final$Plot[df_phen_event_final$Plot == "Art1"] <- "Plot 1" 
df_phen_event_final$Plot[df_phen_event_final$Plot == "Art2"] <- "Plot 2" 
df_phen_event_final$Plot[df_phen_event_final$Plot == "Art3"] <- "Plot 3" 
df_phen_event_final$Plot[df_phen_event_final$Plot == "Art4"] <- "Plot 4" 
df_phen_event_final$Plot[df_phen_event_final$Plot == "Art5"] <- "Plot 5" 
df_phen_event_final$Plot[df_phen_event_final$Plot == "Art6"] <- "Plot 6"
df_phen_event_final$Plot[df_phen_event_final$Plot == "Art7"] <- "Plot 7" 


df_phen_event_final$SpeciesID[df_phen_event_final$SpeciesID == "CHCE"] <- "Chironomidae"
df_phen_event_final$SpeciesID[df_phen_event_final$SpeciesID == "ANMU"] <- "Muscidae"
df_phen_event_final$SpeciesID[df_phen_event_final$SpeciesID == "MYSC"] <- "Sciaridae"


dfsnowmelt_climatestation <-
  read_xlsx("Data/Snowmelt_climatestation.xlsx")


#Match climate variables with phen. event data to compile them in the same dataframe
df_phen_event_final$Snowmelt <-
  dfsnowmelt_climatestation$DOY[match(paste0(df_phen_event_final$Year),
                                      paste0(dfsnowmelt_climatestation$Year))]


#Peak pheno temp

df_summary_temp_peak <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_temp_snow_final.xlsx")

df_phen_event_final$Break <-
  df_summary_temp_peak$Break[match(
    paste0(df_phen_event_final$SpeciesID, df_phen_event_final$Plot),
    paste0(df_summary_temp_peak$SpeciesID, df_summary_temp_peak$Plot)
  )]

df_phen_event_final$Pvalue <-
  df_summary_temp_peak$Pvalue[match(
    paste0(df_phen_event_final$SpeciesID, df_phen_event_final$Plot),
    paste0(df_summary_temp_peak$SpeciesID, df_summary_temp_peak$Plot)
  )]

df_phen_event_final$Significance_level_Peaktemp <-
  ifelse(df_phen_event_final$Pvalue < 0.06, "True", "False")

df_significant <-
  subset(df_phen_event_final, Significance_level_Peaktemp == "True")

df_significant %>%
  group_by(SpeciesID, Plot) %>%
  reframe(
    Break = unique(Break),
    Pvalue = unique(Pvalue),
    Min = min(Peak_Temp, na.rm = T),
    Max = max(Peak_Temp, na.rm = T),
    Difference = (Max - Min),
    BP_Min = (Break - Min),
    BP_Min_Max = ((Break - Min) / (Max - Min))
  ) -> df1


h <-
  hist(
    df1$BP_Min_Max,
    breaks = 2,
    main = "Peak - Temperature",
    xlim = (0:1),
    xlab = "",
    cex.lab = 1.5
  )



#Onset pheno temp

df_summary_temp_onset <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_temp_snow_final.xlsx")

df_phen_event_final$Break_onset <-
  df_summary_temp_onset$Break[match(
    paste0(df_phen_event_final$SpeciesID, df_phen_event_final$Plot),
    paste0(df_summary_temp_onset$SpeciesID, df_summary_temp_onset$Plot)
  )]

df_phen_event_final$Pvalue_onset <-
  df_summary_temp_onset$Pvalue[match(
    paste0(df_phen_event_final$SpeciesID, df_phen_event_final$Plot),
    paste0(df_summary_temp_peak$SpeciesID, df_summary_temp_peak$Plot)
  )]

df_phen_event_final$Significance_level_Onsettemp <-
  ifelse(df_phen_event_final$Pvalue_onset < 0.06, "True", "False")

df_significant <-
  subset(df_phen_event_final, Significance_level_Onsettemp == "True")


df_significant %>%
  group_by(SpeciesID, Plot) %>%
  reframe(
    Break = unique(Break_onset),
    Pvalue = unique(Pvalue_onset),
    Min = min(Onset_Temp, na.rm = T),
    Max = max(Onset_Temp, na.rm = T),
    Difference = (Max - Min),
    BP_Min = (unique(Break_onset) - Min),
    BP_Min_Max = ((unique(Break_onset) - Min) / (Max - Min))
  ) -> df2

h2 <-
  hist(
    df2$BP_Min_Max,
    breaks = 3,
    main = "Onset - Temperature",
    xlim = (0:1),
    xlab = "",
    cex.lab = 1.5
  )


#End pheno temp
df_summary_temp_end <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_temp_snow_final.xlsx")

df_phen_event_final$Break_end <-
  df_summary_temp_end$Break[match(
    paste0(df_phen_event_final$SpeciesID, df_phen_event_final$Plot),
    paste0(df_summary_temp_end$SpeciesID, df_summary_temp_end$Plot)
  )]

df_phen_event_final$Pvalue_end <-
  df_summary_temp_end$Pvalue[match(
    paste0(df_phen_event_final$SpeciesID, df_phen_event_final$Plot),
    paste0(df_summary_temp_end$SpeciesID, df_summary_temp_end$Plot)
  )]

df_phen_event_final$Significance_level_Endtemp <-
  ifelse(df_phen_event_final$Pvalue_end < 0.06, "True", "False")

df_significant <-
  subset(df_phen_event_final, Significance_level_Endtemp == "True")

df_significant %>%
  group_by(SpeciesID, Plot) %>%
  reframe(
    Break = unique(Break_end),
    Min = min(End_Temp, na.rm = T),
    Max = max(End_Temp, na.rm = T),
    Difference = (Max - Min),
    BP_Min = (unique(Break_end) - Min),
    BP_Min_Max = ((unique(Break_end) - Min) / (Max - Min))
  ) -> df3

h3 <-
  hist(
    df3$BP_Min_Max,
    breaks = 6,
    main = "End - Temperature",
    xlim = (0:1),
    xlab = "Breakpoint location",
    cex.lab = 1.5
  )


#Snowmelt onset

df_summary_snow_onset <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_snow_temp_final.xlsx")


df_phen_event_final$Break_snow_onset <-
  df_summary_snow_onset$Break[match(
    paste0(df_phen_event_final$SpeciesID, df_phen_event_final$Plot),
    paste0(df_summary_snow_onset$SpeciesID, df_summary_snow_onset$Plot)
  )]

df_phen_event_final$Pvalue_snow_onset <-
  df_summary_snow_onset$Pvalue[match(
    paste0(df_phen_event_final$SpeciesID, df_phen_event_final$Plot),
    paste0(df_summary_snow_onset$SpeciesID, df_summary_snow_onset$Plot)
  )]

df_phen_event_final$Significance_level_Onset_snow <-
  ifelse(df_phen_event_final$Pvalue_snow_onset < 0.06, "True", "False")

df_significant <-
  subset(df_phen_event_final, Significance_level_Onset_snow == "True")

df_significant %>%
  group_by(SpeciesID, Plot) %>%
  reframe(
    Break = unique(Break_snow_onset),
    Pvalue = unique(Pvalue_snow_onset),
    Min = min(Snowmelt, na.rm = T),
    Max = max(Snowmelt, na.rm = T),
    Difference = (Max - Min),
    BP_Min = (unique(Break_snow_onset) - Min),
    BP_Min_Max = ((unique(Break_snow_onset) - Min) / (Max - Min))
  ) -> df4

h4 <-
  hist(
    df4$BP_Min_Max,
    breaks = 6,
    main = "Onset - Snowmelt",
    xlim = (0:1),
    xlab = "",
    ylab = ""
  )



#Snowmelt peak
df_summary_snow <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_snow_temp_final.xlsx")

df_phen_event_final$Break_snow <-
  df_summary_snow$Break[match(
    paste0(df_phen_event_final$SpeciesID, df_phen_event_final$Plot),
    paste0(df_summary_snow$SpeciesID, df_summary_snow$Plot)
  )]

df_phen_event_final$Pvalue_snow_peak <-
  df_summary_snow$Pvalue[match(
    paste0(df_phen_event_final$SpeciesID, df_phen_event_final$Plot),
    paste0(df_summary_snow$SpeciesID, df_summary_snow$Plot)
  )]

df_phen_event_final$Significance_level_Peak_snow <-
  ifelse(df_phen_event_final$Pvalue_snow_peak < 0.06, "True", "False")

df_significant <-
  subset(df_phen_event_final, Significance_level_Peak_snow == "True")

df_significant %>%
  group_by(SpeciesID, Plot) %>%
  reframe(
    Break = unique(Break_snow),
    Min = min(Snowmelt, na.rm = T),
    Max = max(Snowmelt, na.rm = T),
    Difference = (Max - Min),
    BP_Min = (unique(Break_snow) - Min),
    BP_Min_Max = ((unique(Break_snow) - Min) / (Max - Min))
  ) -> df5

h5 <-
  hist(
    df5$BP_Min_Max,
    breaks = 6,
    main = "Peak - Snowmelt",
    xlim = (0:1),
    xlab = "",
    ylab = ""
  )

#Snowmelt end
df_summary_snow_end <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_snow_temp_final.xlsx")

df_phen_event_final$Break_snow_end <-
  df_summary_snow_end$Break[match(
    paste0(df_phen_event_final$SpeciesID, df_phen_event_final$Plot),
    paste0(df_summary_snow_end$SpeciesID, df_summary_snow_end$Plot)
  )]

df_phen_event_final$Pvalue_snow_end <-
  df_summary_snow_end$Pvalue[match(
    paste0(df_phen_event_final$SpeciesID, df_phen_event_final$Plot),
    paste0(df_summary_snow_end$SpeciesID, df_summary_snow_end$Plot)
  )]

df_phen_event_final$Significance_level_Endsnow <-
  ifelse(df_phen_event_final$Pvalue_snow_end < 0.06, "True", "False")

df_significant <-
  subset(df_phen_event_final, Significance_level_Endsnow == "True")

df_significant %>%
  group_by(SpeciesID, Plot) %>%
  reframe(
    Break = unique(Break_snow_end),
    Min = min(Snowmelt, na.rm = T),
    Max = max(Snowmelt, na.rm = T),
    Difference = (Max - Min),
    BP_Min = (unique(Break_snow_end) - Min),
    BP_Min_Max = ((unique(Break_snow_end) - Min) / (Max - Min))
  ) -> df6

h6 <-
  hist(
    df6$BP_Min_Max,
    breaks = 6,
    main = "End - Snowmelt",
    xlim = (0:1),
    xlab = "Breakpoint location",
    ylab = "",
    cex.lab = 1.5
  )

par(mfrow = c(3, 2), oma = c(12,6,10,6))

plot(h)
plot(h4)
plot(h2)
plot(h5)
plot(h3)
plot(h6)

dev.off()
