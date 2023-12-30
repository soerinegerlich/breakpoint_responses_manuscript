#Figure 2 with restrictions on standard errors

library(tidyverse)
library(readxl)

#Read datasets

df_summary_all_onset <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_snow_final.xlsx")
df_summary_all_temp_onset <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_temp_final.xlsx")
df_summary_all_peak <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_snow_final.xlsx")
df_summary_all_temp_peak <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_temp_final.xlsx")
df_summary_all_end <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_snow_final.xlsx")
df_summary_all_temp_end <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_temp_final.xlsx")

#Onset snow

#Onset snow

#Create new dataframe with mean values from meta-analysis
Slopedifference_onset_snow <- 0.67
SlopediffSE_onset_snow <- 0.29
Slope1_onset_snow <- 0.17
SlopeSE_onset_snow <- 0.09

df_mean_onset_snow <- as.data.frame(Slopedifference_onset_snow)
df_mean_onset_snow$SlopediffSE <- SlopediffSE_onset_snow
df_mean_onset_snow$Slope1 <- Slope1_onset_snow
df_mean_onset_snow$SlopeSE <- SlopeSE_onset_snow


df_summary_all_onset$condition <-
  ifelse(
    df_summary_all_onset$SEslope < 1 |
      df_summary_all_onset$SEslopediff < 1,
    df_summary_all_onset$SpeciesID,
    NA
  )

df_summary_all_onset$SpeciesID <-
  factor(
    df_summary_all_onset$SpeciesID,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Coccoidea",
      "Aphidoidea",
      "Chalcidoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Culicidae",
      "Muscidae",
      "Nymphalidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae",
      "Thomisidae"
    )
  )

df_summary_all_onset$condition <-
  factor(
    df_summary_all_onset$condition,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Coccoidea",
      "Aphidoidea",
      "Chalcidoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Culicidae",
      "Muscidae",
      "Nymphalidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae",
      "Thomisidae"
    )
  )


Onset_snow <- ggplot(df_summary_all_onset) +
  geom_point(
    mapping = aes(
      Slope1,
      Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      fill = condition,
      color = SpeciesID
    ),
    size = 6,
    alpha = 0.8,
    stroke = 1
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
  scale_color_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "chartreuse3",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    )
  ) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    ),
    na.value = "white"
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  xlim(-3, 3) +
  ylim(-3, 5) +
  geom_point(
    data = df_mean_onset_snow,
    mapping = aes(Slope1, Slopedifference_onset_snow),
    size = 6,
    shape = 4,
    stroke = 4
  ) +
  theme_bw() +
  labs(shape = "Habitat", colour = "Taxa") +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 20,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20,
      color = "black"
    ))
    #plot.margin = margin(-7.5, 5, 5.5, 1, "cm")


#Onset temp

df_summary_all_temp_onset$condition <-
  ifelse(
    df_summary_all_temp_onset$SEslope < 15 &
      df_summary_all_temp_onset$SEslopediff < 15,
    df_summary_all_temp_onset$SpeciesID,
    NA
  )


df_summary_all_temp_onset$SpeciesID <-
  factor(
    df_summary_all_temp_onset$SpeciesID,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Coccoidea",
      "Aphidoidea",
      "Chalcidoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Culicidae",
      "Muscidae",
      "Nymphalidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae",
      "Thomisidae"
    )
  )

df_summary_all_temp_onset$condition <-
  factor(
    df_summary_all_temp_onset$condition,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Coccoidea",
      "Aphidoidea",
      "Chalcidoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Culicidae",
      "Muscidae",
      "Nymphalidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae",
      "Thomisidae"
    )
  )

#Create new dataframe with mean values from meta analysis
Slopedifference_onset_temp <- -0.74
SlopediffSE_onset_temp <- 2.19
Slope1_onset_temp <- -0.92
SlopeSE_onset_temp <- 1.44

df_mean_onset_temp <- as.data.frame(Slopedifference_onset_temp)
df_mean_onset_temp$SlopediffSE <- SlopediffSE_onset_temp
df_mean_onset_temp$Slope1 <- Slope1_onset_temp
df_mean_onset_temp$SlopeSE <- SlopeSE_onset_temp

Onset_temp <- ggplot(df_summary_all_temp_onset) +
  geom_point(
    mapping = aes(
      Slope1,
      Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      fill = condition,
      color = SpeciesID
    ),
    size = 6,
    alpha = 0.8,
    stroke = 1
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
  scale_color_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "chartreuse3",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    )
  ) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    ),
    na.value = "white"
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.3)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.3)+
  xlim(-50, 50) +
  ylim(-60, 60) +
  geom_point(
    data = df_mean_onset_temp,
    mapping = aes(Slope1, Slopedifference_onset_temp),
    size = 6,
    shape = 4,
    stroke = 4
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 20,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20,
      color = "black"
    ))
    #plot.margin = margin(-7.5, 1, 5.5, 4, "cm")


#Peak snow

#Create new dataframe with mean values from mata-analysis
Slopedifference_peak_snow <- 0.51
SlopediffSE_peak_snow <- 0.30
Slope1_peak_snow <- 0.03
SlopeSE_peak_snow <- 0.15

df_mean_peak_snow <- as.data.frame(Slopedifference_peak_snow)
df_mean_peak_snow$SlopediffSE <- SlopediffSE_peak_snow
df_mean_peak_snow$Slope1 <- Slope1_peak_snow
df_mean_peak_snow$SlopeSE <- SlopeSE_peak_snow

df_summary_all_peak$condition <-
  ifelse(
    df_summary_all_peak$SEslope < 1 |
      df_summary_all_peak$SEslopediff < 1,
    df_summary_all_peak$SpeciesID,
    NA
  )

df_summary_all_peak$SpeciesID <-
  factor(
    df_summary_all_peak$SpeciesID,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Coccoidea",
      "Aphidoidea",
      "Chalcidoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Culicidae",
      "Muscidae",
      "Nymphalidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae",
      "Thomisidae"
    )
  )

df_summary_all_peak$condition <-
  factor(
    df_summary_all_peak$condition,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Coccoidea",
      "Aphidoidea",
      "Chalcidoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Culicidae",
      "Muscidae",
      "Nymphalidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae",
      "Thomisidae"
    )
  )


Peak_snow <- ggplot(df_summary_all_peak) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(
    mapping = aes(
      Slope1,
      Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      fill = condition,
      color = SpeciesID
    ),
    size = 6,
    alpha = 0.8,
    stroke = 1
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
  scale_color_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "chartreuse3",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    )
  ) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "chartreuse3",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    ),
    na.value = "white"
  ) +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  xlim(-3, 3) +
  ylim(-3, 5) +
  geom_point(
    data = df_mean_peak_snow,
    mapping = aes(Slope1, Slopedifference_peak_snow),
    size = 6,
    shape = 4,
    stroke = 4
  ) +
  theme_bw() +
  labs(shape = "Habitat", colour = "Taxa") +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 20,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20,
      color = "black"
    ))
    #plot.margin = margin(-5, 5, 3, 1, "cm")


#Peak temperature
#Create new dataframe with mean values from meta-analysis
Slopedifference_peak_temp <- 5.72
SlopediffSE_peak_temp <- 2.06
Slope1_peak_temp <- -6.41
SlopeSE_peak_temp <- 1.97

df_mean_peak_temp <- as.data.frame(Slopedifference_peak_temp)
df_mean_peak_temp$SlopediffSE <- SlopediffSE_peak_temp
df_mean_peak_temp$Slope1 <- Slope1_peak_temp
df_mean_peak_temp$SlopeSE <- SlopeSE_peak_temp

df_summary_all_temp_peak$condition <-
  ifelse(
    df_summary_all_temp_peak$SEslope < 15 &
      df_summary_all_temp_peak$SEslopediff < 15,
    df_summary_all_temp_peak$SpeciesID,
    NA
  )


df_summary_all_temp_peak$SpeciesID <-
  factor(
    df_summary_all_temp_peak$SpeciesID,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Coccoidea",
      "Aphidoidea",
      "Chalcidoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Culicidae",
      "Muscidae",
      "Nymphalidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae",
      "Thomisidae"
    )
  )

df_summary_all_temp_peak$condition <-
  factor(
    df_summary_all_temp_peak$condition,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Coccoidea",
      "Aphidoidea",
      "Chalcidoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Culicidae",
      "Muscidae",
      "Nymphalidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae",
      "Thomisidae"
    )
  )


Peak_temp <- ggplot(df_summary_all_temp_peak) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(
    mapping = aes(
      Slope1,
      Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      fill = condition,
      color = SpeciesID
    ),
    size = 6,
    alpha = 0.8,
    stroke = 1
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
  scale_color_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "chartreuse3",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    )
  ) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    ),
    na.value = "white"
  ) +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.3)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.3)+
  xlim(-50, 50) +
  ylim(-60, 60) +
  geom_point(
    data = df_mean_peak_temp,
    mapping = aes(Slope1, Slopedifference_peak_temp),
    size = 6,
    shape = 4,
    stroke = 4
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 20,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20,
      color = "black"
    )
    #plot.margin = margin(-5, 1, 3, 4, "cm")
  )



#End snow

#Create new dataframe with mean values
Slopedifference_end_snow <- 0.23
SlopediffSE_end_snow <- 0.18
Slope1_end_snow <- 0.07
SlopeSE_end_snow <- 0.15

df_mean_end_snow <- as.data.frame(Slopedifference_end_snow)
df_mean_end_snow$SlopediffSE <- SlopediffSE_end_snow
df_mean_end_snow$Slope1 <- Slope1_end_snow
df_mean_end_snow$SlopeSE <- SlopeSE_end_snow


df_summary_all_end$condition <-
  ifelse(
    df_summary_all_end$SEslope < 1 |
      df_summary_all_end$SEslopediff < 1,
    df_summary_all_end$SpeciesID,
    NA
  )

df_summary_all_end$SpeciesID <-
  factor(
    df_summary_all_end$SpeciesID,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Coccoidea",
      "Aphidoidea",
      "Chalcidoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Culicidae",
      "Muscidae",
      "Nymphalidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae",
      "Thomisidae"
    )
  )

df_summary_all_end$condition <-
  factor(
    df_summary_all_end$condition,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Coccoidea",
      "Aphidoidea",
      "Chalcidoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Culicidae",
      "Muscidae",
      "Nymphalidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae",
      "Thomisidae"
    )
  )


End_snow <- ggplot(df_summary_all_end) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(
    mapping = aes(
      Slope1,
      Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      fill = condition,
      color = SpeciesID
    ),
    size = 6,
    alpha = 0.8,
    stroke = 1
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
  scale_color_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "chartreuse3",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    )
  ) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "chartreuse3",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    ),
    na.value = "white"
  ) +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  xlim(-3, 3) +
  ylim(-3, 5) +
  geom_point(
    data = df_mean_end_snow,
    mapping = aes(Slope1, Slopedifference_end_snow),
    size = 6,
    shape = 4,
    stroke = 4
  ) +
  theme_bw() +
  labs(shape = "Habitat", colour = "Taxa") +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 20,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20,
      color = "black"
    )
    #plot.margin = margin(-2.5, 5, 0.5, 1, "cm")
  )


#End temp


#Create new dataframe with mean values from meta-analysis
Slopedifference_end_temp <- 3.10
SlopediffSE_end_temp <- 2.12
Slope1_end_temp <- -2.36
SlopeSE_end_temp <- 1.13

df_mean_end_temp <- as.data.frame(Slopedifference_end_temp)
df_mean_end_temp$SlopediffSE <- SlopediffSE_end_temp
df_mean_end_temp$Slope1 <- Slope1_end_temp
df_mean_end_temp$SlopeSE <- SlopeSE_end_temp

df_summary_all_temp_end$condition <-
  ifelse(
    df_summary_all_temp_end$SEslope < 15 |
      df_summary_all_temp_end$SEslopediff < 15,
    df_summary_all_temp_end$SpeciesID,
    NA
  )


df_summary_all_temp_end$SpeciesID <-
  factor(
    df_summary_all_temp_end$SpeciesID,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Coccoidea",
      "Aphidoidea",
      "Chalcidoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Culicidae",
      "Muscidae",
      "Nymphalidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae",
      "Thomisidae"
    )
  )

df_summary_all_temp_end$condition <-
  factor(
    df_summary_all_temp_end$condition,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Coccoidea",
      "Aphidoidea",
      "Chalcidoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Culicidae",
      "Muscidae",
      "Nymphalidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae",
      "Thomisidae"
    )
  )

End_temp <- ggplot(df_summary_all_temp_end) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(
    mapping = aes(
      Slope1,
      Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      fill = condition,
      color = SpeciesID
    ),
    size = 6,
    alpha = 0.8,
    stroke = 1
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
  scale_color_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "chartreuse3",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    )
  ) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "chartreuse3",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    ),
    na.value = "white"
  ) +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.3)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.3)+
  xlim(-50, 50) +
  ylim(-60, 60) +
  geom_point(
    data = df_mean_end_temp,
    mapping = aes(Slope1, Slopedifference_end_temp),
    size = 6,
    shape = 4,
    stroke = 4
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 20,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20,
      color = "black"
    )
   # plot.margin = margin(-2.5, 1, 0.5, 4, "cm")
  )




require(ggpubr)

ggarrange(
    Onset_temp,
    Onset_snow,
    Peak_temp,
    Peak_snow,
    End_temp,
    End_snow,
    labels = c(
      "b. Onset - Temperature",
      "c. Onset - Snowmelt",
      "d. Peak - Temperature",
      "e. Peak - Snowmelt",
      "f. End - Temperature",
      "g. End - Snowmelt"
    ),
    hjust = -0.3,
    vjust = -0.5,
    ncol = 2,
    nrow = 3,
    legend = "none",
    font.label = list(color = "black", size = 18)
  ) +
    theme(plot.margin = margin(2, 3, 2, 3, "cm"))

