library(devtools)
library(irrkappa)
library(dplyr)

setwd("C:/Users/s4367707/AppData/Local/Programs/R/R-4.4.0/library/irrkappa")
devtools::load_all()
setwd("C:/Users/s4367707/Documents/R projs/IAA R package test")

tier <- "body_position"
replace_value <- FALSE
Overlap.threshold <- 51

df <- init(tier, replace_value = replace_value)
df.matches <- matches(df)
df.matches <- filter(df.matches, NMM.x != "other")
df.matches <- filter(df.matches, NMM.y != "other")
df.matches <- df.matches %>% drop_na(NMM.x, NMM.y)

categories <- c('forward', 'tilted', 'sideways', 'backward', 'unmatched')
categories_without_unmatched = categories[categories != "unmatched"]
from <- c("backward", "backward", "backward", "backward", "forward", "forward", "forward", "tilted", "tilted", "sideways", "unmatched", "unmatched", "unmatched", "unmatched", "unmatched")
to <- c("backward", "forward", "tilted", "sideways", "forward", "tilted", "sideways", "tilted", "sideways", "sideways", "unmatched", "backward", "forward", "tilted", "sideways")
real_weights <- c(0,1,0.5,0.5,0,0.5,0.5,0,0.5,0,0,1,1,1,1) 
real_weight_matrix <- create_weight_matrix(from, to, real_weights, categories)

df.matches$NMM.x <- factor(df.matches$NMM.x, levels = categories)
df.matches$NMM.y <- factor(df.matches$NMM.y, levels = categories)

confusion.matrices <- confusion.matrix(df.matches)
confusion <- confusion.matrices$confusion

agreement_kappa <- calculate_agreement_kappa(df = confusion, nmm_values = categories_without_unmatched)
agreement_alpha_sep_weighted_equal <- calculate_krippendorff_alpha_by_category(df = df.matches, nmm_values = categories)
agreement_alpha_sep_weighted_custom <- calculate_krippendorff_alpha_by_category_weighted(df = df.matches, nmm_values = categories, wm=real_weight_matrix)
agreement_alpha_weighted_equal <- calculate_krippendorff_alpha(df = df.matches, nmm_values = categories)
agreement_alpha_weighted_custom <- calculate_krippendorff_alpha_weighted(df = df.matches, nmm_values = categories, wm = real_weight_matrix)
plot.agreement_kappa(agreement_kappa)
plot.agreement_alpha_by_category(agreement_alpha_sep_weighted_equal)
plot.agreement_alpha_by_category(agreement_alpha_sep_weighted_custom)
plot.agreement_alpha(agreement_alpha_weighted_equal)
plot.agreement_alpha(agreement_alpha_weighted_custom)