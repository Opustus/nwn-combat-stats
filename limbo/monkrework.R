roll20 <- 1:20

attacks <- function(ab, base_apr, ubab = TRUE) {
  first_attack_ab <- ab
  attack_decrement <- if (ubab) -3 else -5
  last_attack_ab <- first_attack_ab + attack_decrement * (base_apr - 1)
  seq(from = first_attack_ab, to = last_attack_ab, by = attack_decrement)
}

attacks_df <- function(ab, base_apr, ubab = FALSE, haste = TRUE, flurry = FALSE, dualwield = FALSE, ferocity = FALSE) {
  df <- data.frame("1" = roll20)
  
  attack_values <- attacks(ab, base_apr, ubab)
  
  for (i in 1:base_apr) {
    df[, i] <- attacks(ab, base_apr, ubab)[i] + roll20
  }
  
  attack_decrement <- if (ubab) 3 else 5
  
  if (haste) {
    df$haste <- attack_values[length(attack_values)] + roll20 - attack_decrement
  }
  
  if (flurry) {
    df$flurry <- attack_values[length(attack_values)] + roll20 - attack_decrement
    df <- df - 2
  }
  
  if (dualwield) {
    df$dualwield1 <- attacks(ab, base_apr, ubab)[1] + roll20
    df$dualwield2 <- attacks(ab, base_apr, ubab)[2] + roll20
    df <- df - 2
  }
  
  if (ferocity) {
    df$ferocity <- attacks(ab, base_apr, ubab)[1] + roll20
  }
  
  return(df)
}

hits <- function(ab, base_apr, ubab, haste, flurry, dualwield, ferocity, enemy_ac) {
  hit_rolls <- attacks_df(ab, base_apr, ubab, haste, flurry, dualwield, ferocity) - enemy_ac
  hit_rolls[1, ] <- -1
  hit_rolls[20, ] <- 1
  hits <- length(hit_rolls[hit_rolls >= 0])
  return(hits)
}

crits <- function(ab, base_apr, ubab, haste, flurry, dualwield, ferocity, enemy_ac, crit_range) {
  crit_rolls <- attacks_df(ab, base_apr, ubab, haste, flurry, dualwield, ferocity) - enemy_ac
  crits <- crit_rolls[crit_range:20, ]
  crits <- length(crits[crits >= 0])
  crit_chance <- crits/(length(crit_rolls)*nrow(crit_rolls))
  crits <- hits(ab, base_apr, ubab, haste, flurry, dualwield, ferocity, enemy_ac) * crit_chance
  return(crits)
}

damage <- function(ab, base_apr, ubab, haste, flurry, dualwield, ferocity, enemy_ac, crit_range, crit_threat, dmg_per_hit, sneak_per_hit) {
  crits_val <- crits(ab, base_apr, ubab, haste, flurry, dualwield, ferocity, enemy_ac, crit_range)
  hits_val <- hits(ab, base_apr, ubab, haste, flurry, dualwield, ferocity, enemy_ac) - crits_val
  hits_damage <- hits_val * (dmg_per_hit + sneak_per_hit)
  crits_damage <- dmg_per_hit * crits_val * crit_threat
  total_damage <- (hits_damage + crits_damage) / 20
  return(total_damage)
}

damage_to_ac_range <- function(ab, base_apr, ubab, haste, flurry, dualwield, ferocity, crit_range, crit_threat, dmg_per_hit, sneak_per_hit) {
  ac_ranges <- list(
    "vs AC 30 to 35" = 30:35,
    "vs AC 35 to 40" = 35:40,
    "vs AC 40 to 45" = 40:45,
    "vs AC 45 to 50" = 45:50,
    "vs AC 50 to 55" = 50:55,
    "vs AC 55 to 60" = 55:60,
    "vs AC 55 to 60" = 60:65,
    "vs AC 55 to 60" = 65:70
  )
  
  mean_damages <- sapply(ac_ranges, function(ac_range) {
    sapply(ac_range, function(enemy_ac) {
      damage(ab, base_apr, ubab, haste, flurry, dualwield, ferocity, enemy_ac, crit_range, crit_threat, dmg_per_hit, sneak_per_hit)
    }) |> mean()
  })
  
  return(as.data.frame(t(mean_damages)))
}

library(googlesheets4)
library(dplyr)

gs4_auth()

sheet_url <- "https://docs.google.com/spreadsheets/d/1zmUBmbZUbCmm5DpKoAuexaD1uU5RUFVDxE1-vwREUnY"

builds <- read_sheet(sheet_url, sheet = "Builds")

results <- lapply(1:nrow(builds), function(i) {
  row <- builds[i, ]
  damage_df <- damage_to_ac_range(
    ab = row$ab,
    base_apr = row$base_apr,
    ubab = as.logical(row$ubab),
    haste = as.logical(row$haste),
    flurry = as.logical(row$flurry),
    dualwield = as.logical(row$dualwield),
    ferocity = as.logical(row$ferocity),
    crit_range = row$crit_range,
    crit_threat = row$crit_threat,
    dmg_per_hit = row$dmg_per_hit,
    sneak_per_hit = row$sneak_per_hit
  )
  return(as.numeric(damage_df[1, ]))
})
results <- lapply(results, round)

damage_matrix <- do.call(rbind, results)
colnames(damage_matrix) <- c("vs_AC_30_35","vs_AC_35_40", "vs_AC_40_45", "vs_AC_45_50", "vs_AC_50_55", "vs_AC_55_60", "vs_AC_60_65", "vs_AC_65_70")

range_write(
  ss = sheet_url,
  data = as.data.frame(damage_matrix),
  sheet = "Builds",
  range = "N2",
  col_names = FALSE
)
