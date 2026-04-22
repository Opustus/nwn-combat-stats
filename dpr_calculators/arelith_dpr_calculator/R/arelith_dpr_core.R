# nwn_dpr_core.R

#' Standard d20 roll distribution for probability calculations
roll20 <- 1:20

#' Calculate Base Attack Bonuses
attacks <- function(ab, base_apr) {
  first_attack_ab <- ab
  attack_decrement <- -5
  last_attack_ab <- first_attack_ab + attack_decrement * (base_apr - 1)
  seq(from = first_attack_ab, to = last_attack_ab, by = attack_decrement)
}

#' Generate Attack Roll Matrix
attacks_df <- function(ab, base_apr, haste = TRUE, dualwield = FALSE, brawler = FALSE, flurry = FALSE, extra_apr = FALSE) {
  
  # Apply global penalties AB penalties
  if (dualwield) ab <- ab - 1
  if (flurry) ab <- ab - 2
  
  attack_values <- attacks(ab, base_apr)
  df <- data.frame("1" = roll20)
  
  # Populate base attacks
  for (i in 1:base_apr) {
    df[, i] <- attacks(ab, base_apr)[i] + roll20
  }
  
  attack_decrement <- 5
  
  # Add extra attack to tail from Haste
  if (haste) {
    df$haste <- attack_values[length(attack_values)] + roll20 - attack_decrement
  }
  # Add extra attacks from Dual Wielding
  if (dualwield) {
    df$dualwield1 <- attack_values[1] + roll20
    df$dualwield2 <- attack_values[2] + roll20
  }
  # Add 2 extra attacks to tail from Brawler
  if (brawler) {
    df$brawl1 <- attack_values[length(attack_values)] + roll20 - attack_decrement
    df$brawl2 <- attack_values[length(attack_values)] + roll20 - (attack_decrement * 2)
  }
  # Add extra attack to tail from Flurry of Blows
  if (flurry) {
    df$flurry <- attack_values[length(attack_values)] + roll20 - attack_decrement
  }
  # Add extra attack at full AB
  if (extra_apr) {
    df$extra_apr <- attack_values[1] + roll20
  }
  
  return(df)
}

#' Calculate Expected Hits
hits <- function(ab, base_apr, haste, dualwield, brawler, flurry, extra_apr, enemy_ac) {
  hit_rolls <- attacks_df(ab, base_apr, haste, dualwield, brawler, flurry, extra_apr) - enemy_ac
  
  # NWN Rule: Natural 1 always misses, Natural 20 always hits
  hit_rolls[1, ] <- -1
  hit_rolls[20, ] <- 1
  
  hits <- length(hit_rolls[hit_rolls >= 0])
  return(hits)
}

#' Calculate Expected Critical Hits
crits <- function(ab, base_apr, haste, dualwield, brawler, flurry, extra_apr, enemy_ac, crit_range) {
  crit_rolls <- attacks_df(ab, base_apr, haste, dualwield, brawler, flurry, extra_apr) - enemy_ac
  
  # Isolate rolls that fall within the critical threat range
  crits <- crit_rolls[crit_range:20, ]
  crits <- length(crits[crits >= 0])
  
  # Calculate base percentage chance to threaten
  crit_chance <- crits / (length(crit_rolls) * nrow(crit_rolls))
  
  # Multiply threat chance by total successful hits to get confirmed crits
  crits <- hits(ab, base_apr, haste, dualwield, brawler, flurry, extra_apr, enemy_ac) * crit_chance
  return(crits)
}

#' Calculate Average Damage Per Round (DPR)
damage <- function(ab, base_apr, haste, dualwield, brawler, flurry, extra_apr, enemy_ac, crit_range, crit_threat, dmg_per_hit, sneak_per_hit) {
  crits_val <- crits(ab, base_apr, haste, dualwield, brawler, flurry, extra_apr, enemy_ac, crit_range)
  hits_val <- hits(ab, base_apr, haste, dualwield, brawler, flurry, extra_apr, enemy_ac) - crits_val
  
  # Regular hits calculate base damage + sneak attack
  hits_damage <- hits_val * (dmg_per_hit + sneak_per_hit)
  
  # Crits multiply base damage, but do NOT multiply sneak attack damage
  crits_damage <- dmg_per_hit * crits_val * crit_threat
  
  # Divide by 20 to average out the 1:20 matrix permutations
  total_damage <- (hits_damage + crits_damage) / 20
  return(total_damage)
}

#' Calculate DPR Over an AC Sequence
calculate_dpr_for_build <- function(row_data, ac_sequence) {
  sapply(ac_sequence, function(enemy_ac) {
    damage(
      ab = row_data$"Stable AB",
      base_apr = row_data$"Base APR",
      haste = as.logical(row_data$"Has Haste"),
      dualwield = as.logical(row_data$"Has Dualwield"),
      brawler = as.logical(row_data$"Has Brawler"),
      flurry = as.logical(row_data$"Has Flurry"),
      extra_apr = as.logical(row_data$"Has Extra APR"),
      enemy_ac = enemy_ac,
      crit_range = row_data$"Critical Range",
      crit_threat = row_data$"Critical Threat",
      dmg_per_hit = row_data$"Damage Per Hit",
      sneak_per_hit = row_data$"Sneak Per Hit"
    )
  })
}