# nwn_dpr_core.R

#' Standard d20 roll distribution for probability calculations
roll20 <- 1:20

#' Calculate Base Attack Bonuses
#' 
#' Generates a sequence of attack bonuses for a character's base attacks per round.
#'
#' @param ab Numeric. The highest Attack Bonus (AB) for the first attack.
#' @param base_apr Numeric. Base Attacks Per Round (APR).
#'
#' @return A numeric vector of attack bonuses for each base attack.
attacks <- function(ab, base_apr) {
  first_attack_ab <- ab
  attack_decrement <- -5
  last_attack_ab <- first_attack_ab + attack_decrement * (base_apr - 1)
  seq(from = first_attack_ab, to = last_attack_ab, by = attack_decrement)
}

#' Generate Attack Roll Matrix
#' 
#' Creates a data frame representing all possible d20 outcomes (1-20) for every 
#' attack made in a round, factoring in extra attacks from buffs and feats.
#'
#' @param ab Numeric. Highest Attack Bonus.
#' @param base_apr Numeric. Base Attacks Per Round.
#' @param haste Logical. Adds one attack at lowest AB - 5.
#' @param flurry Logical. Adds one attack at lowest AB - 5, applies -2 penalty to all attacks.
#' @param dualwield Logical. Adds two off-hand attacks, applies -1 penalty to all attacks.
#' @param extra_apr Logical. Adds one attack at highest AB.
#'
#' @return A data frame where each column represents an attack and each row represents a d20 roll outcome (1-20).
attacks_df <- function(ab, base_apr, haste = TRUE, flurry = FALSE, dualwield = FALSE, extra_apr = FALSE) {
  df <- data.frame("1" = roll20)
  attack_values <- attacks(ab, base_apr)
  
  # Populate base attacks
  for (i in 1:base_apr) {
    df[, i] <- attacks(ab, base_apr)[i] + roll20
  }
  
  attack_decrement <- 5
  
  # Add extra attack from Haste
  if (haste) df$haste <- attack_values[length(attack_values)] + roll20 - attack_decrement
  # Add extra attack from Flurry of Blows and apply AB penalty
  if (flurry) {
    df$flurry <- attack_values[length(attack_values)] + roll20 - attack_decrement
    df <- df - 2
  }
  # Add extra attacks from Dual Wielding and apply AB penalty
  if (dualwield) {
    df$dualwield1 <- attacks(ab, base_apr)[1] + roll20
    df$dualwield2 <- attacks(ab, base_apr)[2] + roll20
    df <- df - 1
  }
  # Add extra attack at full BAB (barbarian extra APR)
  if (extra_apr) df$extra_apr <- attacks(ab, base_apr)[1] + roll20
  
  return(df)
}

#' Calculate Expected Hits
#' 
#' Determines the total number of successful hits across all possible permutations 
#' of a full round of attacks against a specific Armor Class (AC).
#' Accounts for NWN1 mechanics where a natural 1 is an automatic miss and 20 is an automatic hit.
#'
#' @param ab Numeric. Highest Attack Bonus.
#' @param base_apr Numeric. Base Attacks Per Round.
#' @param haste Logical. Haste active.
#' @param flurry Logical. Flurry of Blows active.
#' @param dualwield Logical. Dual Wielding active.
#' @param extra_apr Logical. Extra Attack Per Round active.
#' @param enemy_ac Numeric. The Armor Class of the target.
#'
#' @return Numeric. The total number of successful hits out of all d20 permutations
hits <- function(ab, base_apr, haste, flurry, dualwield, extra_apr, enemy_ac) {
  hit_rolls <- attacks_df(ab, base_apr, haste, flurry, dualwield, extra_apr) - enemy_ac
  
  # NWN Rule: Natural 1 always misses, Natural 20 always hits
  hit_rolls[1, ] <- -1
  hit_rolls[20, ] <- 1
  
  hits <- length(hit_rolls[hit_rolls >= 0])
  return(hits)
}

#' Calculate Expected Critical Hits
#' 
#' Determines the expected number of critical hits by calculating threat chance 
#' and multiplying it by the confirmation chance (which is equal to the hit chance).
#'
#' @param ab Numeric. Highest Attack Bonus.
#' @param base_apr Numeric. Base Attacks Per Round.
#' @param haste Logical. Haste active.
#' @param flurry Logical. Flurry of Blows active.
#' @param dualwield Logical. Dual Wielding active.
#' @param extra_apr Logical. extra_apr (+1 APR) active.
#' @param enemy_ac Numeric. The Armor Class of the target.
#' @param crit_range Numeric. The lowest number required to threaten a critical hit (e.g., 19 for 19-20).
#'
#' @return Numeric. The expected number of confirmed critical hits.
crits <- function(ab, base_apr, haste, flurry, dualwield, extra_apr, enemy_ac, crit_range) {
  crit_rolls <- attacks_df(ab, base_apr, haste, flurry, dualwield, extra_apr) - enemy_ac
  
  # Isolate rolls that fall within the critical threat range
  crits <- crit_rolls[crit_range:20, ]
  crits <- length(crits[crits >= 0])
  
  # Calculate base percentage chance to threaten
  crit_chance <- crits / (length(crit_rolls) * nrow(crit_rolls))
  
  # Multiply threat chance by total successful hits to get confirmed crits
  crits <- hits(ab, base_apr, haste, flurry, dualwield, extra_apr, enemy_ac) * crit_chance
  return(crits)
}

#' Calculate Average Damage Per Round (DPR)
#' 
#' Computes the total average damage per round by separating regular hits and critical hits,
#' applying base damage, critical multipliers, and precision damage (sneak attacks) appropriately.
#'
#' @param ab Numeric. Highest Attack Bonus.
#' @param base_apr Numeric. Base Attacks Per Round.
#' @param haste Logical. Haste active.
#' @param flurry Logical. Flurry of Blows active.
#' @param dualwield Logical. Dual Wielding active.
#' @param extra_apr Logical. extra_apr (+1 APR) active.
#' @param enemy_ac Numeric. The Armor Class of the target.
#' @param crit_range Numeric. Lowest d20 roll to threaten a crit.
#' @param crit_threat Numeric. The critical multiplier (e.g., 2 for x2, 3 for x3).
#' @param dmg_per_hit Numeric. Average base damage applied on every hit.
#' @param sneak_per_hit Numeric. Average precision/sneak attack damage applied on every hit (not multiplied on crits).
#'
#' @return Numeric. The average Damage Per Round against the specified AC.
damage <- function(ab, base_apr, haste, flurry, dualwield, extra_apr, enemy_ac, crit_range, crit_threat, dmg_per_hit, sneak_per_hit) {
  crits_val <- crits(ab, base_apr, haste, flurry, dualwield, extra_apr, enemy_ac, crit_range)
  hits_val <- hits(ab, base_apr, haste, flurry, dualwield, extra_apr, enemy_ac) - crits_val
  
  # Regular hits calculate base damage + sneak attack
  hits_damage <- hits_val * (dmg_per_hit + sneak_per_hit)
  
  # Crits multiply base damage, but do NOT multiply sneak attack damage
  crits_damage <- dmg_per_hit * crits_val * crit_threat
  
  # Divide by 20 to average out the 1:20 matrix permutations
  total_damage <- (hits_damage + crits_damage) / 20
  return(total_damage)
}

#' Calculate DPR Over an AC Sequence
#' 
#' A generic wrapper that iterates the `damage` function over a sequence of target Armor Classes.
#' Designed to consume a row of build data (e.g., from a Google Sheet) and an AC range.
#'
#' @param row_data A list or single-row data frame containing build parameters 
#'                 (ab, base_apr, haste, flurry, dualwield, extra_apr, crit_range, crit_threat, dmg_per_hit, sneak_per_hit).
#' @param ac_sequence A numeric vector of Armor Classes to test against (e.g., 30:40).
#'
#' @return A numeric vector representing the expected Damage Per Round for each AC in the sequence.
calculate_dpr_for_build <- function(row_data, ac_sequence) {
  sapply(ac_sequence, function(enemy_ac) {
    damage(
      ab = row_data$ab,
      base_apr = row_data$base_apr,
      haste = as.logical(row_data$haste),
      flurry = as.logical(row_data$flurry),
      dualwield = as.logical(row_data$dualwield),
      extra_apr = as.logical(row_data$extra_apr),
      enemy_ac = enemy_ac,
      crit_range = row_data$crit_range,
      crit_threat = row_data$crit_threat,
      dmg_per_hit = row_data$dmg_per_hit,
      sneak_per_hit = row_data$sneak_per_hit
    )
  })
}
