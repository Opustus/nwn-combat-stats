# This is an attack simulator! It currently works for 4 APR enemy only.
# It calculates the chance of being hit per attack.

combat_sim <- function(
  epic_dodge, #         accepts T or F, checks for player's epic dodge, default F
  concealment, #        accepts T or F, checks for player's concealment, default F
  parry, #              accepts any integer, sets player parry
  ac, #                 accepts any integer, sets player AC
  enemy_ab #            accepts any integer, sets enemy AB
){
  # setting defaults and error messages
  if(missing(epic_dodge)){print("epic_dodge set to F.") 
    epic_dodge = F }
  if(missing(concealment)){print("concealment set to F.") 
    concealment = F}
  if(missing(parry)){print("parry set to 0.") 
    parry = 0}
  
  if(missing(ac)){print("Give integer for ac.")}
  if(missing(enemy_ab)){print("Give integer for enemy_ab.")}
  
  # setting variables and a function for attack rolls
  A1 <- enemy_ab + 1:20
  A2 <- enemy_ab - 5 + 1:20
  A3 <- enemy_ab - 10 + 1:20
  A4 <- enemy_ab - 15 + 1:20
  
  a_roll=function(x){
    sample(x,10000,replace=T)
  }
  
  # this creates 100 attack rolls for each 4 attacks
  attack_rolls <- cbind(a_roll(A1), a_roll(A2), a_roll(A3), a_roll(A4))
  
  # this creates 4*100 parry rolls to match the 4*100 attack rolls
  parry <- parry + 1:20
  parry_rolls <- sample(parry,40000,replace=T)
  parry_rolls <- cbind(parry_rolls[1:10000], parry_rolls[10001:20000], parry_rolls[20001:30000], parry_rolls[30001:40000])
  
  # this calculates the mean of parry failing per attack, T == hit
  parry_fails <- parry_rolls < attack_rolls
  
  # this calculates the mean of AC failing per attack
  ac_fails <- ac  <= attack_rolls
  
  # hits coming through, T == hit succeeds
  hits <- ac_fails & parry_fails
  
  # assigns nat20s to hit
  hits[,1][attack_rolls[,1] == enemy_ab + 20] <- T
  hits[,2][attack_rolls[,2] == enemy_ab + 15] <- T
  hits[,3][attack_rolls[,3] == enemy_ab + 10] <- T
  hits[,4][attack_rolls[,4] == enemy_ab + 5] <- T
  
  # assigns nat1s to miss
  hits[,1][attack_rolls[,1] == enemy_ab + 1] <- F
  hits[,2][attack_rolls[,2] == enemy_ab - 4] <- F
  hits[,3][attack_rolls[,3] == enemy_ab - 9] <- F
  hits[,4][attack_rolls[,4] == enemy_ab - 14] <- F
  
  # number of hits
  nhits <- length(hits[hits==T])
  
  if (epic_dodge == T){
    # updates number of hits to minus an attack per round when hit
    nhits <- nhits - length(rowSums(hits)[rowSums(hits) == T] > 0L)
  }
  
  hit_chance <- round(nhits/length(hits), 2)
  
  if (concealment == T){
    # the chance of getting hit without concealment or epic dodge
    hit_chance <- hit_chance * 0.75
  } 
  # outputs the chance of being hit with attack post conditions
  print(paste(hit_chance*100, "% chance to be hit per attack."))
  
  if (parry[1] > 20) {
    # checks for number of riposte
    nriposte <- length(hits[parry_rolls >= attack_rolls + 10 & hits == F])
    riposte_chance <- round(nriposte/length(parry_rolls),2)
    print(paste(riposte_chance*100, "% chance to riposte per attack."))
  }
  print(paste(hit_chance^2*100*3, "% chance to be hit twice per round"))
}


combat_sim(ac=78, parry=0, enemy_ab=58, epic_dodge = F, concealment = F)

#### TEST TEST TEST TEST ####
