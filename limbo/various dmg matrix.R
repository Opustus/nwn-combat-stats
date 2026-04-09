# Expected value of damage per round for various weapons!

roll20 = 1:20

attacks = function(ab, apr, rapid){
  if (rapid == T){
    ab = ab - 2}
  seq(from = ab, to = ab-apr*4, by = -5)
}

attacks_prob = function(ab, apr, rapid){
  c1 <- cbind(attacks(ab, apr, rapid)[1]+roll20)
  c2 <- cbind(attacks(ab, apr, rapid)[2]+roll20)
  c3 <- cbind(attacks(ab, apr, rapid)[3]+roll20)
  c4 <- cbind(attacks(ab, apr, rapid)[4]+roll20)
  attacks_matrix = cbind(c1,c2,c3,c4)
  if (rapid == T){
    c5 <- cbind(attacks(ab, apr, rapid)[5]+roll20)
    attacks_matrix = cbind(c1,c1,c2,c3,c4)
    return(attacks_matrix)
  }
  return(attacks_matrix)
}


hits = function(ab, apr, rapid, ac){
  hits_matrix = attacks_prob(ab, apr, rapid) - ac
  hits_matrix[1,] <- -1
  hits_matrix[20,] <- 1
  hits = length(hits_matrix[hits_matrix >= 0])
  return(hits)
}

crits = function(ab, apr, rapid, ac, crit_range){
  hits(ab,apr,rapid,ac) * crit_range
}

damage = function(ab, apr, rapid, ac, crit_range, crit_threat, damage){
  crits = crits(ab, apr, rapid, ac, crit_range)
  hits = hits(ab, apr, rapid, ac)
  hits = hits - crits
  hits_damage = hits * damage
  crits_damage = crits * damage * crit_threat
  damage = (hits_damage + crits_damage) / 20
  return(damage)
}

crossbow_no_rapid = c()
for(ac in 30:70){
  out <- damage(50, 4, F, ac, 0.3, 2, 10)
  crossbow_no_rapid <- c(crossbow_no_rapid, out)
}

crossbow_rapid = c()
for(ac in 30:70){
  out <- damage(50, 4, T, ac, 0.3, 2, 10)
  crossbow_rapid <- c(crossbow_rapid, out)
}

bow_no_rapid = c()
for(ac in 30:70){
  out <- damage(50, 4, F, ac, 0.1, 3, 10)
  bow_no_rapid <- c(bow_no_rapid, out)
}

bow_rapid = c()
for(ac in 30:70){
  out <- damage(50, 4, T, ac, 0.1, 3, 10)
  bow_rapid <- c(bow_rapid, out)
}

bow_matrix = cbind(crossbow_no_rapid, crossbow_rapid, bow_no_rapid, bow_rapid)
colnames(bow_matrix) <- c("Crossbow, no rapid", "Crossbow, rapid", "Bow, no rapid", "Bow, rapid")


matplot(bow_matrix,  type="l", lwd = 3.0, ylab = "Dmg/round, AB 50", xlab = "Enemy AC", xaxt='n')
axis(side=1, at = 0:40, 30:70)
nn <- ncol(bow_matrix)
legend("topright", colnames(bow_matrix),col=seq_len(nn),cex=0.8,fill=seq_len(nn))

# NEW GRAPH

bow_no_rapid = c()
for(ac in 30:70){
  out <- damage(50, 4, F, ac, 0.1, 3, 16.5)
  bow_no_rapid <- c(bow_no_rapid, out)
}

bow_rapid = c()
for(ac in 30:70){
  out <- damage(50, 4, T, ac, 0.1, 3, 16.5)
  bow_rapid <- c(bow_rapid, out)
}

axe_no_rapid = c()
for(ac in 30:70){
  out <- damage(50, 4, F, ac, 0.15, 2, 19)
  axe_no_rapid <- c(axe_no_rapid, out)
}

axe_rapid = c()
for(ac in 30:70){
  out <- damage(50, 4, T, ac, 0.15, 2, 19)
  axe_rapid <- c(axe_rapid, out)
}

throw_matrix = cbind(axe_no_rapid, axe_rapid, bow_no_rapid, bow_rapid)
colnames(bow_matrix) <- c("Axe, no rapid", "Axe, rapid", "Bow, no rapid", "Bow, rapid")

matplot(throw_matrix,  type="l", lwd = 3.0, ylab = "Dmg/round, AB 50", xlab = "Enemy AC", xaxt='n')
axis(side=1, at = 0:40, 30:70)
nn <- ncol(bow_matrix)
legend("topright", colnames(bow_matrix),col=seq_len(nn),fill=seq_len(nn))


## MELEE WEAPONS

scimmy = c()
for(ac in 50:70){
  out <- damage(50, 4, T, ac, 0.55, 3, 51)
  scimmy <- c(scimmy, out)
}

flail = c()
for(ac in 50:70){
  out <- damage(50, 4, T, ac, 0.4, 3, 57)
  flail <- c(flail, out)
}

scythe = c()
for(ac in 50:70){
  out <- damage(50, 4, T, ac, 0.25, 5, 55)
  scythe <- c(scythe, out)
}

axe1 = c()
for(ac in 50:70){
  out <- damage(50, 4, T, ac, 0.25, 4, 53)
  axe1 <- c(axe1, out)
}

axe2 = c()
for(ac in 50:70){
  out <- damage(50, 4, T, ac, 0.25, 4, 56)
  axe2 <- c(axe2, out)
}


weapon_matrix = cbind(axe1, axe2, flail, scimmy, scythe)
colnames(weapon_matrix) <- c("axe1", "axe2", "flail", "scimmy", "scythe")

matplot(weapon_matrix,  type="l", lwd = 3.0, ylab = "Dmg/round, AB 50", xlab = "Enemy AC", xaxt='n')
axis(side=1, at = 0:20, 50:70)
nn <- ncol(weapon_matrix)
legend("topright", colnames(weapon_matrix),col=seq_len(nn),fill=seq_len(nn))
