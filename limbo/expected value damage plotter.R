# Expected value of damage per round for various weapons!

roll20 = 1:20

attacks = function(ab, apr, rapid, haste){
  if (rapid == T)
  {
    ab = ab - 2
  }
  seq(from = ab, to = ab-apr*4, by = -5)
}

attacks_prob = function(ab, apr, rapid, haste){
  c1 <- cbind(attacks(ab, apr, rapid)[1]+roll20)
  c2 <- cbind(attacks(ab, apr, rapid)[2]+roll20)
  c3 <- cbind(attacks(ab, apr, rapid)[3]+roll20)
  c4 <- cbind(attacks(ab, apr, rapid)[4]+roll20)
  attacks_matrix = cbind(c1,c2,c3,c4)
  if (rapid == T || haste == T)
  {
    attacks_matrix = cbind(c1,c1,c2,c3,c4)
  }
  if (haste == T && rapid == T)
  {
    attacks_matrix = cbind(c1,c1,c1,c2,c3,c4)
  }
  return(attacks_matrix)
}


hits = function(ab, apr, rapid, haste, ac)
{
  hits_matrix = attacks_prob(ab, apr, rapid, haste) - ac
  hits_matrix[1,] <- -1
  hits_matrix[20,] <- 1
  hits = length(hits_matrix[hits_matrix >= 0])
  return(hits)
}

crits = function(ab, apr, rapid, haste, ac, crit_range)
{
  hits(ab,apr,rapid,haste,ac) * crit_range
}

damage = function(ab, apr, rapid, haste, ac, crit_range, crit_threat, damage, masscrit, sneak)
{
  crits = crits(ab, apr, rapid, haste, ac, crit_range)
  hits = hits(ab, apr, rapid, haste, ac)
  hits = hits - crits
  hits_damage = hits * (damage + sneak)
  crits_damage = crits * masscrit + damage * crits * crit_threat
  damage = (hits_damage + crits_damage) / 20
  return(damage)
}


weapon_matrix = cbind(axe1, axe2, flail, scimmy, scythe)
colnames(weapon_matrix) <- c("axe1", "axe2", "flail", "scimmy", "scythe")

matplot(weapon_matrix,  type="l", lwd = 3.0, ylab = "Dmg/round, AB 50", xlab = "Enemy AC", xaxt='n')
axis(side=1, at = 0:20, 50:70)
nn <- ncol(weapon_matrix)
legend("topright", colnames(weapon_matrix),col=seq_len(nn),fill=seq_len(nn))

dmg = 4.5 + 6 + 2 + 19 + 3 + 11

pidge_sneak = c()
for(ac in 40:80){
  out <- damage(68, 4, F, T, ac, 0.3, 2, dmg, 0, 7)
  pidge_sneak <- c(pidge_sneak, out)
}
pidge_masscrit = c()
for(ac in 40:80){
  out <- damage(68, 4, F, T, ac, 0.3, 2, dmg, 13, 0)
  pidge_masscrit <- c(pidge_masscrit, out)
}

pidge_matrix = cbind(pidge_sneak, pidge_masscrit)


matplot(pidge_matrix,  type="l", lwd = 3.0, ylab = "Dmg/round, AB 68, dmg 47.5", xlab = "Enemy AC", xaxt='n')
axis(side=1, at = 0:40, 40:80)
nn <- ncol(pidge_matrix)
legend("topright", colnames(pidge_matrix),col=seq_len(nn),fill=seq_len(nn))
