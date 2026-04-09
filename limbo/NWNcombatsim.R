# Combat simulator for NWN
attack_roll <- function(apr)
{sample(c(1:20), size=apr, replace=T)}
d20 <- c(1:20)

attack_roll()

# COMBATER 1 variables

# BASIC

HP1 = 0
AC1 = 0
apr1 = 4
AB1 = seq(from=50, to=50-(apr1-1)*5, by=-5)
dmg_person1 = 0              # ignores all variables, always stacks with self


crit_range1 = 18
crit_threat1 = 2

# DAMAGE

# physical
slash1 = c(1:12)
#slash1_calc=c(1:12)*(1-slash_immune2)                     # calculation
#slash1=ceiling(slash1_calc[slash1_calc>=0])                # damage vector

pierce1=0
#pierce1_calc=c()*(1-pierce_immune2)-pierce_resist2
#pierce1=ceiling(pierce1_calc[pierce1_calc>=0])

bludge1=0

# elemental
fire1=c(1:8)


electric1=0
cold1=0
acid1=0
# energy
sonic1=0
pos1=0
neg1=0
divine1=0
magic1=0


# COMBATER 2 variables

AC2 = 60
concealment2 = 0.5


slash_immune2=0
slash_resist2=5

fire_resist2=5

HP2 = 100


# COMBATER 1 functions

AB1[4]

attack1 <- function(apr = apr1, AB = AB1, AC = AC2, crit_range = crit_range1, crit_threat = crit_threat1, dmg_weapon = dmg_weapon1, dmg_person = dmg_person1)
                  {
                  rolls = attack_roll(apr)
                  hit_rolls = rolls + AB
                  hits = hit_rolls[rolls > 1 & hit_rolls >= AC | rolls == 20]
                  crits = hit_rolls[rolls > 1 & hit_rolls >= AC & rolls >= crit_range]
              
                  crit_rolls = sample(d20, size = length(crits), replace = T) + AB[rolls > 1 & hit_rolls >= AC & rolls >= crit_range]
                  crit_hits = crit_rolls[crit_rolls >= AC]
                  crit_dmg = crit_threat * sum(sample(x = dmg_weapon, size = length(crit_hits))) + dmg_person*length(crit_hits)
                
                  hit_dmg = sum(sample(x = dmg_weapon, size = length(hits) - length(crit_hits))) + dmg_person*(length(hits) - length(crit_hits))
                  
                  dmg = crit_dmg + hit_dmg 
                  return(dmg)
                  }
attack1()


sample(d20, size = 2) + AB1

dead2 <- function(){
  rounds <- 0
  repeat {
    rounds <- rounds+1
    HP2 <- HP2-attack1()
    if (HP2 <= 0) {
      print(rounds)
      break
    }
  }
}


dead2()

attack1()




