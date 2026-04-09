level=c(1:30)

## Old damage
damage_old_min=1*trunc(level/2)
damage_old_min[1] <- 1
damage_old_min[26:30] <- damage_old_min[26:30]*1.5
damage_old_min[6:30] <-  damage_old_min[6:30]*2
damage_old_max=6*trunc(level/2)
damage_old_max[1] <- 6
damage_old_max[26:30] <- damage_old_max[26:30]*1.5
damage_old_max[6:30] <- damage_old_max[6:30]*2
damage_old_both=cbind(damage_old_min,damage_old_max)
damage_old_sd=apply(damage_old_both, 1, sd)
damage_old_average=(damage_old_min+damage_old_max)/2

## New with CHA
cha_mod=c(4,	4,	6,	6,	6,	6,	6,	7,	11,	11,	11,	11,	11,	11,	11,	12,	12,	12,	12,	12,	12,	12,	13,	13,	13,	13,	13,	14,	14,	14)
damage_new_min=1*level+cha_mod
damage_new_min[25:30] <- damage_new_min[25:30]*1.25
damage_new_min[6:30] <-  damage_new_min[6:30]*2
damage_new_max=2*level+cha_mod
damage_new_max[25:30] <- damage_new_max[25:30]*1.25
damage_new_max[6:30] <-  damage_new_max[6:30]*2
damage_new_both=cbind(damage_new_min,damage_new_max)
damage_new_sd=apply(damage_new_both, 1, sd)
damage_new_average=(damage_new_min+damage_new_max)/2

## New with flat 1/2
flat15=trunc(level/2)

damage_flat15_min=1*level+flat15
damage_flat15_min[25:30] <- damage_flat15_min[25:30]*1.25
damage_flat15_min[6:30] <-  damage_flat15_min[6:30]*2
damage_flat15_max=2*level+flat15
damage_flat15_max[25:30] <- damage_flat15_max[25:30]*1.25
damage_flat15_max[6:30] <-  damage_flat15_max[6:30]*2
damage_flat15_average=(damage_flat15_min+damage_flat15_max)/2
damage_flat15_both=cbind(damage_flat15_min,damage_flat15_max)
damage_flat15_sd=apply(damage_flat15_both, 1, sd)

## Matrix old and flat15
warlock_damages_new=data.frame(damage_flat15_average, damage_old_average)
colnames(warlock_damages_new) <- c("1d6 per 2 warlock", "1d2 per 1 warlock + 1 per warlock / 2")
matplot(level, ylim=c(0,200), warlock_damages_new, ylab="Average dmg per round", xlab="Level", type="l", lty="solid", col=c("darkgreen","red"), lwd=2.0)
legend(2, 190, legend=colnames(warlock_damages_new), lty=1, col=c("red","darkgreen"), cex=1)
arrows(level,damage_old_average-damage_old_sd/2,level,damage_old_average+damage_old_sd/2, code=3, length=0.02, angle = 90, col="red")
arrows(level,damage_flat15_average-damage_flat15_sd/2,level,damage_flat15_average+damage_flat15_sd/2, code=3, length=0.02, angle = 90, col="darkgreen")

plot(warlock_damages_new[,1], type="l", ylim=c(0,200), col="darkgreen", ylab="Average dmg per round", xlab="Level", lwd=2.0)
legend(2,190, lty=1, legend="1d2 per 1 warlock + 1 per warlock / 2", col="darkgreen", cex=1)
arrows(level,damage_flat15_average-damage_flat15_sd/2,level,damage_flat15_average+damage_flat15_sd/2, code=3, length=0.02, angle = 90, col="darkgreen")

plot(warlock_damages_new[,2], type="l", ylim=c(0,200), col="red", ylab="Average dmg per round", xlab="Level", lwd=2.0)
legend(2,190, lty=1, legend="1d6 per 2 warlock", col="red", cex=1)
arrows(level,damage_old_average-damage_old_sd/2,level,damage_old_average+damage_old_sd/2, code=3, length=0.02, angle = 90, col="red")


## Plots
plot(level,damage_new_average,type="l",col="red",ylim=c(0,200), ylab="dmg average / round")
lines(damage_old_average, col="blue")
arrows(level,damage_new_average-damage_new_sd/2,level,damage_new_average+damage_new_sd/2, code=3, length=0.02, angle = 90, col="red")
arrows(level,damage_old_average-damage_old_sd/2,level,damage_old_average+damage_old_sd/2, code=3, length=0.02, angle = 90, col="blue")
legend(2, 160, legend=c("old damage", "new damage"), lty=1, col=c("blue", "red")) 

plot(level,damage_old_average,type="l",col="blue",ylim=c(0,200), ylab="dmg average / round")
arrows(level,damage_old_average-damage_old_sd/2,level,damage_old_average+damage_old_sd/2, code=3, length=0.02, angle = 90, col="blue")
legend(2, 160, legend="old damage", lty=1, col="blue")


plot(level,damage_new_average,type="l",col="red",ylim=c(0,200), ylab="dmg average / round")
arrows(level,damage_new_average-damage_new_sd/2,level,damage_new_average+damage_new_sd/2, code=3, length=0.02, angle = 90, col="red")
legend(2, 160, legend="new damage", lty=1, col="red")


## Perilous flames and AoE stacking

AoE1=13.5+13.5
AoE2=AoE1+14+27*1.1
AoE3=AoE2+27*1.1+27*1.1
AoE4=AoE3+27*1.1+27*1.1
AoE5=AoE4+27*1.1+27*1.1
AoE6=AoE5+27*1.1+27*1.1
AoE7=AoE6+27*1.1+27*1.1
AoE8=AoE7+27*1.1
AoE9=AoE8
AoE10=AoE9

AoE=c(AoE1, AoE2, AoE3, AoE4, AoE5, AoE6, AoE7, AoE8, AoE9, AoE10)

## AoE stacking and Eldritch

eldritch_i = function(i){
  eldritch_i=damage_new_average[i]
  dmg <- AoE1 + 14 + eldritch_i
  return(dmg)
  }

eldritch_16thru30=eldritch_i(16:30)


e16=c(AoE1, rep(eldritch_16thru30[1],9))
e17=c(AoE1, rep(eldritch_16thru30[2],9))
e18=c(AoE1, rep(eldritch_16thru30[3],9))
e19=c(AoE1, rep(eldritch_16thru30[4],9))
e20=c(AoE1, rep(eldritch_16thru30[5],9))
e21=c(AoE1, rep(eldritch_16thru30[6],9))
e22=c(AoE1, rep(eldritch_16thru30[7],9))
e23=c(AoE1, rep(eldritch_16thru30[8],9))
e24=c(AoE1, rep(eldritch_16thru30[9],9))
e25=c(AoE1, rep(eldritch_16thru30[10],9))
e26=c(AoE1, rep(eldritch_16thru30[11],9))
e27=c(AoE1, rep(eldritch_16thru30[12],9))
e28=c(AoE1, rep(eldritch_16thru30[13],9))
e29=c(AoE1, rep(eldritch_16thru30[14],9))
e30=c(AoE1, rep(eldritch_16thru30[15],9))

rounds=1:10

?matplot
damage_rounds=data.frame(AoE, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27, e28, e29, e30)
matplot(rounds, damage_rounds, type="l", ylab="damage/round", xaxt="n")
axis(1, rounds)
legend(1, 380, legend=c("AoE stacking lv 16","long AoE+blast lv 16 thru 30"), lty=1, col=c("black","red"), cex=0.7)



## Damage modifiers
cha10=c(4,	4,	6,	6,	6,	6,	6,	7,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10)
cha8=c(4,	4,	6,	6,	6,	6,	6,	7,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8)
cha2=rep(2,30)
flat10=trunc(level/3)
flat15=trunc(level/2)

## cha10
damage_cha10_min=1*level+cha10
damage_cha10_min[21:30] <- damage_cha10_min[21:30]*1.25
damage_cha10_min[6:30] <-  damage_cha10_min[6:30]*2
damage_cha10_max=2*level+cha10
damage_cha10_max[21:30] <- damage_cha10_max[21:30]*1.25
damage_cha10_max[6:30] <-  damage_cha10_max[6:30]*2
damage_cha10_average=(damage_cha10_min+damage_cha10_max)/2

## cha8
damage_cha8_min=1*level+cha8
damage_cha8_min[21:30] <- damage_cha8_min[21:30]*1.25
damage_cha8_min[6:30] <-  damage_cha8_min[6:30]*2
damage_cha8_max=2*level+cha8
damage_cha8_max[21:30] <- damage_cha8_max[21:30]*1.25
damage_cha8_max[6:30] <-  damage_cha8_max[6:30]*2
damage_cha8_average=(damage_cha8_min+damage_cha8_max)/2

## cha2
damage_cha2_min=1*level+cha2
damage_cha2_min[21:30] <- damage_cha2_min[21:30]*1.25
damage_cha2_min[6:30] <-  damage_cha2_min[6:30]*2
damage_cha2_max=2*level+cha2
damage_cha2_max[21:30] <- damage_cha2_max[21:30]*1.25
damage_cha2_max[6:30] <-  damage_cha2_max[6:30]*2
damage_cha2_average=(damage_cha2_min+damage_cha2_max)/2

## flat10
damage_flat10_min=1*level+flat10
damage_flat10_min[21:30] <- damage_flat10_min[21:30]*1.25
damage_flat10_min[6:30] <-  damage_flat10_min[6:30]*2
damage_flat10_max=2*level+flat10
damage_flat10_max[21:30] <- damage_flat10_max[21:30]*1.25
damage_flat10_max[6:30] <-  damage_flat10_max[6:30]*2
damage_flat10_average=(damage_flat10_min+damage_flat10_max)/2

## flat15
damage_flat15_min=1*level+flat15
damage_flat15_min[21:30] <- damage_flat15_min[21:30]*1.25
damage_flat15_min[6:30] <-  damage_flat15_min[6:30]*2
damage_flat15_max=2*level+flat15
damage_flat15_max[21:30] <- damage_flat15_max[21:30]*1.25
damage_flat15_max[6:30] <-  damage_flat15_max[6:30]*2
damage_flat15_average=(damage_flat15_min+damage_flat15_max)/2

## matrix
warlock_damages=data.frame(damage_new_average, damage_cha10_average, damage_cha8_average, damage_cha2_average, damage_flat15_average, damage_flat10_average)
colnames(warlock_damages) <- c("Cha14", "Cha10", "Cha8", "Cha2", "Flat warlock / 2", "Flat warlock / 3")

nn=ncol(warlock_damages)
matplot(level,warlock_damages, type="l", ylab="dmg/round")
legend(1, 150, colnames(warlock_damages),col=seq_len(nn),cex=0.8,fill=seq_len(nn))

## some important levels
barplot(as.matrix(warlock_damages[16,]), ylim=c(0,150), las=2, main="level 16")
barplot(as.matrix(warlock_damages[21,]), ylim=c(0,150), las=2, main="level 21")
barplot(as.matrix(warlock_damages[26,]), ylim=c(0,150), las=2, main="level 26")
barplot(as.matrix(warlock_damages[30,]), ylim=c(0,150), las=2, main="level 30")
