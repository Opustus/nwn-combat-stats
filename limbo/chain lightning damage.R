## Old chain lightning

oldchain = c()
for(targets in 1:20){
  dmg_out <- 120 + 60*6 + (targets-2)*7*30+30
  oldchain <- c(oldchain, dmg_out)
  if (targets == 1){
    oldchain[targets] <- 120  
  }
}
oldchain_othertarget = round((oldchain-120)/c(1:20),0)
oldchain_othertarget
oldchain_firsttarget = oldchain_othertarget+120
oldchain_firsttarget

newchain = c()
for(targets in 1:20){
  dmg_out <- 120 * 6
  newchain <- c(newchain, dmg_out)
  if (targets == 1){
    newchain[targets] <- 120
  }
}
newchain

newchain_target = newchain/c(1:20)
newchain_target[7:20] <- 0
newchain_target
targets

plot(oldchain, type = "l")
lines(oldchain_firsttarget, col = "blue")
lines(oldchain_othertarget, col = "red")
legend(1, 3900, legend=(c("Overall damage","First target", "Other targets")), col=c("black","blue","red"), lty=1, cex = 0.75)

?plot()
plot(x=1:20, oldchain, type = "l", ylim = c(0,500))
lines(oldchain_firsttarget, col = "blue")
lines(oldchain_othertarget[2:20], col = "red")
legend(11, 480, legend=(c("Overall damage","First target", "Other targets")), col=c("black","blue","red"), lty=1, cex = 0.75)

plot(newchain, type = "l", ylim = c(0,720))
lines(newchain_target, col = "blue")
legend(11, 480, legend=(c("Overall damage","Targets")), col=c("black","blue","red"), lty=1, cex = 0.75)

