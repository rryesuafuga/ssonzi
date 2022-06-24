

# Yamane (1967)

ss = function(N=1650800, e){
  N/(1 + N * (e)^2 )
}

ss(e=0.05)



# Cochran (1963) with finite population correction

ss2 = function(N=1650800, e=0.05, z=1.96, p){
  raw = ( z^2 * p * (1-p) ) / ( e^2)
  corrected = raw / (1 + (raw-1)/N )
  return(corrected)
}

ss2(p=0.4)



# Power of over 99 %

## Method 1
power.prop.test(n=369, p1=0.4, p2=0.6, alternative = "two.sided")

power.prop.test(p1=0.4, p2=0.6, alternative = "two.sided", power = 0.997)



## Method 2

# Power calculations for proportion tests (one sample)
pwr::pwr.p.test(h=0.2,power=0.97,sig.level=0.05,alternative="two.sided")



# Power calculation for two proportions (same sample sizes)
pwr::pwr.2p.test(h=0.2,power=0.97,sig.level=0.05,alternative="two.sided")


# Power calculation for two proportions (different sample sizes)
pwr::pwr.2p2n.test(h=0.2, n1=400, power=0.97 ,sig.level=0.05,
                   alternative="two.sided")

