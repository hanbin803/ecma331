set.seed(31130)

p  = list(gamma = 0.8,beta=1,a=1,rho=1,eta=0.2,delta=-0.2,delta0=-0.1,nu=0.5) # parameters
N=10000  # size of the simulation
simdata = data.table(i=1:N,X=rnorm(N))

# simulating variables
simdata[,X := rnorm(N)]
simdata[,Z := rnorm(N)]
simdata[,u := rnorm(N)]
simdata[,lw := p$eta*X  + Z + 0.2*u ]  # log wage

simdata[,xi := rnorm(N)*0.2]
simdata[,lr := lw + p$delta0+ p$delta*Z + xi]; # log home productivity

simdata[,eps:=rnorm(N)*0.2]
simdata[,beta := exp(p$nu*X  + p$a*xi + eps)]; # heterogenous beta coefficient

# compute decision variables
simdata[, lfp := log(p$rho) + lw >= lr] # labor force participation
simdata[, h   := (p$rho * exp(lw)/beta)^(1/p$gamma)] # hours
simdata[lfp==FALSE,h:=NA][lfp==FALSE,lw:=NA]
simdata[,mean(lfp)]


q  = list(gamma = 0.8,beta=1,a=0,rho=1,eta=0.2,delta=-0.2,delta0=-0.1,nu=0.5) #
N=10000  # size of the simulation
simdataq = data.table(i=1:N,X=rnorm(N))

# simulating variables
simdataq[,X := rnorm(N)]
simdataq[,Z := rnorm(N)]
simdataq[,u := rnorm(N)]
simdataq[,lw := q$eta*X  + Z + 0.2*u ]  # log wage

simdataq[,xi := rnorm(N)*0.2]
simdataq[,lr := lw + q$delta0+ q$delta*Z + xi]; # log home productivity

simdataq[,eps:=rnorm(N)*0.2]
simdataq[,beta := exp(q$nu*X  + q$a*xi + eps)]; # heterogeneous beta coefficient

# compute decision variables
simdataq[, lfp := log(q$rho) + lw >= lr] # labor force participation
simdataq[, h   := (q$rho * exp(lw)/beta)^(1/p$gamma)] # hours
simdataq[lfp==FALSE,h:=NA][lfp==FALSE,lw:=NA]
simdataq[,mean(lfp)]

pander(summary(simdata[,lm(log(h) ~ lw + X)]))
pander(summary(simdataq[,lm(log(h) ~ lw + X)]))


#Question 3
fit2 = glm(lfp ~ Z,simdata,family = binomial(link = "probit"))
#simdata is when a=1

pander(summary(fit2))
delta0 = fit2$coefficients[1]
delta = fit2$coefficients[2]

simdata[,b := delta0 + delta*Z ]

sig = sqrt(var(simdata$xi)*(N-1)/N)

simdata[,m := sig*(dnorm(b, mean=0, sd=1) / (1 - pnorm(b, mean=0, sd=1))]

pander(summary(simdata[,lm(log(h) ~ lw + X + m)]))



##Question 5

simulate <- function(p,N,t) {
  simdata = data.table(i=1:N,X=rnorm(N))
  
  # simulating variables
  simdata[,X := rnorm(N)]
  simdata[,Z := rnorm(N)]
  simdata[,u := rnorm(N)]
  simdata[,lw := p$eta*X + Z + 0.2*u ]  # log wage
  
  simdata[,xi := -rexp(N)]
  simdata[,lr := lw + p$delta0 + p$delta*Z + xi]; # log home productivity
  
  simdata[,eps:=rnorm(N)*0.2]
  simdata[,beta := exp(p$nu*X  + p$a*xi +  eps)]; # heterogenous beta coefficient
  
  # compute decision variables
  simdata[, lfp := log(p$rho) + lw >= lr] # labor force participation
  simdata[, h   := (p$rho * exp(lw)/beta)^(1/p$gamma)] # hours
  
  # make hours and wages unobserved in case individual doesn't work
  simdata[lfp==FALSE,h:=NA][lfp==FALSE,lw:=NA]
  simdata[,mean(lfp)]
  
  # store time
  simdata[, t := t]
  return(simdata)
}

p  = list(gamma = 0.8,beta=1,a=1,rho=1,eta=0.2,delta=-1.0,delta0=-0.1,nu=0.5) # parameters
N=50000  # size of the simulation

# simulate period 1 
sim1  = simulate(p,N,1); 
p$eta = 0.4; p$delta0 = -1.1
# simulate period 2 with different eta and delta0 (including intercept shifts and variation in wages)
sim2 = simulate(p,N,2); 
simdata = rbind(sim1,sim2) # combine the two periods


pander(summary(simdata[,lm(log(h) ~ lw + X)]))



fit2 = glm(lfp ~ Z,simdata,family = binomial(link = "probit"))

pander(summary(fit2))
delta0 = fit2$coefficients[1]
delta = fit2$coefficients[2]

simdata[, b := -delta0 - delta*Z ]
simdata[, m := 1 - (dnorm(b, mean=1, sd=1) / pnorm(b, mean=1, sd=1))]
View(simdata)



pander(summary(simdata[,lm(log(h) ~ lw + X + m)]))
