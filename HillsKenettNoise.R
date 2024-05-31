# Code for Hills & Kenett, An Entropy Modulation Theory of Creativity

rm(list=ls()) # for good health
#### Necessary packages

library(igraph)
library(scales)
library(beeswarm)
library(plot.matrix)

#### Figure 1 ####

par(mar=c(5,5,2,2))
x <- 0:4
y =  hc = 60*exp(-x)
hp = 100*exp(-x) 
hf = 50*exp(-x/6)
lc = 40*exp(-x/6)
# Save plot of theory visualization
pdf("Figure1.pdf")
plot(x,hc, type = "l", ylab = "Associative strength", xaxt="n", yaxt="n", cex.lab = 1.4, xlab="Concepts ranked by associative strength", lty=2, ylim=c(0,100), col="gray80", lwd = 7)
lines(x, lc, lty=1, lwd=7, col="gray80")
lines(x, hp, lty=3, lwd=3)
lines(x, hf, lty=4, lwd=3)
##lines(x, y4, lty=4, lwd=2)
axis(1, at = seq(0, 4, by = 1), labels = c("1", "2", "3", "4", "5") )
axis(2, at = c(0, 100), labels = c("Low", "High"), las =1 )
legend(2,90, legend=c("Lower - Mednick","Higher - Mednick", "Faster-but-proportional", "Faster-and-further"),title="Creativity", lty=c(2, 1, 3, 4), lwd=c(7,7,3, 3), col = c("gray80", "gray80", "black", "black"), cex = 1, bty="n")
dev.off()

#### Figure 2 ####

# Build representation and compute distances from 20 randomly chosen cues

# set seed for reproducibility
rm(list=ls())
set.seed(1)

# Number of nodes
N = 500
# Adjust the above to increase or reduce the network size

# Make the cognitive representation

## Preferential attachment, alpha = 1, m = 2 (used in the article)
eg <- sample_pa(N, 1, m=2, directed = FALSE)

## Erdös-Renyi (uncomment the below to use an Erdös-Renyi random graph)
#eg <- sample_gnp(N, .005)

# IMPORTANT: Whatever representation is used
# Take only nodes in giant component of 'eg', so all nodes are reachable
V(eg)$membership <- components(eg)$membership
eg <- subgraph(eg, which(V(eg)$membership == 1))

# Compute distances from node 20 randomly chosen nodes in the reachable component

# Number of cues (adjust to alter number of cues)
Cu =20 

# Number of possible cues from above representation
N = length(V(eg))

# Sample Cues
noCu <- sample(1:N, Cu)

# Compute distances
distan <- c()
for(i in noCu){
  # Get all distances for each cue
  dd <- igraph::distances(eg, i)
  # Order by distance, closest first
  dd<-dd[base::order(dd)]
  # Add distance to row for cues
  distan <- rbind(distan, dd)
}

dd <- colMeans(distan, na.rm=TRUE)

# Save plot network and similarities
pdf("Figure2.pdf", height = 5, width = 8)

# Plot parameters
par(mar=c(5,5,2,2))
par(mfrow=c(1,2))

# Set node sizes for visualization
V(eg)$size = 3
V(eg)$size[noCu] <-6 
V(eg)$color <- "gray20"
V(eg)$color[noCu] <- "tomato"
# Set image seed
set.seed(11)
# Plot network
plot(eg, vertex.label = NA, edge.width = .5, vertex.frame.width=.3, layout=layout_with_kk(eg), edge.color=alpha("grey20",.3) )

# Plot similarity (1/d)
plot(1/dd, xlab = "Rank", ylab = "Similarity (1/distance)", cex = .4, log="xy", col=alpha("gray20", .6), pch=16)

dev.off()

#### Figure 3 In-silico ####
# Function for noisy activation model
word_race_nam <- function(probs,noise = 0,noisy_mean=0, noisy_sd = 0, threshold=5){ 
  # Probs are similarities
  xx = 1:length(probs)
  # Set time to 0
  time <- 0
  # Initialize activation values
  activation_values <- rep(0, length(probs)+1)
  # Until threshold is reached
  while(max(activation_values)<threshold){
    # Sample a target in proportion to similarity
    samp <- sample(1:length(probs), 1, prob=probs)
    # Add activation with variance to sampled value 
    activation_values[samp] <-  activation_values[samp] + 1 + rnorm(1, mean =noisy_mean, sd=noisy_sd)
    # Add atime unit
    time <- time + 1
  }
  # Identify word that exceeded threshold
  chosen_one = xx[activation_values == max(activation_values)]
  # Return the chosen one with the time of retrieval
  return(data.frame(word = chosen_one, time))
}

entropy <- function(x){
  valuei <- 0
  for(ij in 1:length(x)){
    valuei <- valuei + x[ij]*log(x[ij]) 
  }
  valuei <- -valuei
  return(valuei)
}

# Setup simulation
# Number of retrievals per cue
sims = 500
# Data buffers
time_keeper_high <-c()
time_keeper_low <-c()
assocStr_high <- c()
assocStr_low <- c()
for(i in 1:Cu){
  # Get distances for Cu i
  probs = 1/distan[i,][-1]
  # Data buffers
  results_low  = c()
  results_high = c()
  # Ask for sims retrievals for each cue
  for(k in 1:sims){
    # get retrievals for high and low
    results_low <- rbind(results_low, word_race_nam(probs, noisy_mean = 0, noisy_sd = .5))
    results_high <- rbind(results_high, word_race_nam(probs, noisy_mean = 0, noisy_sd = 1  ) )
    # Keep track of simulation
    print(paste("sim ", k))
  }
 # Get median number of iterations required to retrieve response for this cue
 time_keeper_high <- c(time_keeper_high, median(results_high[,2])) 
 time_keeper_low <- c(time_keeper_low, median(results_low[,2])) 
 # Get number of retrievals for each word
 xlow<-with(results_low, tapply(word, word, length))
 xhigh <-with(results_high, tapply(word, word, length))
 # Order them 
 xlow <- xlow[order(xlow, decreasing=TRUE)]
 xhigh <- xhigh[order(xhigh, decreasing=TRUE)]
 # Normalize by total number of retrievals to get association strengths
 xlow <- xlow/sum(xlow)
 xhigh <- xhigh/sum(xhigh)
 # Keep first approximately half of most frequently retrieved words, to compile 
 xlow <- xlow[1:(sims/2)]
 xhigh <- xhigh[1:(sims/2)]
 # Compile
 assocStr_high <- rbind(assocStr_high, xhigh)
 assocStr_low <- rbind(assocStr_low, xlow)
}

#### Figure 3 ####

#save(assocStr_high,assocStr_low, time_keeper_high,time_keeper_low, file = "Figure3.RData")
#load("Figure3.RData")
pdf("Figure3_NoisyActivationOnlineCode.pdf", height=4.5, width = 7)
par(mfrow=c(1,2))
par(mar=c(5,5,2,2))
nf <- layout( matrix(c(1,1,1,2,2), nrow=1) )
# Limit figure to top 200 targets
targets =100 
# Get mean of association strengths
x1 <- colMeans(assocStr_low)
x2 <- colMeans(assocStr_high)
Elow <- entropy(x1[!is.na(x1)])
Ehigh <- entropy(x2[!is.na(x2)])
# Plot low creative line
plot(x1[1:targets], xlim=c(0, targets), pch=16, xlab = "Rank", ylab= "Association strength", log = "y", yaxt="n", cex = .4, cex.lab=1.4, col=alpha("black", 1), ylim =c(.001, .1))
axis(2, at = c(.0001,.001, .01, .1), labels = c(".0001",".001",".01", ".1"), las =1 )
lines(x1[1:targets], lwd = 2, col=alpha("black", 1))
# Plot high creative line
points(x2[1:targets], col=alpha("gray80", 1), pch = 16, cex = .4)
lines(x2[1:targets], col=alpha("gray80", 1), lwd = 2)
legend(50, .08, legend=c("High (1.0)", "Low (0.5)"), col=c("gray80", "black"), bty="n", lty=1, lwd=2, title = TeX("Noise ($\\sigma$)"), cex = 1.4)
# Boxplot of inter-item retrieval times
#boxplot(data.frame(time_keeper_low, time_keeper_high), names=c("Low", "High"), ylab = "Relative Inter-item Retrieval Time", boxwex= c(.6,.6), cex = .2)
#stripchart(data.frame("Low"=time_keeper_low, "High" =time_keeper_high), vertical=TRUE, method = "jitter", pch = 16, cex = .6, ylab="Response time", ylim = c(100, 300), col=adjustcolor( "black", alpha.f = 0.5), jitter = .3, cex.lab=1.3, xlab=TeX("Noise ($\\sigma$)"))

dtf <- data.frame(y = c(time_keeper_low, time_keeper_high), x = c(rep(1, length(time_keeper_low)), rep(2, length(time_keeper_high))))
opar <- par(lwd = .8)
boxplot(data.frame(time_keeper_low, time_keeper_high), names=c("Low", "High"), ylab = "Relative Inter-item Retrieval Time", boxwex= c(.6,.6), cex = .2, col=alpha("grey80", 0), border = "orange", xlab=TeX("Noise ($\\sigma$)"), cex.lab=1.4, ylim = c(100, 300))
with(dtf, beeswarm(y~x, pch = 16, cex = .6, add=T,
        col=c(rep(alpha("gray80", .7), length(10)), 
        rep(rep(alpha("black", .5), length(time_keeper_high))))))
dev.off()

# Figure 4 simulation


# Function for noisy activation model
word_race_nam <- function(probs,noise = 0,noisy_mean=0, noisy_sd = 0, threshold=5){ 
  # Probs are similarities
  xx = 1:length(probs)
  # Set time to 0
  time <- 0
  # Initialize activation values
  activation_values <- rep(0, length(probs)+1)
  # Until threshold is reached
  while(max(activation_values)<threshold){
    # Sample a target in proportion to similarity
    samp <- sample(1:length(probs), 1, prob=probs)
    # Add activation with variance to sampled value 
    activation_values[samp] <-  activation_values[samp] + 1 + rnorm(1, mean =noisy_mean, sd=noisy_sd)
    # Add atime unit
    time <- time + 1
  }
  # Identify word that exceeded threshold
  chosen_one = xx[activation_values == max(activation_values)]
  # Return the chosen one with the time of retrieval
  return(data.frame(word = chosen_one, time))
}

entropy <- function(x){
  valuei <- 0
  for(ij in 1:length(x)){
   valuei <- valuei + x[ij]*log(x[ij]) 
  }
  valuei <- -valuei
  return(valuei)
}

sigs <- seq(0, 1, .1)
ths <- seq(1,10,1)
Cues = 20
time_matrix <- matrix(NA, ncol=length(sigs), nrow=length(ths))
entropy_matrix <- matrix(NA, ncol=length(sigs), nrow=length(ths))
# Do for each threshold
for(th in 1:length(ths)){
  # Do for each noisy sig
  for(sigsi in 1:length(sigs)){
    # Setup simulation
    # Number of retrievals per cue
    sims = 500 
    # Data buffers
    time_keeper <-c()
    entropy_record <- c()
    for(ci in 1:Cu){
      # Get distances for Cu i
      probs = 1/distan[ci,][-1]
      # Data buffers
      results  = c()
      # Ask for sims retrievals for each cue
      for(si in 1:sims){
        # get retrievals for high and low
        results <- rbind(results, word_race_nam(probs, noisy_mean = 0, noisy_sd = sigs[sigsi], threshold = ths[th]))
        # Keep track of simulation
        print(paste("sim ", si))
      }
      # Get median number of iterations required to retrieve response for this cue
      time_keeper <- c(time_keeper, median(results[,2])) 
      # Get number of retrievals for each word
      xlow<-with(results, tapply(word, word, length))
      # Order them 
      xlow <- xlow[order(xlow, decreasing=TRUE)]
      # Normalize by total number of retrievals to get association strengths
      xlow <- xlow/sum(xlow)
      entropy_record <- rbind(entropy_record, entropy(xlow))
    }
    time_matrix[th,sigsi] <- mean(time_keeper)
    entropy_matrix[th,sigsi] <- mean(entropy_record)
  }
}


 cor.test(as.vector(entropy_matrix), as.vector(time_matrix))
 

## Figure 4 second part

# Function for noisy activation model
word_race_nam <- function(probs,noise = 0,noisy_mean=0, noisy_sd = 0, threshold=5, Ab=1){ 
  # Probs are similarities
  xx = 1:length(probs)
  # Set time to 0
  time <- 0
  # Initialize activation values
  activation_values <- rep(0, length(probs)+1)
  # Until threshold is reached
  while(max(activation_values)<threshold){
    # Sample a target in proportion to similarity
    samp <- sample(1:length(probs), 1, prob=probs)
    # Add activation with variance to sampled value 
    activation_values[samp] <-  activation_values[samp] + Ab + rnorm(1, mean =noisy_mean, sd=noisy_sd)
    # Add atime unit
    time <- time + 1
  }
  # Identify word that exceeded threshold
  chosen_one = xx[activation_values == max(activation_values)]
  # Return the chosen one with the time of retrieval
  return(data.frame(word = chosen_one, time))
}

entropy <- function(x){
  valuei <- 0
  for(ij in 1:length(x)){
    valuei <- valuei + x[ij]*log(x[ij]) 
  }
  valuei <- -valuei
  return(valuei)
}


sigs <- seq(0, 1, .1)
ths <- 5 
Ab <- seq(.1, 1, .1)
Cues = 20
dataoutmonk <- c()
# Do for each threshold
for(th in 1:length(ths)){
  # Do for each noisy sig
  for(sigsi in 1:length(sigs)){
    # Setup simulation
    for(Ais in 1:length(Ab)){
      # Number of retrievals per cue
      sims = 50
      # Data buffers
      time_keeper <-c()
      entropy_record <- c()
      for(ci in 1:Cu){
        # Get distances for Cu i
        probs = 1/distan[ci,][-1]
        # Data buffers
        results  = c()
        # Ask for sims retrievals for each cue
        for(si in 1:sims){
          # get retrievals for high and low
          results <- rbind(results, word_race_nam(probs, noisy_mean = 0, noisy_sd = sigs[sigsi], threshold = ths[th], Ab=Ab[Ais]))
          # Keep track of simulation
          print(paste("sim ", si))
        }
        # Get median number of iterations required to retrieve response for this cue
        time_keeper <- c(time_keeper, median(results[,2])) 
        # Get number of retrievals for each word
        xlow<-with(results, tapply(word, word, length))
        # Order them 
        xlow <- xlow[order(xlow, decreasing=TRUE)]
        # Normalize by total number of retrievals to get association strengths
        xlow <- xlow/sum(xlow)
        entropy_record <- rbind(entropy_record, entropy(xlow))
      }
      dataoutmonk <- rbind(dataoutmonk, c("Time" = mean(time_keeper), "Entropy" = mean(entropy_record), "Ab"=Ab[Ais], "Sigma" = sigs[sigsi], "Thresh"=ths[th]))
    }
  }
}


cor.test(as.vector(entropyout), as.vector(log(timeout)))

### test of tau against A_b

sigs <- seq(0, 1, .1) 
ths <- 5
Ab <- seq(.1, 1, .1)
Cues = 20
dataoutmonk <- c()
# Do for each threshold
for(th in 1:length(ths)){
  # Do for each noisy sig
  for(sigsi in 1:length(sigs)){
    # Setup simulation
    for(Ais in 1:length(Ab)){
      # Number of retrievals per cue
      sims = 500
      # Data buffers
      time_keeper <-c()
      entropy_record <- c()
      for(ci in 1:Cu){
        # Get distances for Cu i
        probs = 1/distan[ci,][-1]
        # Data buffers
        results  = c()
        # Ask for sims retrievals for each cue
        for(si in 1:sims){
          # get retrievals for high and low
          results <- rbind(results, word_race_nam(probs, noisy_mean = 0, noisy_sd = sigs[sigsi], threshold = ths[th], Ab=Ab[Ais]))
          # Keep track of simulation
          print(paste("sim ", si))
        }
        # Get median number of iterations required to retrieve response for this cue
        time_keeper <- c(time_keeper, median(results[,2])) 
        # Get number of retrievals for each word
        xlow<-with(results, tapply(word, word, length))
        # Order them 
        xlow <- xlow[order(xlow, decreasing=TRUE)]
        # Normalize by total number of retrievals to get association strengths
        xlow <- xlow/sum(xlow)
        entropy_record <- rbind(entropy_record, entropy(xlow))
      }
      dataoutmonk <- rbind(dataoutmonk, c("Time" = mean(time_keeper), "Entropy" = mean(entropy_record), "Ab"=Ab[Ais], "Sigma" = sigs[sigsi], "Thresh"=ths[th]))
    }
  }
}


cor.test(as.vector(entropyout), as.vector(log(timeout)))


#### Figure 4 ####

pdf(file="Figure4.pdf", width = 8, height = 7)
#load("tau_sig_data.RData")
pal = colorRampPalette(c("gold", "blue"))
par(mfrow=c(2,2))
par(mar=c(5,5,2,5))
rownames(entropy_matrix) <- c(1:10)
colnames(entropy_matrix) <- c(seq(0,1,.1))
entropy_matrix <- entropy_matrix[10:1,]
plot(entropy_matrix, breaks=5, xlab = TeX("Noise ($\\sigma$)"), ylab=TeX("Threshold ($\\tau$)"), main=TeX("Entropy     "), key=list(font=2, cex.axis=.75), spacing.key=c(1,1,0), col = pal(5))
rownames(time_matrix) <- c(1:10)
colnames(time_matrix) <- c(seq(0,1,.1))
time_matrix <- time_matrix[10:1,]
plot(time_matrix, breaks = 5, key=list(side=4, font=2, cex.axis=.75), main=TeX("Response time     "), xlab = TeX("Noise ($\\sigma$)"), ylab = TeX("Threshold ($\\tau$)"), spacing.key=c(1,1,0), col = pal(5))

load("dataoutmonk.RData")
dataoutmonk <- data.frame(dataoutmonk)
entropyout <- with(dataoutmonk, tapply(Entropy, list(Ab, Sigma),mean))
pal = colorRampPalette(c("gold", "blue"))
entropyout <- entropyout[nrow(entropyout):1,]
plot(entropyout, xlab = TeX("Noise ($\\sigma$)"), ylab=TeX("$A_{b}$"), main=TeX("Entropy     "), key=list(font=2, cex.axis=.75), spacing.key=c(1,1,0), col = pal(5))
#pal = colorRampPalette(c("darkolivegreen2", "coral"))
timeout <- with(dataoutmonk, tapply(Time, list(Ab, Sigma),mean))
timeout <- timeout[nrow(entropyout):1,]
plot(log10(timeout), key=list(side=4, font=2, cex.axis=.75), main=TeX("$Log_{10}$(response time)     "), xlab = TeX("Noise ($\\sigma$)"), ylab = TeX("$A_b$"), spacing.key=c(1,1,0), col = pal(20))
dev.off()

#### Figure 5 #### 

rm(list=ls())
# Function for noisy activation model
word_race_nam <- function(probs,noise = 0,noisy_mean=0, noisy_sd = 0, threshold=5){ 
  # Probs are similarities
  xx = 1:length(probs)
  # Set time to 0
  time <- 0
  # Initialize activation values
  activation_values <- rep(0, length(probs)+1)
  # Until threshold is reached
  while(max(activation_values)<threshold){
    # Sample a target in proportion to similarity
    samp <- sample(1:length(probs), 1, prob=probs)
    # Add activation with variance to sampled value 
    activation_values[samp] <-  activation_values[samp] + 1 + rnorm(1, mean =noisy_mean, sd=noisy_sd)
    # Add atime unit
    time <- time + 1
  }
  # Identify word that exceeded threshold
  chosen_one = xx[activation_values == max(activation_values)]
  # Return the chosen one with the time of retrieval
  return(data.frame(word = chosen_one, time))
}

entropy <- function(x){
  valuei <- 0
  for(ij in 1:length(x)){
    valuei <- valuei + x[ij]*log(x[ij]) 
  }
  valuei <- -valuei
  return(valuei)
}

sigs <- seq(0, 1, .2)
ths <- 5 
networks = 50 
N = 500 # nodes
dataoutcomes <- c()

for(networki in 1:networks){
  ## Preferential attachment, alpha = 1, m = 2 (used in the article)
  eg <- sample_pa(N, 1, m=2, directed = FALSE)
  
  ## Erdös-Renyi (uncomment the below to use an Erdös-Renyi random graph)
  #eg <- sample_gnp(N, .005)
  
  # IMPORTANT: Whatever representation is used
  # Take only nodes in giant component of 'eg', so all nodes are reachable
  V(eg)$membership <- components(eg)$membership
  eg <- subgraph(eg, which(V(eg)$membership == 1))
  degvals <- table(degree(eg)) 
    # Do for each threshold
    for(th in 1:length(ths)){
      # Do for each noisy sig
      for(sigi in 1:length(sigs)){
      for(ki in 1:length(degvals)){
        # Setup simulation
        # Number of retrievals per cue
        sims = 500 
        # Data buffers
        time_keeper <-c()
        entropy_record <- c()
        if(degvals[ki]>20){
          cuesForThisDegree <- sample(which(degree(eg)==names(degvals)[ki]), 5) 
        } else {
          cuesForThisDegree <- which(degree(eg)==names(degvals)[ki])
        }
        for(ci in 1:length(cuesForThisDegree)){
          # get distances for cue
          distancesForThisCue <- igraph::distances(eg, cuesForThisDegree[ci])
          # Get distances for Cu i
          probs = 1/distancesForThisCue[-cuesForThisDegree[ci]]
          # Data buffers
          results  = c()
          # Ask for sims retrievals for each cue
          for(si in 1:sims){
            # get retrievals for high and low
            results <- rbind(results, word_race_nam(probs, noisy_mean = 0, noisy_sd = sigs[sigi], threshold = ths[th]))
            # Keep track of simulation
            print(paste("sim ", si))
          }
          # Get median number of iterations required to retrieve response for this cue
          time_keeper <- median(results[,2])
          # Get number of retrievals for each word
          xlow<-with(results, tapply(word, word, length))
          # Order them 
          xlow <- xlow[order(xlow, decreasing=TRUE)]
          # Normalize by total number of retrievals to get association strengths
          xlow <- xlow/sum(xlow)
          entropy_record <-  entropy(xlow)
          dataoutcomes <- rbind(dataoutcomes, c(as.numeric(names(degvals[ki])), ths[th],sigs[sigi], time_keeper, entropy_record))
        }
        #rtOutcome[ki] <- mean(time_keeper)
        #entOutcome[ki] <- mean(entropy_record)
      }
      }
    }
}
dataoutcomes <- data.frame(dataoutcomes)
names(dataoutcomes) <- c("Degree", "Threshold","Noise", "Responsetime", "Entropy")
#library(scales)

# save(dataoutcomes, file="degreeXnoiseHeatmap.RData")
# save(dataoutcomes, file="degreeXnoiseHeatmap500trails50networks.RData")
load("degreeXnoiseHeatmap500trails50networks.RData")
pdf("Figure5.pdf", height=5, width = 10)
par(mfrow=c(1,2))
xmat <- with(dataoutcomes, tapply(Entropy, list(Noise, Degree), mean))
xmat1 <- xmat[6:1,1:28]
table(dataoutcomes$Degree)
table(list(dataoutcomes$Degree, dataoutcomes$Noise))
# take mean of columns greater than 20 , where data is sparse
#gt30 <- rowMeans(xmat[,30:42])
#xmat <- cbind(xmat[,1:19], gt20)
pal = colorRampPalette(c("gold", "blue"))
par(mar=c(5,5,2,5))
plot(xmat1, breaks = 5, key=list(side=4, font=2, cex.axis=.75), xlab = TeX("Degree ($k$)"), ylab = TeX("Noise ($\\sigma$)"), main="Entropy", col=pal(7), spacing.key=c(1.3,2,.5), border=NA , cex.lab = 1.2)
xmat <- with(dataoutcomes, tapply(Responsetime, list(Noise, Degree), mean))
xmat2 <- xmat[6:1,1:28]
plot(xmat2, breaks = 5, key=list(side=4, font=2, cex.axis=.75), polygon.key = NULL, axis.key=NULL, xlab = TeX("Degree ($k$)"), ylab =TeX("Noise ($\\sigma$)"), main="Response time", col=pal(7), spacing.key=c(1.3,2,.5), border = NA, cex.lab = 1.2)
dev.off()

#### Figure 6 ####

rm(list=ls())
# Function for noisy activation model
word_race_nam <- function(probs,noise = 0,noisy_mean=0, noisy_sd = 0, threshold=5){ 
  # Probs are similarities
  xx = 1:length(probs)
  # Set time to 0
  time <- 0
  # Initialize activation values
  activation_values <- rep(0, length(probs)+1)
  # Until threshold is reached
  while(max(activation_values)<threshold){
    # Sample a target in proportion to similarity
    samp <- sample(1:length(probs), 1, prob=probs)
    # Add activation with variance to sampled value 
    activation_values[samp] <-  activation_values[samp] + 1 + rnorm(1, mean =noisy_mean, sd=noisy_sd)
    # Add atime unit
    time <- time + 1
  }
  # Identify word that exceeded threshold
  chosen_one = xx[activation_values == max(activation_values)]
  # Return the chosen one with the time of retrieval
  return(data.frame(word = chosen_one, time))
}

entropy <- function(x){
  valuei <- 0
  for(ij in 1:length(x)){
    valuei <- valuei + x[ij]*log(x[ij]) 
  }
  valuei <- -valuei
  return(valuei)
}

sigs <- seq(0, 1, .2)
ths <- 5 
N = 500 # nodes
Cues = 20
Associates = 1000 # Edges in environment
dataoutcomes <- c()
ailist <- rep(c(0,.25,.50, .75, 1), each=20 )
networks=length(ailist)

for(networki in 1:networks){
  x <- 1:N
  a = ailist[networki] 
  pairs <- c()
  for(i in 1:Associates){
    pairs <- rbind(pairs, sample(x, size = 2, prob=x^(-a), replace = FALSE))
  }
  
  ii <- graph_from_edgelist(pairs,directed=FALSE) 
  E(ii)$weight <- 1
  eg <- graph_from_adjacency_matrix(get.adjacency(ii), weighted=TRUE, mode = "undirected")
  
  # Take only nodes in giant component of 'eg', so all nodes are reachable
  V(eg)$membership <- components(eg)$membership
  eg <- subgraph(eg, which(V(eg)$membership == 1))
  # Do for each threshold
  for(th in 1:length(ths)){
    # Do for each noisy sig
    for(sigi in 1:length(sigs)){
        # Setup simulation
        # Number of retrievals per cue
        sims = 500 
        # Data buffers
        time_keeper <-c()
        entropy_record <- c()
        Cueslist <- sample(1:length(V(eg)), Cues)
        for(ci in 1:Cues){
          # get distances for cue
          distancesForThisCue <- igraph::distances(eg, Cueslist[ci])
          # Get distances for Cu i
          probs = 1/distancesForThisCue[-Cueslist[ci]]
          # Data buffers
          results  = c()
          # Ask for sims retrievals for each cue
          for(si in 1:sims){
            # get retrievals for high and low
            results <- rbind(results, word_race_nam(probs, noisy_mean = 0, noisy_sd = sigs[sigi], threshold = ths[th]))
            # Keep track of simulation
            #print(paste("sim ", si))
          }
          print(paste("networki ", networki, " of ", networks ))
          # Get median number of iterations required to retrieve response for this cue
          time_keeper <- median(results[,2])
          # Get number of retrievals for each word
          xlow<-with(results, tapply(word, word, length))
          # Order them 
          xlow <- xlow[order(xlow, decreasing=TRUE)]
          # Normalize by total number of retrievals to get association strengths
          xlow <- xlow/sum(xlow)
          entropy_record <-  entropy(xlow)
          dataoutcomes <- rbind(dataoutcomes, c(ths[th],sigs[sigi], time_keeper, entropy_record, as.numeric(a)))
      }
    }
  }
}

#save(dataoutcomes, file="NetworkScalingXNoise.RData")
load("NetworkScalingXNoise.RData")
dataoutcomes <- data.frame(dataoutcomes)
names(dataoutcomes) <- c("Threshold","Noise", "Responsetime", "Entropy", "Lambda")
table(list(dataoutcomes$Lambda, dataoutcomes$Noise))
pdf("Figure6.pdf", width = 11, height = 4)
par(mfrow=c(1,2))
xmat <- with(dataoutcomes, tapply(Entropy, list(Noise, Lambda), mean))
xmat <- xmat[6:1,]
pal = colorRampPalette(c("gold", "blue"))
par(mar=c(5,5,2,5))
plot(xmat, breaks = 5, key=list(side=4, font=2, cex.axis=.75), xlab = TeX("Network scaling ($\\lambda$)"), ylab = TeX("Noise ($\\sigma$)"), main="Entropy", col=pal(7), spacing.key=c(1,.5,0),  cex.lab = 1.2)
xmat <- with(dataoutcomes, tapply(Responsetime, list(Noise, Lambda), mean))
xmat <- xmat[6:1,]
plot(xmat, breaks = 5, key=list(side=4, font=2, cex.axis=.75), polygon.key = NULL, axis.key=NULL, xlab = TeX("Network scaling ($\\lambda$)"), ylab = TeX("Noise ($\\sigma$)"), main="Response time", col=pal(7), spacing.key=c(1,.5,0), cex.lab = 1.2)
dev.off()

