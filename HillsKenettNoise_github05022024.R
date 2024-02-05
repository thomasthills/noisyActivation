# Code for Hills & Kenett, A Noisy Activation Theory of Creativity

rm(list=ls()) # for good health
#### Necessary packages

library(igraph)

#### Figure 1 ####

par(mar=c(5,5,2,2))
x <- 0:4
y =  hc = 60*exp(-x)
hp = 100*exp(-x) 
hf = 50*exp(-x/6)
lc = 40*exp(-x/6)
# Save plot of theory visualization
pdf("Figure1_NoisyActivationOnlineCode.pdf")
plot(x,hc, type = "l", ylab = "Associative strength", xaxt="n", yaxt="n", lwd=3, cex.lab = 1.4, xlab="Concepts ranked by associative strength", lty=2, ylim=c(0,100), col="gray80")
lines(x, lc, lty=1, lwd=3, col="gray80")
lines(x, hp, lty=3, lwd=3)
lines(x, hf, lty=4, lwd=3)
##lines(x, y4, lty=4, lwd=2)
axis(1, at = seq(0, 4, by = 1), labels = c("1", "2", "3", "4", "5") )
axis(2, at = c(0, 100), labels = c("Low", "High"), las =1 )
legend(2,90, legend=c("Lower - Mednick","Higher - Mednick", "Faster-but-proportional", "Faster-and-further"),title="Creativity", lty=c(2, 1, 3, 4), lwd=c(3,3,3, 3), col = c("gray80", "gray80", "black", "black"), cex = .75, bty="n")
dev.off()

#### Figure 2 ####

# Build representation and compute distances from 20 randomly chosen cues

# set seed for reproducibility
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
pdf("Figure2_NoisyActivationOnlineCode.pdf", height = 5, width = 8)

# Plot parameters
par(mar=c(5,5,2,2))
par(mfrow=c(1,2))

# Set node sizes for visualization
V(eg)$size = 1
V(eg)$size[noCu] <-5 
# Set image seed
set.seed(11)
# Plot network
plot(eg, vertex.label = NA, edge.width = .5, vertex.color = "white")

# Plot similarity (1/d)
plot(1/dd, xlab = "Rank", ylab = "Similarity (1/distance)", cex = .4, log="xy")

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

# Setup simulation
# Number of retrievals per cue
sims = 5000 
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
  for(i in 1:sims){
    # get retrievals for high and low
    results_low <- rbind(results_low, word_race_nam(probs, noisy_mean = 0, noisy_sd = .5))
    results_high <- rbind(results_high, word_race_nam(probs, noisy_mean = 0, noisy_sd = 1.5  ) )
    # Keep track of simulation
    print(paste("sim ", i))
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

pdf("Figure3_NoisyActivationOnlineCode.pdf", height=4.5, width = 7)

par(mfrow=c(1,2))
# Limit figure to top 200 targets
targets =200 
# Get mean of association strengths
x1 <- colMeans(assocStr_low)
x2 <- colMeans(assocStr_high)
# Plot low creative line
plot(x1[1:targets], xlim=c(0, targets), pch=16, xlab = "Rank", ylab= "Association strength", log = "y", yaxt="n", cex = .4)
axis(2, at = c(.0001,.001, .01, .1), labels = c(".0001",".001",".01", ".1"), las =1 )
lines(x1[1:targets])
# Plot high creative line
points(x2[1:targets], col="grey80", pch = 16, cex = .4)
lines(x2[1:targets], col="grey80")
legend(50, .08, legend=c("Low (0.5)", "High (1.5)"), col=c("black", "gray80"), bty="n", lty=1, lwd=1.6, title = "Creativity")
# Boxplot of inter-item retrieval times
boxplot( data.frame(time_keeper_low, time_keeper_high), names=c("Low", "High"), ylab = "Relative Inter-item Retrieval Time", boxwex= c(.6,.6), cex = .2)
dev.off()

