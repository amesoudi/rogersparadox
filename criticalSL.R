# same as Rogers model but now adding a 3rd agent type: critical social learner, which initially learns socially then learns individually if what if copies is incorrect

# run Rogers model first, to re-use functions InitialiseAgent, EnvironmentalChange, GetSLnumber, GetSLfitness, GetW
# new functions have .crit appended

# load packages---------------------------
library(reshape2)

#  define functions----------------------------------

# initialise output data frame, to store results
InitialiseOutput.crit <- function(t.max) {
  timestep <- vector("numeric",t.max)  # timestep
  n.SL <- vector("numeric",t.max)  # number of social learners
  n.IL <- vector("numeric",t.max)  # number of individual learners
  n.CL <- vector("numeric",t.max)  # number of critical learners
  w.SL <- vector("numeric",t.max)  # fitness of social learners
  w.IL <- vector("numeric",t.max)  # fitness of individual learners
  w.CL <- vector("numeric",t.max)  # fitness of critical learners
  W <- vector("numeric",t.max)  # total absolute fitness of population
  data.frame(timestep, n.SL, n.IL, n.CL, w.SL, w.IL, w.CL, W)  # return this as 'output'
}

# learning function - now incorporates fitness calculations
LearningStage.crit <- function (agent, p, E, agent.previous, w, b, c, s) {

  # start each agent with baseline fitness w
  agent$fitness <- w
  
  # individual learners
  individual.learners <- agent$learning == "individual"  # vector of individual learners
  success <- runif(nrow(agent))  # vector with random numbers each between 0 and 1
  agent$behaviour[individual.learners & success < p] <- E  # if agent is an individual learner, and p exceeds success, then they learn the correct behaviour
  agent$behaviour[individual.learners & success >= p] <- 1 - E  # otherwise, they learn the incorrect behaviour
  # subtract bc from fitness of ILers
  agent$fitness[individual.learners] <- agent$fitness[individual.learners] - b*c
  
  # social learners
  social.learners <- agent$learning == "social"  # vector of social learners
  if (sum(social.learners) > 0) {  # do this only if there are social learners
    demonstrators <- sample(1:nrow(agent), sum(social.learners), replace = TRUE)  # a vector containing randomly chosen demonstrators, one for each social learner. With replacement, so that demonstrators can be picked more than once
    agent$behaviour[social.learners] <- agent.previous$behaviour[demonstrators]  # copy the behaviour of demonstrators into behaviour of social learners
    # subtract bs from fitness of SLers
    agent$fitness[social.learners] <- agent$fitness[social.learners] - b*s
    }
  
  # critical learners
  critical.learners <- agent$learning == "critical"  # vector of critical learners
  if (sum(critical.learners) > 0) {  # do this only if there are critical learners
    demonstrators <- sample(1:nrow(agent), sum(critical.learners), replace = TRUE)  # a vector containing randomly chosen demonstrators, one for each critical learner. With replacement, so that demonstrators can be picked more than once
    agent$behaviour[critical.learners] <- agent.previous$behaviour[demonstrators]  # copy the behaviour of demonstrators into behaviour of critical learners
    # subtract bs from fitness of CLers
    agent$fitness[critical.learners] <- agent$fitness[critical.learners] - b*s
    
    # get vector of critical learners who learned the wrong behaviour
    critical.wrong <- agent$learning == "critical" & agent$behaviour != E
    
    if (sum(critical.wrong) > 0) {  # do this only if there are wrong critical learners
    # repeat individual learning routine on these critical learners
    success <- runif(nrow(agent))  # vector with random numbers each between 0 and 1
    agent$behaviour[critical.wrong & success < p] <- E  # if agent is an individual learner, and p exceeds success, then they learn the correct behaviour
    agent$behaviour[critical.wrong & success >= p] <- 1 - E  # otherwise, they learn the incorrect behaviour
    # subtract bc from fitness of ILing CLers
    agent$fitness[critical.wrong] <- agent$fitness[critical.wrong] - b*c
    
    }
  }
  
  # for all agents:
  # for those agents with behaviour matched to the environment, add b
  agent$fitness[agent$behaviour == E] <- agent$fitness[agent$behaviour == E] + b  
  # for those agents with behaviour not matched to the environment, subtract b
  agent$fitness[agent$behaviour != E] <- agent$fitness[agent$behaviour != E] - b  
    
  agent  # return agent data frame
}

# new functions for getting fitness and number of ind and critical learners
GetILnumber <- function (agent) {
  sum(agent$learning == "individual") / nrow(agent)  # return n.IL as a proportion of N
}
GetCLnumber <- function (agent) {
  sum(agent$learning == "critical") / nrow(agent)  # return n.CL as a proportion of N
}
GetILfitness <- function (agent) {
  if (sum(agent$learning == "individual") > 0) {  # if there is at least one individual learner
    w.IL <- mean(agent$fitness[agent$learning == "individual"])  # get their mean fitness
  } else {
    w.IL <- 0  # otherwise return zero
  }
  w.IL
}
GetCLfitness <- function (agent) {
  if (sum(agent$learning == "critical") > 0) {  # if there is at least one critical learner
    w.CL <- mean(agent$fitness[agent$learning == "critical"])  # get their mean fitness
  } else {
    w.CL <- 0  # otherwise return zero
  }
  w.CL
}

#  produce new generation from previous
ReproductionStage.crit <- function (agent, agent.previous, mu) {
  
  # from now on, agent denotes new generation, agent.previous denotes previous
  
  agent$behaviour <- NA  # reset agent's behaviour and fitness
  agent$fitness <- NA
  
  # to determine learning, need to get probability of individual learner being a parent, weighted by relative fitness
  W <- GetW(agent.previous)  # get overall total fitness of previous generation
  
  # get probability of new agent being an IL, weighted by fitness
  if (sum(agent.previous$learning == "individual") > 0) {
    prob.IL <- sum(agent.previous$fitness[agent.previous$learning == "individual"]) / W 
  } else {
    prob.IL <- 0
  }
  
  # get probability of new agent being an SL, weighted by fitness
  if (sum(agent.previous$learning == "social") > 0) {
    prob.SL <- sum(agent.previous$fitness[agent.previous$learning == "social"]) / W 
  } else {
    prob.SL <- 0
  }
  
  # vector with random numbers each between 0 and 1, to determine prob of being ind learner
  success.IL <- runif(nrow(agent.previous))  
  # second vector to give prob of SL
  success.SL <- runif(nrow(agent.previous))
 
  # new agent is IL with probability prob.IL
  agent$learning[success.IL < prob.IL] <- "individual" 
  # otherwise SL, with prob prob.SL
  agent$learning[success.IL >= prob.IL & success.SL < prob.SL] <- "social"
  # otherwise CL
  agent$learning[success.IL >= prob.IL & success.SL >= prob.SL] <- "critical"
  
  # now mutation: 
  # vector with random numbers each between 0 and 1, to determine probability of mutation for each agent
  success.mu <- runif(nrow(agent.previous))  
  # random number 0 or 1, to determine which of the other two learning types to switch to
  mutant <- sample(0:1,nrow(agent.previous), replace = TRUE)  
  
  # individual learners: switch to either social or critical
  agent$learning[success.mu < mu & agent.previous$learning == "individual" & mutant == 0] <- "social"
  agent$learning[success.mu < mu & agent.previous$learning == "individual" & mutant == 1] <- "critical"
  
  # social learners: switch to either individual or critical
  agent$learning[success.mu < mu & agent.previous$learning == "social" & mutant == 0] <- "individual"
  agent$learning[success.mu < mu & agent.previous$learning == "social" & mutant == 1] <- "critical"
  
  # critical learners: switch to either social or indiviudal
  agent$learning[success.mu < mu & agent.previous$learning == "critical" & mutant == 0] <- "social"
  agent$learning[success.mu < mu & agent.previous$learning == "critical" & mutant == 1] <- "individual"
  
  agent  # return agent
}

GetOutput.crit <- function(output, t, agent) {
  output$timestep[t] <- t
  output$n.SL[t] <- GetSLnumber(agent)
  output$n.IL[t] <- GetILnumber(agent)
  output$n.CL[t] <- GetCLnumber(agent)
  output$w.SL[t] <- GetSLfitness(agent)
  output$w.IL[t] <- GetILfitness(agent)
  output$w.CL[t] <- GetCLfitness(agent)
  output$W[t] <- GetW(agent) / nrow(agent)
  output
}

#  run the simulation once
RunSimulation.crit <- function (N, t.max, u, p, w, b, c, s, mu, output) {
  
  # check parameters
  if (b*(1+c) > 1 || b*(1+s) > 1) {
    stop("Invalid parameter values: ensure b*(1+c) < 1 and b*(1+s) < 1")
  }
  
  #initialise agent data frame
  agent <- InitialiseAgent(N)
  
  #  initialise environment E
  E <- 0
  
  for (t in 1:t.max) { # cycle thru timesteps
    
    #  update agent as a result of learning  
    agent <- LearningStage.crit(agent, p, E, agent.previous, w, b, c, s)
    
    #  get variables needed for the plots later on
    output <- GetOutput.crit(output, t, agent)
    
    # copy this generation into agent.previous, to serve as demonstrators to next gen's social learners
    agent.previous <- agent
    
    #  create new generation of agents from agent.previous
    agent <- ReproductionStage.crit(agent, agent.previous, mu)
    
    #  possible environmental change
    E <- EnvironmentalChange(E, u)
    
  }  # end of timestep for
  
  output  # return output data frame
}

# simulation loop, repeat runs times and append output to output dataframe 
SimulationLoop.crit <- function (runs, N, t.max, u, p, w, b, c, s, mu) {
  
  # initialise output data frame
  output <- InitialiseOutput.crit(t.max)
  
  # set up s loop, each run produces output. Add to output dataframe if it already exists
  for (i in 1:runs) {
    
    # for first run, create output data frame
    if (i == 1) {
      output <- RunSimulation.crit(N, t.max, u, p, w, b, c, s, mu, output)
    } else {
      output <- rbind(output, RunSimulation.crit(N, t.max, u, p, w, b, c, s, mu, output))  # otherwise add it on
    }
  }  # end of runs loop
  
  # draw plots
  DrawPlots.crit(output, w, b, p, c, u, s, mu, runs)
  
  output  # return output
}

GetOutputSummary.crit <- function (output) {
  mean <- aggregate(output[, 2:8], by = list(output$timestep), mean)  # gives means
  max <- aggregate(output[, 2:8], by = list(output$timestep), max)  # gives maximums
  min <- aggregate(output[, 2:8], by = list(output$timestep), min)  # gives minimums
  
  data.frame(mean$n.SL, min$n.SL, max$n.SL, mean$n.IL, min$n.IL, max$n.IL, mean$n.CL, min$n.CL, max$n.CL, mean$w.SL, min$w.SL, max$w.SL, mean$w.IL, min$w.IL, max$w.IL, mean$w.CL, min$w.CL, max$w.CL, mean$W, min$W, max$W)
}

DrawPlots.crit <- function (output, w, b, p, c, u, s, mu, runs) {
  # with ggplot (and multiplot from Rmisc)
  
  # first need to get means and maximums/minimums from raw output
  output.summary <- GetOutputSummary.crit(output)
  
  # create the graph title from parameter values
  graph.title <- paste("N = " ,nrow(output.summary), ", ", "w = ", w, ", ", "b = ", b, ", ", "c = ", c, ", ", "s = ", s, ", ", "u = ", u, ", ", "mu = ", mu, ", ", "p = ", p, ", ", "runs = ", runs, ", ", sep = "")
  
  # plot number of different learners
  # need to melt output into long format first, to plot three lines
  output.long <- output.summary[, c("mean.n.IL", "mean.n.SL","mean.n.CL")]
  names(output.long) <- c("Individual", "Social","Critical")
  output.long$Timestep <- c(1:nrow(output.summary))
  output.long <- melt(output.long, id = "Timestep", value.name = "Mean", variable.name = "Learning")
  
  plot1 <- ggplot(data = output.long, aes(x = Timestep, y = Mean, colour = Learning)) + geom_line() + labs(x = "Generation", y = "Number of learners") + scale_y_continuous(limits = c(0,NA)) + theme_classic(base_size = 14) + ggtitle(graph.title) + theme(plot.title = element_text(size = 12), axis.line.x = element_line(colour = "black", size = 0.5), axis.line.y = element_line(colour = "black", size = 0.5))
  
  # plot mean fitness of individual, social and critical learners as three lines
  # need to melt output into long format first, to plot three lines
  output.long <- output.summary[, c("mean.w.IL", "mean.w.SL","mean.w.CL")]
  names(output.long) <- c("Individual", "Social", "Critical")
  output.long$Timestep <- c(1:nrow(output.summary))
  output.long <- melt(output.long, id = "Timestep", value.name = "Mean", variable.name = "Learning")
  
  plot2 <- ggplot(data = output.long, aes(x = Timestep, y = Mean, colour = Learning)) + geom_line() + labs(x = "Generation", y = "Fitness of learners") + scale_y_continuous(limits = c(0,NA)) + theme_classic(base_size = 14) + theme(axis.line.x = element_line(colour = "black", size = 0.5), axis.line.y = element_line(colour = "black", size = 0.5))
  
  # plot mean population fitness, W, with a line showing expected W in a population of all individual learners
  plot3 <- ggplot(data = output.summary, aes(x = 1:nrow(output.summary), y = mean.W)) + geom_line() + geom_ribbon(aes(ymin = min.W, ymax = max.W), alpha = 0.2) + geom_hline(yintercept = w + b*(2*p - c - 1), linetype = 2) + labs(x = "Generation", y = "Population fitness") + scale_y_continuous(limits = c(0,NA)) + theme_classic(base_size = 14) + theme(axis.line.x = element_line(colour = "black", size = 0.5), axis.line.y = element_line(colour = "black", size = 0.5))
  
  # optionally, save to png image in folder 'graphs' with filename indicating parameters
  #graph.name <- paste("graphs/", "N" ,nrow(output.summary),"_w", w, "_b", b, "_c", c, "_s", s, "_u", u, "_mu", mu, "_p", p, "_runs", runs, ".png", sep = "")
  
  #png(file = graph.name, width = 20, height = 20, units = "cm", res = 300)
  #multiplot(plot1, plot2, plot3)
  #dev.off()
  
  # draw the plot again to leave it visible when programme finishes
  multiplot(plot1, plot2, plot3)
}

# run simulation-------------------------------

# main function, with defaults
RogersModel.crit <- function (runs = 5, N = 1000, t.max = 250, u = 0.2, p = 1, w = 1, b = 0.5, c = 0.9, s = 0.0, mu = 0.01, runin = 100) {
  
  # system.time shows how long the simulations took
  print(system.time(output <- SimulationLoop.crit(runs, N, t.max, u, p, w, b, c, s, mu)))
  
  # return mean SL after first 100 timesteps
  cat("\nMean proportion of individual learners:", mean(output$n.IL[output$timestep > runin]))
  cat("\nMean proportion of social learners:", mean(output$n.SL[output$timestep > runin]))
  cat("\nMean proportion of critical learners:", mean(output$n.CL[output$timestep > runin]))
  cat("\nMean fitness of individual learners:", mean(output$w.IL[output$timestep > runin]))
  cat("\nMean fitness of social learners:", mean(output$w.SL[output$timestep > runin]))
  cat("\nMean fitness of critical learners:", mean(output$w.CL[output$timestep > runin]))
  cat("\nMean population fitness:", mean(output$W[output$timestep > runin]))
  
  output  # return output data
}

# run simulation here
output <- RogersModel.crit(runs = 10, N = 250, t.max = 250, u = 0.1, p = 1, c = 0.9, mu = 0.01, runin = 100)
