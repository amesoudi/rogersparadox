# agent-based simulation of the model by Rogers (1989), showing the conditions under which social learners can spread in a population of individual learners

#  load packages---------------------------
library(ggplot2)
library(Rmisc)

#  list of variables---------------------
# E, state of the environment, initially 0
# w, baseline fitness, typically fixed at 1
# b, fitness bonus/penalty for being in the right/wrong environment (so w+b or w-b), typically fixed at 0.5
# u, probability of environmental shift. Unlike Rogers, it always shifts to a new value, which is more realistic than cycling between two.
# c, where bc = cost of individual learning. set constraint that b(1+c)<1, to avoid negative fitnesses
# s, where bs = cost of social learning. constraint again of b(1+s)<1
# N, number of agents
# mu, probability of new agent having different learning strategy to parent (mutation)
# p, probability of an individual learner learning correct behaviour
# t.max, maximum number of generations
# runs, number of simulation runs
# runin, number of timesteps to ignore before calculating mean proportions and fitnesses

#  model outline-----------------------------------
# cycle thru each agent, assuming partially-overlapping generations: learning occurs between generations, but complete replacement
# 1st generation is all individual learners
# for each agent:
#   if individual learner, set behaviour to match E with probability p
#   if social learner, choose one previous gen member at random, copy their behaviour
# calculate relative fitness: from baseline w, +b for correct beh, -b for incorrect, -bc from ILers, -bs from SLers. Sum all fitnesses, divide each agents' fitness by this.
# agents reproduce with probability proportional to relative fitness. Create new generation. Small probability mu of mutating learning strategy; this introduces SLers at the start
# cycle again, over t.max generations. Keep track of and plot proportion of social learners (0-1) and mean population fitness relative to an all-IL baseline
# prediction: with environmental change and sufficiently costly individual learning, social learners invade individual learners, spread until reach equilibrium. Then they coexist. But the mean population fitness does not exceed the baseline IL-only fitness (= Rogers' paradox)

#  define functions----------------------------------

# initialise agent data frame
InitialiseAgent <- function (N) {
  # create vectors of length N:
  # 'learning' style is initially all individual, 'behaviour' and 'fitness' are all NA
  learning <- rep("individual", N)
  behaviour <- rep(NA, N)
  fitness <- rep(NA, N)
  
  # combine them into a data frame to return, ensuring that none become factors
  data.frame(behaviour, learning, fitness, stringsAsFactors = FALSE)
}

# environmental change function
EnvironmentalChange <- function(E, u) { 
  # generate 1 random number between 0 and 1. If less than u, change the environment
  if (runif(1) < u) {  
    E <- E + 1 } # increment environment to a new novel value
  E  # return E
}

# initialise output data frame, to store results
InitialiseOutput <- function(t.max) {
  timestep <- vector("numeric",t.max)  # timestep
  n.SL <- vector("numeric",t.max)  # number of social learners
  w.SL <- vector("numeric",t.max)  # fitness of social learners
  W <- vector("numeric",t.max)  # total absolute fitness of population
  data.frame(timestep, n.SL, w.SL, W)  # return this as 'output'
}

#learning function
LearningStage <- function (agent, p, E, agent.previous) {
  
  # individual learners:
  individual.learners <- agent$learning == "individual"  # vector of individual learners
  success <- runif(nrow(agent))  # vector with N random numbers each between 0 and 1
  agent$behaviour[individual.learners & success < p] <- E  # if agent is an individual learner, and p exceeds success, then they learn the correct behaviour
  agent$behaviour[individual.learners & success >= p] <- E - 1  # otherwise, they learn incorrect behaviour, always correct behaviour minus one
  
  # social learners:
  social.learners <- agent$learning == "social"  # vector of social learners
  # continue only if there are social learners
  if (sum(social.learners) > 0) {  
    # a vector containing randomly chosen demonstrators, one for each social learner. With replacement, so that demonstrators can be picked more than once. Note they can potentially copy themselves, but for large N this is rare
    demonstrators <- sample(1:nrow(agent), sum(social.learners), replace = TRUE)  
    # now copy the behaviour of demonstrators into behaviour of social learners
    agent$behaviour[social.learners] <- agent.previous$behaviour[demonstrators]  
  }
  
  agent  # return agent data frame
}

#  functions for getting fitness & number of learners, plus overall mean fitness W
GetSLnumber <- function (agent) {
  sum(agent$learning == "social") / nrow(agent) } # return n.SL as a proportion of N

GetW <- function (agent) {
  sum(agent$fitness, na.rm = TRUE) } # return the total fitness of all agents

GetSLfitness <- function (agent) {
  if (sum(agent$learning == "social") > 0) {  # if there is at least one social learner
    w.SL <- mean(agent$fitness[agent$learning == "social"])  # get their mean fitness
  } else {
    w.SL <- 0  # otherwise return zero
  }
  w.SL
}

#  calculate fitness of agents
CalculateFitness <- function (agent, w, E, b, c, s) {
  # start with baseline fitness w
  agent$fitness <- w  
  
  # for those agents with behaviour matched to the environment, add b
  agent$fitness[agent$behaviour == E] <- agent$fitness[agent$behaviour == E] + b  
  # for those agents with behaviour not matched to the environment, subtract b
  agent$fitness[agent$behaviour != E] <- agent$fitness[agent$behaviour != E] - b  
  
  # impose cost b*c on individual learners
  agent$fitness[agent$learning == "individual"] <- agent$fitness[agent$learning == "individual"] - b*c  
  # impose cost b*s on social learners
  agent$fitness[agent$learning == "social"] <- agent$fitness[agent$learning == "social"] - b*s  
  agent  # return updated agent
}

#  produce new generation from previous
ReproductionStage <- function (agent, agent.previous, mu) {
  
  # from now on, agent denotes new generation, agent.previous denotes previous
  
  agent$behaviour <- NA  # reset agent's behaviour and fitness
  agent$fitness <- NA
  
  # to determine learning, need to get probability of individual learner being a parent, weighted by relative fitness
  W <- GetW(agent.previous)  # get overall total fitness of previous generation
  # get probability of new agent being an individual learner, weighted by fitness
  if (sum(agent.previous$learning == "individual") > 0) {
    prob.IL <- sum(agent.previous$fitness[agent.previous$learning == "individual"]) / W 
  } else {
    prob.IL <- 0
  }

  # vector with random numbers each between 0 and 1, to determine probability of being an individual learner
  success.IL <- runif(nrow(agent.previous))  
  # vector with random numbers each between 0 and 1, to determine probability of mutation
  success.mu <- runif(nrow(agent.previous))  
  
  # if parent is an ind learner and no mutation, then they're an ind learner
  agent$learning[success.IL < prob.IL & success.mu >= mu] <- "individual"  
  # if parent is a social learner and no mutation, then they're a social learner
  agent$learning[success.IL >= prob.IL & success.mu >= mu] <- "social"  
  # if parent is an ind learner plus mutation, then they're a social learner
  agent$learning[success.IL < prob.IL & success.mu < mu] <- "social"  
  # if parent is a social learner, plus mutation, then they're an ind learner
  agent$learning[success.IL >= prob.IL & success.mu < mu] <- "individual"  
  
  agent  # return agent
}

# update output data frame for this run
GetOutput <- function(run.output, t, agent) {
  run.output$timestep[t] <- t
  run.output$n.SL[t] <- GetSLnumber(agent)
  run.output$w.SL[t] <- GetSLfitness(agent)
  run.output$W[t] <- GetW(agent) / nrow(agent)
  run.output
}

# run the simulation once
RunSimulation <- function (N, t.max, u, p, w, b, c, s, mu) {
  
  # check parameters, to avoid negative fitnesses
  if (b*(1+c) > 1 || b*(1+s) > 1) {
    stop("Invalid parameter values: ensure b*(1+c) < 1 and b*(1+s) < 1")
  }
  
  #initialise agent data frame
  agent <- InitialiseAgent(N)
  
  #  initialise environment E
  E <- 0
  
  #  initialise output data frame for this run
  run.output <- InitialiseOutput(t.max)
  
  for (t in 1:t.max) { # cycle thru timesteps
    
    #  update agent as a result of learning
    agent <- LearningStage(agent, p, E, agent.previous)
    
    #  update agent's fitness as a result of fitness calculations
    agent <- CalculateFitness(agent, w, E, b, c, s)
    
    #  get variables needed for the plots later on
    run.output <- GetOutput(run.output, t, agent)
    
    # copy this gen into agent.previous, to act as demonstrators to next gen's social learners
    agent.previous <- agent
    
    #  create new generation of agents from agent.previous
    agent <- ReproductionStage(agent, agent.previous, mu)
    
    #  possible environmental change
    E <- EnvironmentalChange(E, u)
    
  }  # end of timestep-for
  
  run.output  # return output data frame for this run
}

# simulation loop, repeat 'runs' times and append output to output dataframe 
SimulationLoop <- function (runs, N, t.max, u, p, w, b, c, s, mu) {
  
  # set up runs loop, each run produces output. Add to output dataframe if it already exists
  for (i in 1:runs) {
    
    # for first run, create output data frame
    if (i == 1) {
    output <- RunSimulation(N, t.max, u, p, w, b, c, s, mu)
    } else {
      # otherwise add the new run.output on to the end of output using rbind
      run.output <- RunSimulation(N, t.max, u, p, w, b, c, s, mu)
      output <- rbind(output, run.output)
    }
  }  # end of runs-loop
  
  # draw plots
  DrawPlots(output, N, w, b, p, c, u, s, mu, runs)
  
  output  # return output
}

GetOutputSummary <- function (output) {
  # take the full multiple-runs output and generate means, maxs and mins across timesteps
  mean <- aggregate(output[, 2:4], by = list(output$timestep), mean)  # gives means
  max <- aggregate(output[, 2:4], by = list(output$timestep), max)  # gives maximums
  min <- aggregate(output[, 2:4], by = list(output$timestep), min)  # gives minimums
  
  # return output.summary
  data.frame(mean$n.SL, min$n.SL, max$n.SL, mean$w.SL, min$w.SL, max$w.SL, mean$W, min$W, max$W)
}

DrawPlots <- function (output, N, w, b, p, c, u, s, mu, runs) {

  # first need to get means and maximums/minimums from raw output
  output.summary <- GetOutputSummary(output)
  
  # create the graph title from parameter values
  graph.title <- paste("t.max = " ,nrow(output.summary), ", ", "N = ", N, ", ", "w = ", w, ", ", "b = ", b, ", ", "c = ", c, ", ", "s = ", s, ", ", "u = ", u, ", ", "mu = ", mu, ", ", "p = ", p, ", ", "runs = ", runs, sep = "")

  # plot number of social learners
  plot1 <- ggplot(data = output.summary, aes(x = 1:nrow(output.summary), y = mean.n.SL)) + geom_line() + geom_ribbon(aes(ymin = min.n.SL, ymax = max.n.SL), alpha = 0.2) + labs(x = "Generation", y = "Proportion of social learners") + scale_y_continuous(limits = c(0,1)) + theme_classic(base_size = 14) + ggtitle(graph.title) + theme(plot.title = element_text(size = 14), axis.line.x = element_line(colour = "black", size = 0.5), axis.line.y = element_line(colour = "black", size = 0.5))
  
  # plot mean population fitness, W, with a dotted line showing expected W in a population of all individual learners
  plot2 <- ggplot(data = output.summary, aes(x = 1:nrow(output.summary), y = mean.W)) + geom_line() + geom_ribbon(aes(ymin = min.W, ymax = max.W), alpha = 0.2) + geom_hline(yintercept = w + b*(2*p - c - 1), linetype = 2) + labs(x = "Generation", y = "Population fitness") + scale_y_continuous(limits = c(0,NA)) + theme_classic(base_size = 14) + theme(axis.line.x = element_line(colour = "black", size = 0.5), axis.line.y = element_line(colour = "black", size = 0.5))
  
  # optionally: save to png image in folder 'graphs' with filename indicating parameters
  #graph.name <- paste("graphs/", "N" ,nrow(output.summary),"_w", w, "_b", b, "_c", c, "_s", s, "_u", u, "_mu", mu, "_p", p, "_runs", runs, ".png", sep = "")
  
  #png(file = graph.name, width = 20, height = 20, units = "cm", res = 300)
  #multiplot(plot1, plot2)
  #dev.off()

  # display 3 plots using multiplot from Rmisc
  multiplot(plot1, plot2)
}

# main function, with defaults for infrequently modified parameters (override below)
RogersModel <- function (runs, N, t.max, u, p, w = 1, b = 0.5, c, s = 0.0, mu, runin = 100) {
  
# system.time shows how long the simulations took
print(system.time(output <- SimulationLoop(runs, N, t.max, u, p, w, b, c, s, mu)))
  
# return mean SL after first 100 timesteps
cat("\nMean proportion of social learners:", mean(output$n.SL[output$timestep > runin]))
cat("\nMean fitness of social learners:", mean(output$w.SL[output$timestep > runin]))
cat("\nMean fitness of individual learners:", w + b*(2*p - c - 1))
cat("\nMean population fitness:", mean(output$W[output$timestep > runin]))

output  # return output data
}

# run simulation here, overriding defaults if needed-------------------------
output <- RogersModel(runs = 10, N = 1000, t.max = 500, u = 0.2, p = 1, c = 0.9, mu = 0.01)

