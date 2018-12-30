# Rogers’ paradox: An agent-based model in R

An agent-based model in R illustrating a key result in the evolution of social learning literature: "Rogers' paradox". This was introduced by Alan Rogers in a 1988 paper and stimulated several further models trying to figure out when and why social learning (learning from others) is adaptive relative to individual learning (learning on one's own). One of these extensions - Enquist et al.'s 'critical social learning' - is also included here.

There are standalone R script versions of each model, as well as an RMarkdown version containing model descriptions and explanations of all models and results.

## Files:

rogers_paradox.pdf - description and explanation of the models, with representative model output

rogers_paradox.Rmd - the RMarkdown code used to create rogers_paradox.pdf

rogers_model.R - R script for the basic Rogers model, virtually identical to that contained within rogers_paradox.Rmd

criticalSL.R - R script for the Enquist et al. 'critical social learner' extension, virtually identical to that contained within rogers_paradox.Rmd

Use RStudio to knit the Rmd file. Install the following packages before running the scripts: ggplot2, Rmisc, reshape2

**Alex Mesoudi, 30 Dec 2018**
