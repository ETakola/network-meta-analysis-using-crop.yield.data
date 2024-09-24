# Network Meta-Analysis
# Author: Maximus Anochirim
# Date: September 2024

#Set working directory
setwd("C:/")
#installing the packages meta (for pairwise meta-analysis) and netmeta (for network meta-analysis)
install.packages("meta")
install.packages("netmeta")
#loading the installed packages
library("meta")
library("netmeta")
# To get a listing of the current/default settings being used in the meta package
settings.meta()
# argument "digits = 2": specifies that treatment estimates and confidence interval limits should be printed with two digits 
#argument "digits.se = 3": specifies that standard errors should be printed with three digits 
#argument "common = F": telling the command to not include results for a common effect model aka fixed effect model but rather for the random effect model"
settings.meta(digits = 2, digits.se = 3, common = FALSE)
#step 1: loading the dataset
library(readxl)
yield_data <- as.data.frame(read_excel("yield_dataset.xlsx", sheet = 1))
View(yield_data)
#The data_FORMAT is wide_arm-based_format where each row corresponds to a single study wt multiple or double comparisons
head(yield_data)
list(yield_data)
dim(yield_data)
str(yield_data)
summary(yield_data)
class(yield_data)
#step 2: transform data into input format (contrast-based format) for netmeta using "pairwise" function
# This step is also used to estimate treatment effects and corresponding standard errors.
# Additionally, we use the pairwise() function to organise the data / calculate pairwise comparisons
p1 <- pairwise(treat = list(pr_Treatment, pr_control),
               n = list(n_treatment, n_control),
               mean = list(pr_yield_treatm_kgha, pr_yield_control_kgha),
               sd = list(treatment_SD, control_SD),
               studylab = "Source",
               data = yield_data, sm="SMD")
##ARGUMENTS. 1=treatment or list of treatments, 2=sample size or list of sample sizes, 3=treatment means or a list of treatment means, 4=standard deviations (SD) or a list of the standard deviations, 5=study labels, 6=dataset, and 7=summary measure in this case "SMD" (Standardized mean differences).

#Results / what do we see
# An R object p1 which is a data frame with 3939 rows, each corresponding to a pairwise comparison
# (3938 rows from 3938 two-arm studies), and 32 columns (variables).
# The first two variables contain the mean differences (TE) and corresponding standard errors (seTE) of the pairwise comparison defined by variables treat1 and treat2. 

#step 3: Performing the main analysis – A standard NMA
net1 <- netmeta(p1, common = FALSE, reference.group = "Control")
net2 <- netmeta(TE, seTE, treat1, treat2, studlab, data = p1, common = FALSE, ref = "Control")
net1
net2

#Results / what do we see
# Printing the R object "net1" shows the main NMA results. It starts with a concise summary of the network showing:
# 1. k which is the number of independent studies. k simultaneously corresponds to m when there are only two-arm studies in our network; otherwise m is greater than k if at least one study evaluates more than two treatments.
# 2. m which is the number of pairwise comparisons each study contributes to the network. The sum of all pairwise comparisons across studies in our network denoted as m= 3732. 
# 3. o which is the number of observations
# 4. n which is the number of treatments of interest in our network (also called nodes or vertices) (n=4). 
# 5. And d which is the number of designs.
head(summary(net1), nma = FALSE)
print(summary(net1), nchar.trts = 4, nma = FALSE)
print(summary(net1), truncate = studlab %in% selstudy1, nchar.trts = 4,
      + nma = FALSE)
# network estimates under the random effects model

netconnection(p1)
#The function netconnection () is used to obtain information on the network structure
# It is also used to determine whether a given network is fully connected or consists of sub-networks
# In a disconnected network, the use of netmeta() will result in an error with a recommendation to use netconnection() for further information.

#step 4: present the result which include steps such as;
#1. Visualizing the evidence, 2. visualizing treatment effects, 3. ranking the treatments
#step 4.1: Graphically visualizing the evidence. This is done by constructing a network graph (function "netgraph") in order to get an overview of the network structure
netgraph(net1, points = TRUE, 
         cex.points = 7, adj = 0.5)
netgraph(net1, number.of.studies = TRUE,
         seq = "optimal",
         points = TRUE, offset = 0.03,
         cex.points = 7, adj = 0.5, cex = 1.5)

netgraph(net1, number.of.studies = TRUE)
#number.of.studies = TRUE makes the number blurry
#lwd = 2: allows the width of each line to correspond to the number of studies 
#Results / what do we see
#In the network graph, each treatment is represented by a point (node) in the plane
#furthermore, treatments are connected by a line (edge), if at least one direct pairwise comparison exists.
#our graph also visualizes the number of studies contributing to each pairwise comparison
#the thickness of the edges (line) are proportional to the number of studies directly comparing treatments.

#step 4.2: Visualizing the treatment effects
#Drawing a forest plot for network meta-analysis helps to summarize results
forest(net1, reference.group = "Control", drop.reference.group = TRUE,
       label.left = "Favours second intervention",
       label.right = "Favours first intervention")
forest(netsplit(net1, order = NULL), show = "all",
       text.overall = "Mixed treatment estimate")
forest(net1)
# Forest plot with Control as reference
forest(net1, ref = c("L", "Straw return", "Super Absorbent Polymer (SAP)"), baseline = FALSE, drop = TRUE)
# Forest plot with an active treatment versus all other treatments

#league table
netleague(net1)

#step 4.3: Ranking of the treatments evaluated in an NMA provides additional information on the merits of individual treatments
netrank(net1)
#Results / what do we see
# what we see is a treatment ranking using P-scores

rankogram(net1)
# what we see is a more detailed picture of the treatment hierarchy given by the rankograms for the competing treatments
# Control has 100% estimated probability of producing the best value compared to only 0% for the other active treatments. 
install.packages("mvtnorm")
library(mvtnorm)
plot(rankogram(net1))

netrank(rankogram(net1))
#what we see are SUCRA values for our NMA
#League table sorted by decreasing P-scores
netleague(net1, seq = netrank(net1), ci = FALSE)


#step 5: Evaluation of Heterogeneity. The decomposition of the overall Q statistic to assess heterogeneity and inconsistency
decomp.design(net1)

#Results / what do we see
# The design-specific decomposition of QW shows that the within-design heterogeneity can largely be traced back to the comparison of Control versus Straw return and Control versus Liming.
# Results for detaching single designs and the full design-by-treatment interaction model do not show evidence of inconsistency between designs.

#step 6: Evaluation of inconsistency – the comparison of direct and indirect evidence for each pairwise comparison.
netsplit(net1)
netheat(net1)

#step 7: Evaluation of funnel plot assymetry
funnel()

#Test Summary
#Based on the results of the tests for heterogeneity and inconsistency, we observe that there is significant variation among studies (this is supported by the high value of I² and the significance of the Q value. 
#Given this, we performed the following recommended steps:
Investigating sources of Heterogeneity:
  #With the use of (i) Subgroup Analysis, we conducted analyses within subgroups of studies to explore whether certain study characteristics would account for the observed heterogeneity,
  #(ii) Meta-regression techniques to identify factors that might explain the variability in effect sizes across studies
  
  
  #SUBGROUP ANALYSIS
  head(p1)
list(p1)
dim(p1)
str(p1)
summary(p1)
class(p1)
p1$DatasetID <- as.factor(p1$DatasetID)
p1$pr_Croptype <- as.factor(p1$pr_Croptype)
#for a subgroup analysis, we decided on using the different croptypes as a study characteristic
subgroup_results <- netmeta(TE, seTE, treat1, treat2, studlab, data = p1, 
                            common = FALSE, ref = "Control", 
                            byvar = p1$pr_Croptype)

# Summary of results
summary(subgroup_results)

# Forest plot for visualizing subgroup analysis
forest(subgroup_results)

#Meta regression???
