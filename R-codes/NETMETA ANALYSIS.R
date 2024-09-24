# Network Meta-Analysis
# Author: Maximus Anochirim
# Date: July 2024

#Set working directory
setwd("C:/Users/CP24/Desktop/INTERNSHIP-UFZ")
#NOTES: mixed comparisons are always more precise except in cases where the direct and indirect evidence disagree
#NOTES: One of the main aim of NMA is to include multi-arm studies in the anlysis
#NOTES: Another aim is to rank treatments according to efficacy
#NOTES: Network consistency estimation - sum of direct treatment effects must be zero and in a closed loop
#NOTES: Network consistency estimation - indirect evidence for diff btw 2 trts does not differ from the direct evidence
EXAMPLES:BASED:ON:TWO:ARM:STUDIES{getwd()
# what is a two arm study?
#installing the packages meta (for pairwise meta-analysis) and netmeta (for network meta-analysis)
install.packages("meta")
install.packages("netmeta")
#functions "metagen" for meta-analysis and "netmeta" for network meta-analysis
#loading the installed packages
library("meta")
library("netmeta")
# To get a listing of the current/default settings being used in the meta package
settings.meta()
# argument "digits = 2": specifies that treatment estimates and confidence interval limits should be printed with two digits 
#argument "digits.se = 3": specifies that standard errors should be printed with three digits 
#argument "common = F": telling the command to not include results for a common effect model aka fixed effect model  but rather for the random effect model"
settings.meta(digits = 2, digits.se = 3, common = FALSE)
?meta()
#meta analysis----
data ("Senn2013")
#using built-in dataset comparing effects of a number of drugs on the HbA1c value
#data has only one treatment estimate. i.e they are not independent ??
#"metagen" function for generic inverse variance pairwise meta-analysis
#arguments. 1=trt estimate, 2=std.error, 4=summary measure (mean diff), overall=FALSE: not interested in overall result across subgroups
#overall.hetstat=FALSE: no overall heterogeneity estimates calculated, test.subgroup=testing for subgroup differences
m1 <- metagen(TE, seTE, studlab = studlab, sm= "MD",
              data = Senn2013, subgroup = paste(treat1.long, treat2.long, sep = "vs"),
              overall = FALSE, overall.hetstat = FALSE, test.subgroup = FALSE, 
              print.subgroup.name = FALSE)
#Result interpretation: k is the number of studies having direct comparisons, MD is the result of the statistical test used
##Direct comparison (Rosiglitazone vs Metformin)#### 
print(update(m1, subset = treat1 %in% c("metf", "rosi") & treat2 %in% c("metf", "plac")),
      details = FALSE)
#Interpretation. tau^2 = estimates for between study variants in the subgroup
#Tau^2 tells us how much of a variation there is between studies
##Indirect comparison (Rosiglitazone vs Metformin)####
#nMA assumption: tau squared is the same within subgroups for any pairwise comparison we have
print(update(m1, subset = treat1 %in% c("metf", "rosi") & treat2 == "plac",
             tau.common = TRUE), details = FALSE)
#network meta-analysis----
##direct comparisons----
#some defaults defined with the settings.meta() are also considered for network meta-analysis
data (Senn2013)
?netmeta()
#"netmeta" function for network meta-analysis using graph-theoretical method
#arguments. 1=trt estimate, 2=std.error, 3&4=Label/Number for first and second treatment
#5=vector with study labels. 6=data frame containing the study information
#7=vector specifying a subset of studies to be used, 8=summary measure (mean diff), 
#9=indicating which method is used to estimate the between-study variance and its square root (tau2), 10=Reference treatment
#11=A character or numerical vector specifying the sequence of treatments in printouts
net1 <- netmeta(TE, seTE, treat1.long, treat2.long, studlab = studlab, data = Senn2013, 
                subset = treat1 %in% c("metf", "rosi") & treat2 == "plac", sm = "MD",
              method.tau = "REML", reference.group = "P", seq = c("R", "M", "P"))
###network graph----
#creating a network graph using the "netgraph" function
#arguments. 1=object, 2=indicating whether no. of studies should be added to the graph(logical)
#3=magnification to be used for treatment labels, 4=magnification to be used for number of studies
netgraph(net1, number.of.studies = TRUE, cex = 1.25, cex.number.of.studies = 1.5)
print(net1)
##indirect evidence - to calculate variance estimate of indirect treatment estimate----
#under assumption that treatment estimates are independent. only works in 2-arm studies and not on 
#multi-arm studies (studies comparing more than  2 treatments)
netleague(net1)
#calculating variance estimate using the "netleague" function
#sum of two variances
#result: second row is the indirect estimate together with confidence interval
##mixed treatment comparison---- 
#getting network estimates based on combination of direct and indirect treatment comparisons
#combining direct and indirect evidence. weights are inverse of variance weights
net2 <- netmeta(TE, seTE, treat1.long, treat2.long, studlab = studlab, data = Senn2013, 
                subset = treat1 %in% c("metf", "rosi") & treat2 %in% c("metf", "plac"), sm = "MD",
                method.tau = "REML", reference.group = "P", seq = c("R", "M", "P"))
netgraph(net2, number.of.studies = TRUE, cex = 1.25, cex.number.of.studies = 1.5)
print(net2)
netleague(net2)
###forest plot with direct, indirect and mixed treatment estimates ----
forest(netsplit(net2, order = c("R", "M", "P")),
       text.overall = "Mixed treatment estimate")
#creating a forest plot using the "forest" function
#arguments. 1="netsplit" function(), 2=specifying the order of treatments in comparisons
#3=character string used in the plot to label the network estimates
#"netsplit" function()>:Methods to split network estimates into the contribution of direct and indirect evidence and to test for local inconsistency in nma

#NOTES: mixed comparisons are always more precise except in cases where the direct and indirect evidence disagree
}
EXAMPLES:BASED:ON:THREE:ARM:STUDIES{getwd()
DATA_FORMAT=contrast-based_format{
#nma with multi-arm studies----
  data ("Senn2013")
  #each row corresponds to a pairwise comparison
subset(Senn2013, studlab %in% c("DeFronzo1995", "Lewin2007", "Willms1999"))
net3 <- netmeta(TE, seTE, treat1.long, treat2.long, studlab = studlab, 
                data = Senn2013, sm = "MD", method.tau = "REML", reference.group = "plac")

netgraph(net3, number.of.studies = TRUE, cex = 1.25, cex.number.of.studies = 1.5)
print(net3)
netleague(net3)
forest(netsplit(net3, order = NULL),
       text.overall = "Mixed treatment estimate")

##nma wt detailed explanation----
print(summary(net3), 
     truncate = studlab %in% c("DeFronzo1995", "Lewin2007", "Willms1999"),
     reference.group = "", all.treatments = TRUE, nchar.trts = 4)
#ARGUMENTS. truncate: only to see certain studies/obj. of focus
#ARGUMENTS. ref="": indicating there is no reference group to be used as a comparator 
#but rather all trts should be compared against themselves (argument "all.trt=TRUE")
#ARGUMENTS. nchar.trts: suggesting that the number of characters for a trt should be limted to 4

###network graph----
#creating a network graph using the "netgraph" function
netgraph(net3, number.of.studies = TRUE, cex = 1.25, cex.number.of.studies = 1.5)
#ARGUMENTS. 1: object, 2: set to specify that the number of studies for each comparison should be displayed in the graph, 3 &4 : specifying the font size and position of treatment labels
###forest plot ----
forest(net3)
}
DATA_FORMAT=wide_arm-based_format{----
#each row corresponds to a single study wt multiple or double comparisons
data(smokingcessation)
head(smokingcessation)
#step 1: transform data into input format (contrast-based format) for netmeta using "pairwise" function
p1 <- pairwise(treat = list(treat1,treat2,treat3),
         event = list(event1,event2,event3), n =list(n1,n2,n3),
         data = smokingcessation, sm="OR")
##ARGUMENTS. 1=treatment or list of treatments, 2=suggesting that our data is with binary outcomes and we need the event or list of events, 
#3=sample size or list of sample sizes, 4=dataset, 5="OR"(Odds ratio for summary measure)
net4 <- netmeta(p1, common = FALSE, reference.group = "A")
netgraph(net4, number.of.studies = TRUE, points = TRUE,
         cex.points = 7, adj = 0.5)
forest(net4, reference.group = c("A", "D"), drop.reference.group = TRUE,
       label.left = "Favours second intervention",
       label.right = "Favours first intervention")
forest(netsplit(net4, order = NULL),
       text.overall = "Mixed treatment estimate")
forest(net4)
}
DATA_FORMAT=long_arm-based_format{
  #each row corresponds to information on a single treatment arm
  #has a high rob = risk of bias assessment
library("readxl")
  AcuteMania <- as.data.frame(read_excel("AcuteMania.xls"))
  p2 <- pairwise(treat = treatment,
                 event = r, n = n, studlab = studyid,
                 data = AcuteMania, sm="OR")
  net5 <- netmeta(p2, common = FALSE, reference.group = "PLA")
  netgraph(net5, number.of.studies = TRUE, seq = "optimal",
           rotate = -360 * (5 / net5$n))
  forest(net5)
  forest(net5, reference.group = c("PLA", "CARB"), drop.reference.group = TRUE,
         label.left = "Favours second intervention",
         label.right = "Favours first intervention")
  forest(netsplit(net5, order = NULL),
         text.overall = "Mixed treatment estimate")
}
}
data.frame(arms = p <- 1:6, comps = choose(p,2), df = p-1)
#2 approaches. 1)standard (wt one baseline treatment) or alternative (reducing weight of all comparisons by half)

#Next steps in MNA after netgraph and forest plot: ranking of treatments, generation of league tables, evaluation of inconsistency
#limitations of R package netmeta: no methods for subgroup analysis or network meta-regression.
#what can be done however is a subset analysis (with the subset argument shown)

USING.OUR.DATA.(Vasquez.et.al){
#----long arm based format where each row corresponds to information on a single treatment
#Effect of nutrient addition on Aboveground biomass
Vasquez <- as.data.frame(read_excel("SuppMat_Vázquez_11104_2023_6083_MOESM2_ESM.xlsx", sheet = 2))
Vasquez <- Vasquez[(1:348), ]
library(dplyr)

mean_biomass <- Vasquez %>% group_by(Site, Year, Treatments) %>%
  summarise(r = mean(`Aboveground biomass (g m-2)`)) 
mean_biomass$r <- round(mean_biomass$r,0)

#cdpt.us <- filter(mean_biomass, Site == "cdpt.us")
#cdpt.us %>% group_by(Treatments) %>%
 # summarise(r = mean(r), sd = sd(r)) 
  
 # summarise(r = mean(r), sd = sd(r))
#sd(mean_biomass$r)
  
#colnames(mean_biomass)[4] <- "r"
#library("writexl")
#write_xlsx(mean_biomass, "C:/Users/CP24/Desktop/INTERNSHIP-UFZ/mean_biomass.xlsx")
#getwd()

{
AGBiomass <- as.data.frame(read_excel("mean_biomass.xlsx"))
results <- AGBiomass %>%
  group_by(Site, Treatments) %>%
  summarise(Mean = round(mean(r, na.rm = TRUE), digits=1),
    SD = round(sd(r, na.rm = TRUE), digits=1))
results <- results %>% mutate(n = 4.5)
#created a new column called n for the number of sample sizes

p3 <- pairwise(treat = Treatments, n = n, mean = Mean,
               sd = SD, studlab = Site,
               data = results, sm = "SMD")
#our data is with continuous outcomes so instead of the event or list of events as in binary outcomes, 
#we make use of both mean values and standard deviation
gs("smcont")
#To get the statistical tool used in calculating summary measure

net6 <- netmeta(p3, common = FALSE, reference.group = "Control")
netgraph(net6, number.of.studies = TRUE, seq = "optimal",
         offset = 0.03,lwd = 2, 
         rotate = -360 * (5 / net5$n))
#Arguments: 3=specifying that we want the treatment in the netgraph to be arranged in an optimal way, 
#6=argument used in rotating the netgraph clockwise or anti-clockwise

forest(net6)
forest(net6, reference.group = c("Con","NP"), drop.reference.group = TRUE,
       label.left = "Favours second intervention",
       label.right = "Favours first intervention")
forest(netsplit(net6, order = NULL),
       text.overall = "Mixed treatment estimate")

forest(net6, sortvar = TE,
       leftcols = c("studlab", "k"),
       leftlabs = c("Nutrient", "Direct\nstudies"),
       xlab = "Aboveground biomass")
#sortvar: sorts according to increasing treatment efficacy
#leftcols and leftlabels: add directly to the forest plot, the number of comparisons 

forest(net6, reference.group = "NP",
       label.left = "favours a combination of N and P",
       label.right = "favours other treatment")
}
{
  results2 <- mean_biomass %>%
    group_by(Site, Treatments) %>%
    summarise(Mean = round(mean(r, na.rm = TRUE), digits=1),
              SD = round(sd(r, na.rm = TRUE), digits=1))
  results2 <- results2 %>% mutate(n = 4.5)
  #created a new column called n for the number of sample sizes
  
  p4 <- pairwise(treat = Treatments, n = n, mean = Mean,
                 sd = SD, studlab = Site,
                 data = results2, sm = "SMD")
  gs("smcont")
  #To get the statistical tool used in calculating summary measure
  
net7 <- netmeta(p4, common = FALSE, reference.group = "Control")
  
png(filename = "netgraph-Abovegroundbiomass_Vasquez.png",
      width = 3.5, height = 3.5, units = "in", pointsize = 12,
      bg = "white", res = 72)
netgraph(net7, number.of.studies = TRUE, seq = "optimal",
           offset = 0.03,lwd = 2, 
           rotate = -360 * (5 / net5$n))
invisible(dev.off())

  
forest(net7)
png(filename = "forest-Abovegroundbiomass_Vasquez.png",
      width = 6.5, height = 3.5, units = "in", pointsize = 12,
      bg = "white", res = 72)
  
forest(net7, sortvar = TE,
         leftcols = c("studlab", "k"),
         leftlabs = c("Nutrient", "Direct\nstudies"),
         xlab = "Aboveground biomass")
#sortvar: sorts according to increasing treatment efficacy
#leftcols and leftlabels: add directly to the forest plot, the number of comparisons 

invisible(dev.off())
}
}
net6$designs  #to get the list of designs
with(net6, table(paste0(treat1, sep.trts, treat2))) 
#also to get the list of designs but with more information such as the number of study per pairwise comparison
net6$A.matrix
#same information as above but presented in a matrix

wide_arm-based_format2{----
data("Stowe2010")
}
long_arm-based_format2{----
    data("Dogliotti2014")
  data("Baker2009")
}

#NMA using the yield dataset
#testing the effects of different treatments on crop yield
#first step is loading the dataset
library(readxl)
yield_data <- as.data.frame(read_excel("Graduate Assistant position/Cleaned_dataset.xlsx", sheet = 1))
View(yield_data)
#The data_FORMAT is wide_arm-based_format where each row corresponds to a single study wt multiple or double comparisons
head(yield_data)
list(yield_data)
dim(yield_data)
str(yield_data)
summary(yield_data)
class(yield_data)
#step 2: transform data into input format (contrast-based format) for netmeta using "pairwise" function
# This step is also used to to estimate treatment effects and corresponding standard errors.
# Additionally, we use the pairwise() function to organise the data / calculate pairwise comparisons
p1 <- pairwise(treat = list(pr_Treatment, pr_control),
               n = list(n_treatment, n_control),
               mean = list(pr_yield_treatm_kgha, pr_yield_control_kgha),
               sd = list(treatment_SD, control_SD),
               studylab = "Source",
               data = yield_data, sm="SMD")
##ARGUMENTS. 1=treatment or list of treatments, 2=sample size or list of sample sizes, 3=treatment means or a list of treatment means, 
#4=standard deviations (sd) or a or list of the standard deviations, 5=study labels, 6=dataset, and 7=summary measure in this case"SMD"(Standardized mean differences).

#Results / what do we see
# An R object p1 which is a data frame with 3939 rows, each corresponding to a pairwise comparison
# (3938 rows from 3938 two-arm studies), and 32 columns (variables).
# The first two variables contain the mean differences (TE) and corresponding standard errors (seTE) of the pairwise comparison defined by variables treat1 and treat2. 

#step 3: Performing the main analysis . A standard NMA
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

#Next step is to present the result which include steps such as;
#1. Visualizing the evidence, 2. visualizing treatment effects, 3. ranking the treatments

x11()
options(bitmapType = "cairo")
#step 4: Graphically visualizing the evidence. This is done by constructing a network graph (function "netgraph") in order to get an overview of the network structure
netgraph(net1, points = TRUE, 
         cex.points = 7, adj = 0.5)
netgraph(net1, number.of.studies = TRUE,
         seq = "optimal",
         points = TRUE, offset = 0.03,
         cex.points = 7, adj = 0.5, cex = 1.5)

netgraph(net1, number.of.studies = TRUE)
#number.of.studies = TRUE makes the number blurry
#lwd = 2: allows the width of each line to correspond to the number of studies 
$nodes
#what do we see
#In the network graph, each treatment is represented by a point (node) in the plane
#furthermore, treatments are connected by a line (edge), if at least one direct pairwise comparison exists.
#our graph also visualizes the number of studies contributing to each pairwise comparison
#the thickness of the edges (line) are proportional to the number of studies directly comparing treatments.

#step 5: Visualizing the treatment effects
#Drawing a forest plot for network meta-analysis helps to summarize results
forest(net1, reference.group = "Control", drop.reference.group = TRUE,
       label.left = "Favours second intervention",
       label.right = "Favours first intervention")
forest(netsplit(net1, order = NULL), show = "all",
       text.overall = "Mixed treatment estimate")
forest(net1)
# Forest plot with placebo as reference
forest(net1, ref = c("L", "Straw return", "Super Absorbent Polymer (SAP)"), baseline = FALSE, drop = TRUE)
# Forest plot with an active treatment versus all other treatments

#league table
netleague(net1)

# step 6: Ranking of the treatments evaluated in an NMA provides additional information on the merits of individual treatments
netrank(net1)
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


# step 7: Evaluation of Heterogeneity. The decomposition of the overall Q statistic to assess heterogeneity and inconsistency
decomp.design(net1)

#what do we see
# The design-specific decomposition of QW shows that the within-design heterogeneity can largely be traced back to
# the comparison of Control versus Straw return and Control versus Liming.
# Results for detaching single designs and the full design-by-treatment interaction model do not show evidence of inconsistency between designs.

# step 8: Evaluation of inconsistency. the comparison of direct and indirect evidence for each pairwise comparison.
netsplit(net1)
netheat(net1)

#step 9: Evaluation of funnel plot assymetry
funnel()

#Test Summary
#Based on the results of the tests for heterogeneity and inconsistency, we observe; 
#that there is significant variation among studies (this is supported by the high value of I² and the significance of the Q value. 
#Given this, we performed the following recommended steps:Investigating sources of Heterogeneity:
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

#Meta regression 
