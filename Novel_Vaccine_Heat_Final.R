# ---
# title: "Novel_Vaccine_Line"
# author: "K. Anderson"
# date: "2023-09-27"
# output:
#   html_document:
#     html_document: null
#     toc: yes
#     toc_depth: 2
#     toc_float: yes
#     code_folding: hide
#   pdf_document:
#     toc: yes
#     toc_depth: '2'
# ---
# 
# ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)
# ```

### For line plots in "Exploring the effects of cultural transmission and decision-making biases on the acceptance of novel vaccines"


# #### Matrix of individuals
# [Vaccination state (V), Attitude state (A), Disease state (D), Bias state (B)]
# 
# 
# #### Initialize this matrix
# -- Begin with unvaccinated population
# -- Begin with certain percentage of people with confident (A^+) attitude
# -- Desease state also pre-specified
# 
# ##### Bias options:
# -- conformity (B^1) = more likely to choose the majority
# -- novelty bias (B^-1) = more likely to choose the minority view
# -- neutral (B^0) = probability of changing mind scales with number of people with a different attitude 

library(magick)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gganimate)
library(corrplot)
library(fields)

seed=1112
set.seed(seed)

source('Novel_Vaccine_Vars_Final.R')  #Script creating bias combination list

#Output directories
dir_out <- "NovelVaccineOut"
dir_out_pdf <- "NovelVaccineOutPdf"


### For computer
# #Folders for movie images "novelvax" and graph pdfs "novelvaxpdf"
# dir_out <- file.path()#
# #dir.create(dir_out, recursive = TRUE)
# dir_out_pdf <- file.path()#
# #dir.create(dir_out_pdf, recursive = TRUE)

Vars = template_list #List of parameters combinations, varying by bias, used for testing
BiasVars = c(1, 11, 62, 63) #Specified rows from Vars

for (xg in 1:length(BiasVars)){

NumTimesteps = 100 #Number of timesteps

#Population size and homophily specification
sizei <-40
sizej <-25
Homophily <- 1 #== 1 if homophily biases movements; 0 if random movements

runend = 10 #Number of runs
  
g <- BiasVars[xg] #Row number in Vars

print(g) #used for progress tracking

#pdf
outname <- paste("927_AvgHtmp_kk_conf_kk_hes",seed,"g2",g, "move", runend, "Homophily", Homophily,sep="_")
pdf(file.path(dir_out_pdf,paste0(outname,".pdf")),height=9, width=8.2)#saves file as "noveltest.pdf"
par(mfrow=c(2,2))

#Axes intervals
  Vector_A <- seq(0,1,0.1)
  Vector_B <- seq(0,1,0.1)
#Initializing Vaccination and Confidence matrices 
  matrix_V =array(data=0, dim=c(length(Vector_A),length(Vector_B)))
  matrix_A =array(data=0, dim=c(length(Vector_A),length(Vector_B)))

 
  for (aw in 1:length(Vector_A)){ #For graphing: aw- y label bw x label
    for (bw in 1:length(Vector_B)){
    
      #print(c(aw,bw)) For progress tracking
  
  ### Heatmap  Tests 
      
      #test_name <- 1 # Probability of Infection vs Vaccinated Infection Probability
       # Prob_of_infect <- Vector_A[aw]
       # Vaccinated_Disease_threshold <- Vector_B[bw]
    
    # #test_name <- 2 # Hesitancy Influencer (Trait) vs Confident Influencer (Trait)
       # influencer_hes <- c(0,round(Vector_A[aw]*(sizei*sizej))) #Change one of these numbers
       # influencer_conf <- c(1,round(Vector_B[bw]*(sizei*sizej)))
     
    # test_name <- 3
      #Attitude_threshold = Vector_A[aw]
      #Vaccinated_Disease_threshold = Vector_B[bw]
      #OR
      #Disease_threshold = Vector_B[bw]
    
      ##test_name <- 4 
      kk_conf <- Vector_A[aw] #Confident Influencer weight
      kk_hes <- Vector_A[bw]  #Hesitant Influencer weight
      
#Arrays for collecting Confidence, Vaccination, Infected and Recovered frequencies over time per run 
 
  #Confidence
  ATT_run = array(data = 0, dim = c(runend,NumTimesteps))
  ATT_run_avg = array(data = 0, dim = c(1,NumTimesteps))
  #Vaccination
  VACC_run = array(data = 0, dim = c(runend,NumTimesteps))
  VACC_run_avg = array(data = 0, dim = c(1,NumTimesteps))
  #Infected
  DIS_run = array(data = 0, dim = c(runend,NumTimesteps))
  DIS_run_avg = array(data = 0, dim = c(1,NumTimesteps))
  #Recovered
  Recov_run = array(data = 0, dim = c(runend,NumTimesteps))
  Recov_run_avg = array(data = 0, dim = c(1,NumTimesteps))
  
  #g <- BiasVars[xg]
  #plot(NA, NA,main = c(paste(as.vector(Vars[[g]]),collapse = " "),paste(c(c("H ",Homophily), c("Vars", g),c("HT ", avg_Herd_time)),collapse = " ")),xlim = c(0, NumTimesteps),xlab = "Time", ylim = 0:1, ylab = "Frequency")
  
for (run in 1:runend){
 
# Influencer specifications - set each run
# c(attitude state, reach from specified parameter list*population size)   
influencer_hes <- c(0,round(Vars[[g]][8]*(sizei*sizej))) #Change one of these numbers
influencer_conf <- c(1,round(Vars[[g]][9]*(sizei*sizej)))

# #Initializing follower array. New followers are chosen each timestep
# followers_conf <- array(data = 0, c(1,2)) #moved here 6/27/2023
# followers_hes <- array(data = 0, c(1,2))  
    
ATT = array(data = 0)#Attitude;for plotting
Time = array(data =0)#Time;for plotting
VACC = array(data =0)#Vacc. Freq;for plotting
Disease = array(data =0)#Infected;for plotting
Recovered = array(data =0)#Recovered;for plotting

Tcontagious = array(data = 0) #

Individual_matrix=array(data=0, dim=c(sizei,sizej,4) )

#addvars = array(data = 0, dim = c(size[1],2))#collects the final V and A for each line in var matrix
addvars <- list()
addvarslist <- array(data= 0, dim = c(10,2))

#g <- random_integers[rn] # row number in vars indexed by rn
  #print(g)
  
#for (g in 1:length(Vars)){#varsize #size[1] # #of rows in vars


#Parameter assignments and variable initialization
#initial vaccination state is 0, i.e. Individual_matrix[i,j,1]=0
#Vars = c(0.1, 0.25, 0.26, 0.17, B-, B0, B+, 0.48, 0.28, 0.19)
Attitude_threshold = Vars[[g]][1]#Confidence Frequency; Vars[g,1]
Disease_threshold = Vars[[g]][2]#Infected;Vars[g,2]
Vaccinated_Disease_threshold = Vars[[g]][3]#Infection Probability if vaccinated; Vars[g,3]
Infected_Disease_threshold = Vars[[g]][4]#Infection probability if previously infected; Vars[g,4]
CulturalBias <- c(-1, 0 , 1) #(novelty, neutral, conform)
BiasProb <- c(Vars[[g]][5],Vars[[g]][6],Vars[[g]][7])#Bias Proportions; Vars[g,5], Vars[g,6], Vars[g,7]
Prob_of_infect = Vars[[g]][10]# Probability of infection if susceptible (never infected/unvaccinated)


# ##Initialization Step##

#keep Individual_matrix[i,j,1]=0 because nobody is vaccinated yet
for (i in 1:sizei){#go through each agent in the matrix
  for (j in 1:sizej){
    #print(i, j) #for tracking
    
    #initialize attitude state
    random_number=runif(1)#pick one random number from uniform distribution
    if (random_number < Attitude_threshold){# if above number < threshold
      Individual_matrix[i,j,2]=1 #Assign positive attitude
      }
    #initialize disease state
    random_number2=runif(1)
    if (random_number2 < Disease_threshold){
        Individual_matrix[i,j,3]=1# Assign infected
    }
    #Assigning cultural bias based on probability
    Individual_matrix[i,j,4]=sample(CulturalBias,1,prob=BiasProb)
    
  
 }#j
}#i #End of Initialization

# Initial frequencies for plotting
init_tot_vacc = sum(Individual_matrix[,,1])/(sizei*sizej)#Vaccinated
init_tot_conf = sum(Individual_matrix[,,2])/(sizei*sizej)#Confident
init_tot_infct = sum(Individual_matrix[,,3][Individual_matrix[,,3]>0])/(sizei*sizej)#Infected
init_tot_recov = abs(sum(Individual_matrix[,,3][Individual_matrix[,,3]<0]))/(sizei*sizej)#Recovered


## Edge Adjustment such that agents on the edge of the matrix have neighbors on all sides ##
starti <- i-1 #assigning name to possible "0" index
if (starti == 0){
  newstarti = sizei#currently name of matrix length
  starti <- newstarti #starti is now end value instead of 0
}

endi <- i+1 #end: #assigning name to possible "end+1" index
if (endi == sizei+1){#"end" +1; when
  newendi = 1 #back to beginning index
  endi <- newendi
}

startj <- j-1 #Same done for j
if (startj == 0){
  newstartj = sizej
  startj <- newstartj
}

endj <- j+1
if (endj == sizej +1){#"end" +1; when
  newendj = 1
  endj <- newendj
}


##Post-initialization Timesteps
for (t in 1:NumTimesteps){
 
#Adjusting matrix for image
 rotate <- function(x) t(apply(x, 2, rev))
 rotate2 <- function(x) (apply(x, 2, rev))
 fgh <-rotate(Individual_matrix[,,3])

 
###Impact of influencer###
   
 #Initializing follower array. New followers are chosen each timestep
 followers_conf <- array(data = 0, c(1,2)) #moved here 6/27/2023
 followers_hes <- array(data = 0, c(1,2))

  if (influencer_conf[2] == 0){# If the confident influencer has no reach (no followers)
             followers_conf <- array(data = 0, c(1,2))#...keep array empty
                } else { # else scan the population to fill influencer array

     for (scan in 1:influencer_conf[2]){#Creates list of influencer_conf followers
        
        #Chooses agents at random
        follower_conf_i=round(runif(1,1,sizei))
        follower_conf_j=round(runif(1,1,sizej))


    ##if an agent is already a follower (row with (i,j) is already in list) -- skip agent
     if (any(apply(followers_conf, 1, function(x) all(x == c(follower_conf_i,follower_conf_j))))== TRUE){
      
       } else {
         
          #if not already in followers (gives FALSE), add to list
           followers_conf <- rbind(followers_conf,c(follower_conf_i,follower_conf_j))
            }
 
      }## scan end
 
                  
   }#if else influencer_conf[2] == 0 
                  
                  
 # Repeating process for hesitant influencer 
 if (influencer_hes[2] == 0){# If influencer has no reach, keep array empty
      followers_hes <- array(data = 0, c(1,2))
    }else{
  
    for (scan2 in 1:influencer_hes[2]){#Creates list of influencer_hes followers

    follower_hes_i=round(runif(1,1,sizei)) 
    follower_hes_j=round(runif(1,1,sizej))


    # skip agent if already a follower
    if (any(apply(followers_hes, 1, function(x) all(x == c(follower_hes_i,follower_hes_j))))== TRUE){
      
    }else {
      
      #if not already in followers (above gives FALSE), add to list
      followers_hes <- rbind(followers_hes,c(follower_hes_i,follower_hes_j))# should this be c(i,j)
    }
    
  }##scan2 end
     
        }#if else influencer_hes[2] == 0 
 
  
##Attitude Transition## 
  for (i in 1:sizei){
    for (j in 1:sizej){
      
      #Checks if an agent is a follower of either or both influencers and assigns name
       in_followers_conf <- any((followers_conf[, 1]== i) * (followers_conf[, 2] == j))
       in_followers_hes <- any((followers_hes[, 1]== i) * (followers_hes[, 2] == j))
  
  
       #tally the A+ attitudes around agent  
        SumOfPostitives=Individual_matrix[starti,startj,2]+Individual_matrix[starti,j,2]+Individual_matrix[starti,endj,2]+
        Individual_matrix[i,startj,2]+Individual_matrix[i,endj,2]+
        Individual_matrix[endi,startj,2]+Individual_matrix[endi,j,2]+Individual_matrix[endi,endj,2]
 
      #Bias determines attitude transition probability equation  
      #Cultural Bias index used to assign prob_of_change eqn
      #Cultural Bias (-1, 0, 1); k = 1, 2 or 3
      k = match(Individual_matrix[i,j,4], CulturalBias)
     
      kk = 0.14  #Weight of Influencer effects
   
     ### Calculate initial probability of changing attitudes for all confident (i,j)###
        
##If agent is Confident     
  if(Individual_matrix[i,j,2]==1){#if agent is confident
       
        if (k==1){#if agent holds novelty bias
          
          prob_of_change = 0.002 + (0.99/(1 + exp(-13*((SumOfPostitives/8)-0.5))))
       
        }
      
        if (k==2){#if neutral bias
          
          prob_of_change = -0.075*(SumOfPostitives) + 0.8
         
        }
        
        if (k==3){ #if conformity bias
        
            prob_of_change = 0.99 - 0.99/(1+ exp(-13*((SumOfPostitives/8)-0.5)))
        
        }
     
  ### Recalculate probability of mind change based on influencer effects
        
      #If confident and only following confident --> reduce probability of change by kk
          if (in_followers_conf ==TRUE & in_followers_hes == FALSE){
            
            #kk = kk_conf
            
            yy <- prob_of_change*(1-kk)
              
          }
        
        
      #if confident and only following hesitant -> increase prob of change
          if (in_followers_conf ==FALSE & in_followers_hes == TRUE){
            
            #kk = kk_hes
            
            yy <- kk + (1-kk)*prob_of_change
           
        }
        
      #if confident and following both confident and hesitant --> reduce probability of change
          if (in_followers_conf ==TRUE & in_followers_hes == TRUE){
         
            #kk = sum(kk_conf, kk_hes)/2
            
             yy = (1-kk/2)*(prob_of_change)
              
          }
        
    #if agent follows neither influencer -> no changes to calculated probability
          if (in_followers_conf == FALSE & in_followers_hes == FALSE){
          
              yy <- prob_of_change
        } 
       
      ##adding stochasticity 
       individual_prob=runif(1)
       
           if(individual_prob < yy){
          
          Individual_matrix[i,j,2]= 0
        }
      
      }#end: if confident
      
  ### Calculate initial probability of changing attitudes for all hesitant (i,j)###      
      
##If agent is Hesitant    
    if (Individual_matrix[i,j,2]==0){#if hesitant
        
        if (k==1){#if agent holds novelty bias
          
           prob_of_change = 0.99 - 0.99/(1+ exp(-13*((SumOfPostitives/8)-0.5)))
        }
        
        if (k==2){#if neutral bias
          
          prob_of_change = 0.075*(SumOfPostitives) + 0.2
          
          }
        
        if (k==3){ #if conformity bias
        
        prob_of_change = 0.002 + 0.99/(1+ exp(-13*((SumOfPostitives/8)-0.5)))
        }
      
  ### Recalculate probability of mind change based on influencer effects
      
  #if agent is hesitant and only follows confident -> increase prob of change by some factor determined by kk
        if (in_followers_conf == TRUE & in_followers_hes == FALSE){
          
            #kk = kk_conf
            
              yy <- kk + (1-kk)*prob_of_change
              
                  } #If hesitant and... end

      # if hesitant agent only follows hesitant
        if (in_followers_conf == FALSE & in_followers_hes == TRUE){
          
            kk = kk_hes
            
            yy <- prob_of_change*(1-kk)
          
            }

      # if agent follows both confident and hesitant -> reduced prob of change
          if (in_followers_conf ==TRUE & in_followers_hes == TRUE){
         
            kk = sum(kk_conf, kk_hes)/2
            
                yy <- (1-kk/2)*(prob_of_change)
            
                  }
      #if agent follows neither influencer -> changes to calculated probability
          if (in_followers_conf == FALSE & in_followers_hes == FALSE){
                   
           yy <- prob_of_change
           
           }

       # adding a chance that individual changes mind opposed to calculated probability 
         individual_prob=runif(1)
        
        if(yy > individual_prob){#prob_of_change >
          Individual_matrix[i,j,2]=1
        }
        
      }#end if hesitant
    
      
###Probability that agent[i,j] gets vaccinated###
##Based on surrounding infected and vaccinated, attitude and bias
 
      #Summing the number if infected individuals surrounding an agent
      infected_list <- c(Individual_matrix[starti,startj,3], Individual_matrix[starti,j,3], Individual_matrix[starti,endj,3],
                        Individual_matrix[i,startj,3], Individual_matrix[i,endj,3], Individual_matrix[endi,startj,3], Individual_matrix[endi,j,3], Individual_matrix[endi,endj,3])
      
      SumOfInfected = sum(infected_list[infected_list>0])
      
    #Summing the number of vaccinated individuals surrounding the agent
      SumOfVaccinated=Individual_matrix[starti,startj,1]+Individual_matrix[starti,j,1]+Individual_matrix[starti,endj,1]+
        Individual_matrix[i,startj,1]+Individual_matrix[i,endj,1]+
        Individual_matrix[endi,startj,1]+Individual_matrix[endi,j,1]+Individual_matrix[endi,endj,1]

        
      if (Individual_matrix[i,j,1]==0){# if agent is unvaccinated
        
        if (Individual_matrix[i,j,2]== 1){# if confident
          
          prob_of_vacc =  (1.01 - exp(-0.35* SumOfInfected))/2 + 0.5
        
              if (k==1){#if agent holds novelty bias
              
              prob_of_vacc2 = 0.99 - 0.5/(1+ exp(-13*((SumOfVaccinated/8)-0.5)))
          }
          
          if (k==2){#if neutral bias

              prob_of_vacc2 = 0.05*(SumOfVaccinated) + 0.55
            
          }
          
          if (k==3){ #if conformity bias
            
             prob_of_vacc2 = 0.5+ 0.49/(1+ exp(-13*((SumOfVaccinated/8)-0.5)))
            
               }
          
      }#end if confident
        
        if  (Individual_matrix[i,j,2]== 0){#if hesitant
          
          prob_of_vacc = (1.2 - exp(-0.35* SumOfInfected))/2 + 0.1
          
        if (k==1){#if agent holds novelty bias
          
          prob_of_vacc2 = 0.5 - 0.5/(1+ exp(-13*((SumOfVaccinated/8)-0.5)))
        }
        
        if (k==2){#if neutral bias

          prob_of_vacc2 = 0.05*(SumOfVaccinated) + 0.05
           
        }
        
        if (k==3){ #if conformity bias
          
          prob_of_vacc2 = 0.002+ 0.49/(1+ exp(-13*((SumOfVaccinated/8)-0.5)))
          
          }
        
        } #end if hesitant
        
        
        #Calculating final probability of vaccination for timesteps >15
        #Vaccination for timesteps >15 can vary based on agent disease state
        if (Individual_matrix[i,j,4] == 0){Comp_prob_of_vacc = (prob_of_vacc*(Prob_of_infect))*(prob_of_vacc2)*(1-Vaccinated_Disease_threshold)}#Completely susceptible
        if (Individual_matrix[i,j,4] == -1){Comp_prob_of_vacc = (prob_of_vacc*(Infected_Disease_threshold))*(prob_of_vacc2)*(1-Vaccinated_Disease_threshold)}#Recovered (Previously Infected)
        if (Individual_matrix[i,j,4] == 1){Comp_prob_of_vacc = 0} #Currently infected

        # Calculation vaccination probability for timestep < 15 (Dampen early vaccination uptake)
        if (t<15){Comp_prob_of_vacc <- Comp_prob_of_vacc*(t/20)}
        
        individual_prob = runif(1)
        

        if(individual_prob < Comp_prob_of_vacc){
            Individual_matrix[i,j,1]=1
          } 
        
        }#if unvaccinated end
      
      }#j end
  }#i end
  

##Disease Transmission##

 #Initializing exposed and contagious arrays 
 exposed = array(data = 0, c(1,2))#Exposed individuals are at risk of contracting the disease
 contagious = array(data = 0 , c(1,3)) #Contagious individual have the disease
 
for (i in 1:sizei){
  for (j in 1:sizej){
    
    if (Individual_matrix[i,j,3]==1){# if infected 
     
       contagious <- rbind(contagious,c(i,j,t))#collects position (i,j) and infected timestep
  
       }
    
    if (Individual_matrix[i,j,3]==0 || Individual_matrix[i,j,3]==-1 ){# if no disease (never infected or recovered)
      
      
         if (SumOfInfected >0){
        
              exposed <- rbind(exposed,c(i,j))
             }
      }
    }#j
  }#i #End of collecting exposed and contagious
  
if (length(exposed[,1])>1){
  
  for (h in 2:length(exposed[,1])){#going through exposed
  
  random_number2=runif(1)
  
## If not vaccinated --> chance of being infected ##
 if (Individual_matrix[exposed[h,1],exposed[h,2],1]==0){
   
 # if not vaccinated but previously infected
 if (Individual_matrix[exposed[h,1],exposed[h,2],3]== -1 & random_number2 < Infected_Disease_threshold){
   
   Individual_matrix[exposed[h,1],exposed[h,2],3]=1 # then infect
 }
   
 # if not vaccinated and completely susceptible
 if (Individual_matrix[exposed[h,1],exposed[h,2],3]== 0 & random_number2 < Prob_of_infect){
   Individual_matrix[exposed[h,1],exposed[h,2],3]=1 #then infect
    }
 }
  
  
## If Vaccinated ##
  if (Individual_matrix[exposed[h,1],exposed[h,2],1]==1){
  #if vaccinated and previously infected
  if (Individual_matrix[exposed[h,1],exposed[h,2],3]==-1 & random_number2 < (Vaccinated_Disease_threshold*Infected_Disease_threshold)){
    Individual_matrix[exposed[h,1],exposed[h,2],3]=1 #then infect
  }
  
  #if vaccinated and susceptible
  if (Individual_matrix[exposed[h,1],exposed[h,2],3]==0 & random_number2 < Vaccinated_Disease_threshold){
   
    Individual_matrix[exposed[h,1],exposed[h,2],3]=1 #then infect
  
    }#if vacc and suscep
   }# if vacc
  }
}#end of going through exposed 

Tcontagious <- rbind(Tcontagious,contagious)#list of contagious individuals

#Reassign contagious agents (agents are contagious for 2 timesteps)
for (xx in 1: nrow(Tcontagious)){
  
  if (Tcontagious[xx, 3] == t-2){
  
    Individual_matrix[Tcontagious[xx,1],Tcontagious[xx,2],3]= -1 #Recovered assignment
   
    }
  
  } #end of reassign
 

  ########choose people to swap positions
    for (move in 1:round(sizei*sizej/10)){
    
    #choose random first person
    choosei1=round(runif(1,1,sizei))
    choosej1=round(runif(1,1,sizej))
    person1=Individual_matrix[choosei1,choosej1,]

    
    # #choose random second person
    choosei2=round(runif(1,1,sizei))
    choosej2=round(runif(1,1,sizej))
    person2=Individual_matrix[choosei2,choosej2,]
    
    
    #If set to random swap
    if (Homophily==0){# No Homophily- Random Swapping (set at beginning)
     
      swap_random1 = runif(1)
      swap_random2 = runif(1)
      
      if (swap_random1 < swap_random2){
      
      #They swap positions at some probability
      Individual_matrix[choosei1,choosej1,]=person2
      Individual_matrix[choosei2,choosej2,]=person1
      
        }
    }
  
#If set to attitude based relocation (non-random swapping)
    if (Homophily==1){# Attitude based Movement: check potential position for matching beliefs, if the neighbors share attituted, likely to switch
      
    
    #Defining Edge Cases    
      startchoosei1 <- choosei1-1 
        if (startchoosei1 == 0){
        newstartchoosei1 = sizei
        startchoosei1 <- newstartchoosei1
      }
      
      startchoosei2 <- choosei2-1
        if (startchoosei2 == 0){
        newstartchoosei2 = sizei
        startchoosei2 <- newstartchoosei2
      }

      endchoosei1 <- choosei1+1
      if (endchoosei1 == sizei+1){
        newendchoosei1 = 1
        endchoosei1 <- newendchoosei1
      }
      
      endchoosei2 <- choosei2+1
      if (endchoosei2 == sizei+1){
        newendchoosei2 = 1
        endchoosei2 <- newendchoosei2
      }

      startchoosej1 <- choosej1-1
      if (startchoosej1 == 0){
        newstartchoosej1 = sizej
        startchoosej1 <- newstartchoosej1
      }
      
      startchoosej2 <- choosej2-1
      if (startchoosej2 == 0){
        newstartchoosej2 = sizej
        startchoosej2 <- newstartchoosej2
      }
      # 
      endchoosej1 <- choosej1+1
      if (endchoosej1 == sizej +1){
        newendchoosej1 = 1
        endchoosej1 <- newendchoosej1
      }
      
      endchoosej2 <- choosej2+1
      if (endchoosej2 == sizej +1){
        newendchoosej2 = 1
        endchoosej2 <- newendchoosej2
      }
      
      SumOfPostitives_person1=Individual_matrix[startchoosei1,startchoosej1,2]+Individual_matrix[startchoosei1,choosej1,2]+Individual_matrix[startchoosei1,endchoosej1,2]+
        Individual_matrix[choosei1,startchoosej1,2]+Individual_matrix[choosei1,endchoosej1,2]+
        Individual_matrix[endchoosei1,startchoosej1,2]+Individual_matrix[endchoosei1,choosej1,2]+Individual_matrix[endchoosei1,endchoosej1,2]
      
      SumOfPostitives_person2=Individual_matrix[startchoosei2,startchoosej2,2]+Individual_matrix[startchoosei2,choosej2,2]+Individual_matrix[startchoosei2,endchoosej2,2]+
        Individual_matrix[choosei2,startchoosej2,2]+Individual_matrix[choosei2,endchoosej2,2]+
        Individual_matrix[endchoosei2,startchoosej2,2]+Individual_matrix[endchoosei2,choosej2,2]+Individual_matrix[endchoosei2,endchoosej2,2]
        
      if (SumOfPostitives_person1 < SumOfPostitives_person2 ){
       
           if (Individual_matrix[choosei1,choosej1, 2]==1 & Individual_matrix[choosei2,choosej2, 2]==0){#modified to include & 4-21-23
       
               Individual_matrix[choosei1,choosej1,]=person2
               Individual_matrix[choosei2,choosej2,]=person1
       
         collect_conf = collect_conf+1 #collect counting swaps?? (A+)person1 or  A- person2
         #print(c("h",collect))
           } 
           #if (Individual_matrix[choosei1,choosej1, 2]==1 & Individual_matrix[choosei2,choosej2, 2]==1){}#"do nothing"added 4-21-23
         }
         
     if (SumOfPostitives_person1 > SumOfPostitives_person2 ){
      
      if (Individual_matrix[choosei1,choosej1, 2]==0 & Individual_matrix[choosei2,choosej2, 2]==1){
       
            Individual_matrix[choosei1,choosej1,]=person2
            Individual_matrix[choosei2,choosej2,]=person1
           
            collect_hes = collect_hes+1 # collect2(A- swaps)
            # print(c("f",collect2))
        }
        #if (Individual_matrix[choosei1,choosej1, 2]==0 & Individual_matrix[choosei2,choosej2, 2]==0){} 
      
      }
      
    }# If Homophily =1
   
    #prev_moved[,,] <- c(person1, person2) 
    
    }#move swapping 

  
Time[t] <-t
ATT[t] <- sum(Individual_matrix[,,2])/(sizei*sizej) #size^2
VACC[t] <- sum(Individual_matrix[,,1])/(sizei*sizej)
Disease[t] <- sum(Individual_matrix[,,3][Individual_matrix[,,3]>0])/(sizei*sizej)#sum(Individual_matrix[,,3])/(sizei*sizej)### Edit to leave out recovered
Recovered[t] <- abs(sum(Individual_matrix[,,3][Individual_matrix[,,3]<0]))/(sizei*sizej)
#print(VACC[t])

#ATT_run[xg] <- ATT

#counting Timesteps to Herd Immunity
# if (VACC[t] >= 0.7){
# 
#   # Herd_time[w,t] <- t
#   # print(VACC[t])
# 
#   }
#   
 if (VACC[t] >= .70){
     if (VACC[t-1] < .70){
       Herd_time <- t
       #print(c(g,Herd_time))
      }
 }

if (t == NumTimesteps & VACC[t] < .70){
  
  Herd_time <- 'NULL'
  
}
   
   


# for (Individual_matrix[,,4]== 0){}
# for (Individual_matrix[,,4]== 1){}

#print(collect)
collect_array[t] <- collect_conf#/(collect+collect2)#(sizei*sizej)#/100 or /sum of total swaps
collect2_array[t] <- collect_hes#/(collect+collect2)#(sizei*sizej)
Totalswaps[t] <- collect_conf + collect_hes

# rndmcllctarr_conf[t] <-rndmcollect_conf #Check: Confident who swap
# rndmcllctarr_hes[t] <- rndmcollect_hes# Hesitant who swap
# Totrndmcollect[t] <- rndmcollect_conf + rndmcollect_hes #Total swaps

# print(collect_array)
# print(collect2)

 #print(ATT)
   # print("Positive Attitude")
   # print(sum(Individual_matrix[,,2])/size^2)# 10000 = size^2 for size = 100
  # print("Vaccinated")
  # print(sum(Individual_matrix[,,1])/size^2)
  #TotalVaccinated[t]=sum(Individual_matrix[,,2])
  #TotalPositiveBelief[t]=sum(Individual_matrix[,,1])
 
 #Tally vaccination status and belief status for conformity, neutral, novelty biased individuals separately
 
  #}, interval = 1, movie.name = "noveldemo.gif", ani.width = 1000, ani.height = 1000)
   
 #}
 
 #print(Individual_matrix[,,1])
#if (Individual_matrix[,,4] == -1){
  #vaccsum_novelty[t] <- sum(Individual_matrix[,,1])/(sizei*sizej)#size^2
  # unvaccsum_novelty[t] <- 1-vaccsum_novelty[t]
  # print(unvaccsum_novelty[t])
  #print(vaccsum_novelty[t])
#print(Individual_matrix[,,3])
#print(VACC)

##Random person infected every 5 timesteps
if (t %% 5){
  
  Individual_matrix[round(runif(1,1,sizei)),round(runif(1,1,sizej)),3] = 1
 
}



}#t

#}} #a, b

#print(c(g,Herd_time))

finV <- sum(Individual_matrix[,,1])/(sizei*sizej)
finA <- sum(Individual_matrix[,,2])/(sizei*sizej)
#addvars[g,] <- c(finV, finA)
#addvars[[rn]] <- c(finV, finA)
addvars[[g]] <- c(finV, finA)
#addvarslist <- rbind(addvarslist, addvars[[rn]])

ATT_run[run,] <- ATT

VACC_run[run,] <- VACC
DIS_run[run,] <- Disease
Recov_run[run,] <- Recovered



#print(ATT_run)

  } #run 1 : runend
  
  for (colnum in 1:NumTimesteps){
    
    ATT_run_avg[colnum] <- mean(ATT_run[,colnum])
    VACC_run_avg[colnum] <- mean(VACC_run[,colnum])
    DIS_run_avg[colnum] <- mean(DIS_run[,colnum])
    Recov_run_avg[colnum] <- mean(Recov_run[,colnum])
    
    if (VACC_run_avg[colnum] >= .70){
      
      if (VACC_run_avg[colnum-1] < .70){
        
        avg_Herd_time <- colnum
        
      }else if (colnum == NumTimesteps & VACC_run_avg[colnum] < .70){avg_Herd_time <- 0
        #print(c(g,Herd_time))
      }
    }
    
    # if (t == NumTimesteps & VACC[t] < .70){
    #   
    #   Herd_time <- 'NULL'
    #   
    # }
    
  }#colnum
  

  matrix_V[aw,bw] <- VACC_run_avg[,NumTimesteps]
  matrix_A[aw,bw] <- ATT_run_avg[,NumTimesteps]
  
  # print(c(Vector_A[aw], Vector_B[bw]))
  # print(c(matrix_V[aw,bw],matrix_A[aw,bw]))
  
 }#bw
 }#aw
  
###Plots###

# ##Line###
# #Confidence freq. overtime
# 
# #plot(c(0,Time), c(init_tot_conf,ATT),main = c(paste(as.vector(Vars[[g]]),collapse = " "),paste(c(c("H ",Homophily), c("Vars", g), c("HT ", Herd_time)),collapse = " ")),type= "l", col = "blue",lwd = 2, xlim = c(0, NumTimesteps),xlab = "Time", ylim = 0:1, ylab = "Frequency")
# plot(c(0,Time), c(init_tot_conf,ATT_run_avg),main = c(paste(as.vector(Vars[[g]]),collapse = " "),paste(c(c("H ",Homophily), c("Vars", g), c("HT ", avg_Herd_time)),collapse = " ")),type= "l", col = "blue",lwd = 2, xlim = c(0, NumTimesteps),xlab = "Time", ylim = 0:1, ylab = "Frequency")
# #title(main = paste(as.vector(Vars[[g]]),collapse = " "),sub = paste(g))
# axis(side=1, at=seq(0, NumTimesteps, by=10))
# axis(side=2, at=seq(0, 1, by= 0.10))
# abline(h = 0.70, v = avg_Herd_time ,col = 'darkgreen', lwd=2, lty=2)
# #text(90,0.75, "Herd Immunity")
# #Confidence Freq,
# #lines(c(0,Time), c(init_tot_conf,ATT), type= "l", col = "blue",lwd = 2)
# #lines(c(0,Time), c(init_tot_conf,ATT_run_avg), type= "l", col = "blue",lwd = 2)
# #Vaccination freq. overtime
# #lines(c(0,Time), c(init_tot_vacc, VACC), type = "l", lwd = 2)
# lines(c(0,Time), c(init_tot_vacc, VACC_run_avg), type = "l", lwd = 2)
# #Disease Occurrence
# #lines(c(0,Time), c(init_tot_infct,Disease), type = "l", col = "red", lwd = 2)
# lines(c(0,Time), c(init_tot_infct,DIS_run_avg), type = "l", col = "red", lwd = 2)
# #Recovered
# #lines(c(0,Time), c(init_tot_recov,Recovered), type = "l", col = "orange", lwd = 2)
# lines(c(0,Time), c(init_tot_recov,Recov_run_avg), type = "l", col = "orange", lwd = 2)
# #legend("top", c("Confidence (A+)", "Vaccination (V+)", "Disease (D+)", "Recovered (D-)"), fill = c("blue","black","red", "orange"))
# #dev.off



#}#g #run
 
  # ATT_run[run,] <- ATT



######Heatmaps
#####
#image(matrix_A, col = colorRampPalette(c("blue", "white", "red"))(100), xlab = "Columns", ylab = "Rows")
#  pdf(file.path(dir_out_pdf,paste("AvgHeatmap_seed",seed,"g",g,"NumTimesteps",NumTimesteps,"sizei",sizei,"sizej",sizej,"Homophily",Homophily,"runend",runend,".pdf",sep="_")),height=9, width=8.2)#saves file as "noveltest.pdf"
#  par(mfrow=c(2,2))#), mar=c(2,5,5,2) + 0.1)

#aw- y label bw x label
  
my_palette <- colorRampPalette(c("#FFFFFF", "#0000FF")) #Blue
#my_palette2 <- colorRampPalette(c("#FFFFFF", "#FF0000")) #Red
my_palette2 <- colorRampPalette(c("#FFFFFF", "#000000")) #Black

#imagePlot(rotate2(matrix_A), col = my_palette(100), zlim=c(0,1), add=TRUE)#cm.colors(256)
#imagePlot(rotate2(matrix_V), col = my_palette2(100), zlim=c(0,1), add=TRUE)#cm.colors(256)

write.csv(matrix_A, paste0(outname, "_A.csv")) #Influ_weight, Influ_A+

image(t(matrix_A), col = my_palette(100), zlim=c(0,1), ylab="kk_conf", xlab="kk_hes")#cm.colors(256)
#image(t(matrix_A), col = my_palette(100), zlim=c(0,1), ylab="Infl_Reach_A-", xlab="Influ_Reach_A+")#cm.colors(256)
#image(t(matrix_A), col = my_palette(100), zlim=c(0,1), ylab="Probability of Infection (Susceptible)", xlab="Probability of Infection (Vaccinated)")#cm.colors(256)
#axis(3, at=seq(0,1, length=length(Vector_A)), labels=colnames(matrix_A), lwd=0, pos=1.15)
image.plot(legend.only=TRUE, col = my_palette(100), zlim=c(0,1), smallplot = c(.945, .965, .22, .82))

# legend(grconvertX(0.5, "device"), grconvertY(1, "device"),
#        legend = seq(0, 1, 0.1), fill = rev(heat.colors(my_palette2(100))), xpd = NA)
# legend("topright", legend = seq(0, 1, 0.1), fill = rev(heat.colors(my_palette2(100))), bty = "n", title = "Value")

write.csv(matrix_V, paste0(outname, "_V.csv")) #Probability of Infection (Vaccinated)

image(t(matrix_V), col = my_palette2(100) , zlim=c(0,1), ylab="", xlab="kk_hes" )#cm.colors(256)xlab="Influ_Reach_A+"
image.plot(legend.only=TRUE, col = my_palette2(100), zlim=c(0,1), smallplot = c(.94, .96, .22, .82))

#legend("topright", legend = seq(0, 1, 0.1), fill = rev(heat.colors(my_palette2(100))), bty = "n", title = "Value")
#
#


#
dev.off()
#dev.off()
#
} #xg


# #
#
# ## list file names and read in
# imgs <- list.files(dir_out, full.names = TRUE)
# img_list <- lapply(imgs, image_read)
# 
# ## join the images together
# img_joined <- image_join(img_list)
# 
# ## animate at 2 frames per second
# img_animated <- image_animate(img_joined, fps = 4)
# 
# ## view animated image
# img_animated
# 
# ## save to disk
# image_write(image = img_animated,
#             file.path(dir_out,"novelvax42023.gif")) #path = "novelvax.gif")
# 
# #dev.off()
# # # # #


