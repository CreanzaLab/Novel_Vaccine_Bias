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


# Loading data and and setting up .pdf.

library(magick)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)

set.seed(1112)
source('Novel_Vaccine_Vars_Final.R') #Script creating bias combination list

#CDC Data
covid_data=read.csv("trends_in_number_of_covid19_vaccinations_in_the_us.csv", skip =2, header= TRUE)
vacc_data <- covid_data[seq(1,877,7),] #weekly data points of vaccination frequencies

#monthly data points for vaccine confidence frequencies
conf_vector <- c(0.562337741, 0.563890157, 0.560575749, 0.560575749, 0.620087429, 0.620917153, 0.60866963, 0.637597932, 0.631641034, 0.644998168, 0.646375439, 0.640844816, 0.633407578, 0.628911109, 0.623037764, 0.616250954, 0.592019173, 0.60387269, 0.598952125, 0.595468683, 0.581219402, 0.573850863, 0.573069988, 0.583145319, 0.55831107)

#Output directories
dir_out <- "NovelVaccineOut"
dir_out_pdf <- "NovelVaccineOutPdf"

Vars = template_list # List of parameters combinations, varying by bias, used for testing


NumTimesteps = 100 #Number of timesteps 

#Population size and homophily specification
sizei <-40
sizej <-25

Homophily <- 0 #==1 if attitude homophily biases movements; 0 if random movements
per_move = 10 #percent of population relocating

runend = 10 #Number of runs

modelvs = array(data = 0, dim = c(length(Vars),3))# Initialized Matrix [Bias distribution ID, abs(difference between the simulated and primary dose vaccination frequencies), abs(simulated and at least one dose vaccination frequencies)]

new_ATT_run_avg = array(data = 0, dim = c(length(Vars),23))#Initialized array for collection of every 10 simulated average confidence data points for comparison to CDC data.

diff_new_ATT = array(data = 0, c(length(Vars),2)) #Matrix of [Bias distribution ID, abs(difference between the simulated and CDC vaccine confidence frequencies)]


#pdf
outname <- paste("LinePlots_Final_Homophily=", Homophily, "_Move=", per_move, "(2025)")
#paste("LinePlots_Final_Homophily=0_Move=10(2025)") #paste("Test")
pdf(file.path(dir_out_pdf,paste0(outname,".pdf")), height=9, width=12)
par(mfrow=c(2,2))

# dir_out <- file.path()
# #dir.create(dir_out, recursive = TRUE)
# dir_out_pdf <- file.path()
# #dir.create(dir_out_pdf, recursive = TRUE)


# Simulation
for (g in 1:length(Vars)){# Applying each list of parameters varying by bias

  print(g) #used for progress tracking
  
  #Arrays for collecting Confidence, Vaccination, Infected and Recovered frequencies over time per run
  ATT_run = array(data = 0, dim = c(runend,NumTimesteps))
  ATT_run_avg = array(data = 0, dim = c(1,NumTimesteps))
  
  VACC_run = array(data = 0, dim = c(runend,NumTimesteps))
  VACC_run_avg = array(data = 0, dim = c(1,NumTimesteps))
  
  DIS_run = array(data = 0, dim = c(runend,NumTimesteps))
  DIS_run_avg = array(data = 0, dim = c(1,NumTimesteps))
  
  Recov_run = array(data = 0, dim = c(runend,NumTimesteps))
  Recov_run_avg = array(data = 0, dim = c(1,NumTimesteps))
  
  
# Influencer specifications
# c(attitude state, reach from specified parameter list*population size)
influencer_hes <- c(0,round(Vars[[g]][8]*(sizei*sizej))) #Hesitant influencer
influencer_conf <- c(1,round(Vars[[g]][9]*(sizei*sizej)))#Confident influencer


for (run in 1:runend){# Multiple runs used to calculate the average frequencies
  
ATT = array(data = 0)# Confidence
Time = array(data =0)# Time
VACC = array(data =0)# Vaccination Frequency
Disease = array(data =0)# Infected
Recovered = array(data =0) #Recovered

model_complete = array(data =0)# Complete Primary Series (CDC)
model_one = array(data =0) # At least one dose (CDC)

Tcontagious = array(data = 0) # Collection of contagious individuals

avg_Herd_time = array(data = 0) # Average time to herd immunity

Individual_matrix=array(data=0, dim=c(sizei,sizej,4))# Initial empty population matrices

#Parameter assignments and variable initialization
#initial vaccination state is 0, i.e. Individual_matrix[i,j,1]=0
#Vars = c(0.1, 0.25, 0.26, 0.17, B-, B0, B+, 0.48, 0.28, 0.19)
Attitude_threshold = Vars[[g]][1]# Confidence Frequency; Vars[g,1]
Disease_threshold = Vars[[g]][2]#Infected;Vars[g,2]
Vaccinated_Disease_threshold = Vars[[g]][3]#Infection Probability if vaccinated; Vars[g,3]
Infected_Disease_threshold = Vars[[g]][4]#Infection probability if previously infected; Vars[g,4]
CulturalBias <- c(-1, 0 , 1)# novelty, neutral, conform IDs
BiasProb <- c(Vars[[g]][5],Vars[[g]][6],Vars[[g]][7])#Bias Proportions; Vars[g,5], Vars[g,6], Vars[g,7]
Prob_of_infect = Vars[[g]][10]# Probability of infection if susceptible (never infected/unvaccinated)

for (i in 1:sizei){#Going through each agent in the matrix
  for (j in 1:sizej){
    
    #initialize attitude state
    random_number=runif(1)#pick one random number from uniform distribution
    if (random_number < Attitude_threshold){# if random number < threshold
      Individual_matrix[i,j,2]=1 #Assign positive attitude
      }
    #initialize disease state
    random_number2=runif(1)
    if (random_number2 < Disease_threshold){
        Individual_matrix[i,j,3]=1#Assign infected
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
  newstarti = sizei #current name of matrix length
  starti <- newstarti #starti is now end value instead of 0
}

endi <- i+1 #assigning name to possible "end+1" index
if (endi == sizei+1){#if "end" +1 exceeds sizei
  newendi = 1 #back to beginning index
  endi <- newendi #endi + 1 value is now 1
}

#Same for for j index
startj <- j-1 
if (startj == 0){
  newstartj = sizej
  startj <- newstartj
}

endj <- j+1
if (endj == sizej +1){
  newendj = 1
  endj <- newendj
}


##Post-initialization Timesteps
for (t in 1:NumTimesteps){

### Impact of influencer ###

  #Initializing follower array. New followers are chosen each timestep
  followers_conf <- array(data = 0, c(1,2))# Confident influencer followers
  followers_hes <- array(data = 0, c(1,2)) #Hesitant influencer followers


  if (influencer_conf[2] == 0){# If the confident influencer has no reach (no followers)
    followers_conf <- array(data = 0, c(1,2))#...keep array empty
  } else {# else scan the population to fill influencer array
    
    for (scan in 1:influencer_conf[2]){#Creates list of influencer_conf followers
      
      #Chooses agents at random
      follower_conf_i=round(runif(1,1,sizei)) 
      follower_conf_j=round(runif(1,1,sizej))
      
      #if an agent is already a follower (row with (i,j) is already in list) --.no action
      if (any(apply(followers_conf, 1, function(x) all(x == c(follower_conf_i,follower_conf_j))))== TRUE){
  
      } else {
        
        #if not already in followers (above gives FALSE), add to list
         followers_conf <- rbind(followers_conf,c(follower_conf_i,follower_conf_j))
      }
      
      
    }## scan end 
   
  }#if else influencer_conf[2] == 0 
  
  
 # Repeating process for hesitant influencer 
  if (influencer_hes[2] == 0){# If influencer has no reach, keep array empty
    followers_hes <- array(data = 0, c(1,2))
  }else{ #else create list of followers
   
    for (scan2 in 1:influencer_hes[2]){#Creates list of influencer_hes followers
      
      follower_hes_i=round(runif(1,1,sizei))
      follower_hes_j=round(runif(1,1,sizej))
      
      # skip agent if already a follower
      if (any(apply(followers_hes, 1, function(x) all(x == c(follower_hes_i,follower_hes_j))))== TRUE){
        
      }else {
        
        #if not already in followers (above gives FALSE), add to list
        followers_hes <- rbind(followers_hes,c(follower_hes_i,follower_hes_j))
      
    }##scan2 end
    
  }
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
      #Cultural Bias: (-1, 0, 1) --> index: k = (1, 2, 3)
      k = match(Individual_matrix[i,j,4], CulturalBias)
      
      kk = 0.14 #Weight of Influencer effects
      
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
        
  ### Recalculate probability of attitude change based on influencer effects ###
        
    #If confident and only following confident --> reduce probability of change by kk
        if (in_followers_conf ==TRUE & in_followers_hes == FALSE){
          
           yy <- prob_of_change*(1-kk)
        }
        
    #if confident and only following hesitant -> increase prob of change
        if (in_followers_conf ==FALSE & in_followers_hes == TRUE){
          
          yy <- kk + (1-kk)*prob_of_change
         
        }
        
    #if confident and following both confident and hesitant --> reduce probability of change
        if (in_followers_conf ==TRUE & in_followers_hes == TRUE){
          
            yy = (1-kk/2)*(prob_of_change)
            
          }
        
    #if following no one -> no action
        if (in_followers_conf == FALSE & in_followers_hes == FALSE){
          
          yy <- prob_of_change
        } 
  
      ##adding stochasticity 
        individual_prob = runif(1)
        
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
        
    ### Recalculate probability of attitude change based on influencer effects### 
    
  #if agent is hesitant and only follows confident -> increase prob of change by some factor determined by kk
    if (in_followers_conf == TRUE & in_followers_hes == FALSE){
          
         yy <- kk + (1-kk)*prob_of_change
            
        }#If hesitant and... end
        
  # if hesitant agent only follows hesitant
    if (in_followers_conf == FALSE & in_followers_hes == TRUE){
          
          yy <- prob_of_change*(1-kk)
            
        }
        
  # if agent follows both confident and hesitant -> reduced prob of change
        if (in_followers_conf ==TRUE & in_followers_hes == TRUE){
          
          yy <- (1-kk/2)*(prob_of_change)
         
        }
        
        #if agent follows neither influencer -> no action
        if (in_followers_conf == FALSE & in_followers_hes == FALSE){
          
          yy <- prob_of_change
        }

       #adding stochasticity
        individual_prob=runif(1)
        
        if(yy > individual_prob){
          Individual_matrix[i,j,2]=1
        }
        
      }#end if hesitant
      
      
  ##Probability that agent[i,j] gets vaccinated##
  ##Based on surrounding infected and vaccinated, attitude and bias

      #Summing the number if infected individuals surrounding an agent
      infected_list <- c(Individual_matrix[starti,startj,3], Individual_matrix[starti,j,3], Individual_matrix[starti,endj,3],
                         Individual_matrix[i,startj,3], Individual_matrix[i,endj,3], Individual_matrix[endi,startj,3], Individual_matrix[endi,j,3], Individual_matrix[endi,endj,3])
      
      SumOfInfected = sum(infected_list[infected_list>0])
     
      #Summing the number of vaccinated individuals surrounding the agent
      SumOfVaccinated=Individual_matrix[starti,startj,1]+Individual_matrix[starti,j,1]+Individual_matrix[starti,endj,1]+
        Individual_matrix[i,startj,1]+Individual_matrix[i,endj,1]+
        Individual_matrix[endi,startj,1]+Individual_matrix[endi,j,1]+Individual_matrix[endi,endj,1]
      

      if (Individual_matrix[i,j,1]==0){#if agent is unvaccinated
        
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
          
        } #end if confident
        
        
        if  (Individual_matrix[i,j,2]== 0){#if agent is hesitant
          
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
        if (Individual_matrix[i,j,3] == 0){Comp_prob_of_vacc = (prob_of_vacc*(Prob_of_infect))*(prob_of_vacc2)*(1-Vaccinated_Disease_threshold)}#Completely susceptible
        if (Individual_matrix[i,j,3] == -1){Comp_prob_of_vacc = (prob_of_vacc*(Infected_Disease_threshold))*(prob_of_vacc2)*(1-Vaccinated_Disease_threshold)}#Recovered (Previously Infected) 
        if (Individual_matrix[i,j,3] == 1){Comp_prob_of_vacc = 0}#Currently infected
        
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
  exposed = array(data = 0, c(1,2)) #Exposed individuals are at risk of contracting the disease
  contagious = array(data = 0 , c(1,3)) #Contagious individual have the disease
 
  
for (i in 1:sizei){
  for (j in 1:sizej){
    
    if (Individual_matrix[i,j,3]==1){# if infected 
     
       contagious <- rbind(contagious,c(i,j,t))#collects position (i,j) and infected timestep
  
       }
    
    if (Individual_matrix[i,j,3]==0 || Individual_matrix[i,j,3]==-1 ){#if no disease (never infected or recovered)
      
         if (SumOfInfected >0){
        
              exposed <- rbind(exposed,c(i,j))
             }
      }
    }#j
  }#i #End of collecting exposed and contagious
  
  
if (length(exposed[,1])>1){
  
  for (h in 2:length(exposed[,1])){#going through exposed array
  
  random_number2=runif(1)
  
  ## If not vaccinated --> chance of being infected##
 if (Individual_matrix[exposed[h,1],exposed[h,2],1]==0){
   
 # if not vaccinated but previously infected
    if (Individual_matrix[exposed[h,1],exposed[h,2],3]== -1 & random_number2 < Infected_Disease_threshold){
   
      Individual_matrix[exposed[h,1],exposed[h,2],3]=1 #then infect
                            }
 # not vaccinated and completely susceptible
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
  
  #If vaccinated and completely susceptible
  if (Individual_matrix[exposed[h,1],exposed[h,2],3]==0 & random_number2 < Vaccinated_Disease_threshold){
   
    Individual_matrix[exposed[h,1],exposed[h,2],3]=1 #then infect
 
     }#if vacc and suscep
   }# if vacc
  }
}#end of going through exposed 

Tcontagious <- rbind(Tcontagious,contagious) #list of contagious individuals

#Reassign contagious agents (agents are contagious for 2 timesteps)
for (xx in 1: nrow(Tcontagious)){
  
  if (Tcontagious[xx, 3] == t-2){ 
 
    Individual_matrix[Tcontagious[xx,1],Tcontagious[xx,2],3]= -1
    
    }
  
  } #end of reassign
 

##Choose agents to swap positions
    for (move in 1:round(sizei*sizej/per_move)){
    
    #choose random first person
    choosei1=round(runif(1,1,sizei))
    choosej1=round(runif(1,1,sizej))
    person1=Individual_matrix[choosei1,choosej1,]

    #choose random second person
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
    } #if Homophily = 0
  
    
  #If set to attitude based relocation (non-random swapping)
    if (Homophily==1){## Attitude based Movement: check potential position for matching beliefs, if the neighbors share attitudes, likely to switch
      
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
     
  #Calculate the number of      
      SumOfPostitives_person1=Individual_matrix[startchoosei1,startchoosej1,2]+Individual_matrix[startchoosei1,choosej1,2]+Individual_matrix[startchoosei1,endchoosej1,2]+
        Individual_matrix[choosei1,startchoosej1,2]+Individual_matrix[choosei1,endchoosej1,2]+
        Individual_matrix[endchoosei1,startchoosej1,2]+Individual_matrix[endchoosei1,choosej1,2]+Individual_matrix[endchoosei1,endchoosej1,2]
      
      SumOfPostitives_person2=Individual_matrix[startchoosei2,startchoosej2,2]+Individual_matrix[startchoosei2,choosej2,2]+Individual_matrix[startchoosei2,endchoosej2,2]+
        Individual_matrix[choosei2,startchoosej2,2]+Individual_matrix[choosei2,endchoosej2,2]+
        Individual_matrix[endchoosei2,startchoosej2,2]+Individual_matrix[endchoosei2,choosej2,2]+Individual_matrix[endchoosei2,endchoosej2,2]
        
      if (SumOfPostitives_person1 < SumOfPostitives_person2 ){
       
           if (Individual_matrix[choosei1,choosej1, 2]==1 & Individual_matrix[choosei2,choosej2, 2]==0){
       
               Individual_matrix[choosei1,choosej1,]=person2
               Individual_matrix[choosei2,choosej2,]=person1
       
            } 
           
         }
         
     if (SumOfPostitives_person1 > SumOfPostitives_person2 ){
      
      if (Individual_matrix[choosei1,choosej1, 2]==0 & Individual_matrix[choosei2,choosej2, 2]==1){
       
            Individual_matrix[choosei1,choosej1,]=person2
            Individual_matrix[choosei2,choosej2,]=person1
           
           }
    
      }
      
    }# If Homophily =1
   
    }#move end 

#For plotting-- frequency at each timestep (t)
Time[t] <-t
ATT[t] <- sum(Individual_matrix[,,2])/(sizei*sizej)
VACC[t] <- sum(Individual_matrix[,,1])/(sizei*sizej)
Disease[t] <- sum(Individual_matrix[,,3][Individual_matrix[,,3]>0])/(sizei*sizej)
Recovered[t] <- abs(sum(Individual_matrix[,,3][Individual_matrix[,,3]<0]))/(sizei*sizej)

#Determining when "herd immunity" is reached   
 if (VACC[t] >= .70){
     if (VACC[t-1] < .70){
       Herd_time <- t
       #print(c(g,Herd_time))
      }
 }

if (t == NumTimesteps & VACC[t] < .70){
  
  Herd_time <- 'NULL'
  
}
   
##Random person infected every 5 timesteps
if (t %% 5){
  
  Individual_matrix[round(runif(1,1,sizei)),round(runif(1,1,sizej)),3] = 1
 
          }

}#t

#Collecting the frequencies at each timestep for each simulation run
ATT_run[run,] <- ATT
VACC_run[run,] <- VACC
DIS_run[run,] <- Disease
Recov_run[run,] <- Recovered

  }#run end
  
for (colnum in 1:NumTimesteps){
    
  #Calculating the average frequencies at each timestep
    ATT_run_avg[colnum] <- mean(ATT_run[,colnum])
    VACC_run_avg[colnum] <- mean(VACC_run[,colnum])
    DIS_run_avg[colnum] <- mean(DIS_run[,colnum])
    Recov_run_avg[colnum] <- mean(Recov_run[,colnum])

  #Determining average herd immunity time     
    if (VACC_run_avg[colnum] >= .70){
      
      if (VACC_run_avg[colnum-1] < .70){
        
        avg_Herd_time <- colnum
        
        }else if (colnum == NumTimesteps & VACC_run_avg[colnum] < .70){avg_Herd_time <- 0
        #print(c(g,Herd_time))
      }
    }
    
  
#Calculating the difference between the simulated average frequency at each time and:
#Complete Primary series vaccination percentage  
model_complete[colnum] <- abs(VACC_run_avg[colnum]-vacc_data$Percent.of.People.with.Complete.Primary.Series[colnum]/100)
#...At least one dose vaccination percentage
model_one[colnum] <-  abs(VACC_run_avg[colnum]-vacc_data$Percent.of.Total.Pop.with.at.least.One.Dose[colnum]/100)
    
    }#colnum

#Matrix of Vars index and the associated total difference between vaccination frequencies
modelvs[g,] <-c(g,sum(model_complete), sum(model_one))

#Matrix of Vars and Confidence frequencies at 4 week intervals (~Monthly)
new_ATT_run_avg[g,] <-ATT_run_avg[seq(10,NumTimesteps,4)]
#
diff_new_ATT[g,] <- c(g,sum(abs(new_ATT_run_avg[g,]-conf_vector[1:length(new_ATT_run_avg[g,])])))


### Line Plots###

#Simulated Confidence freq. overtime
plot(c(0,Time), c(init_tot_conf,ATT_run_avg),main = c(paste(as.vector(Vars[[g]]),collapse = " "),paste(c(c("H ",Homophily), c("Vars", g), c("HT ", avg_Herd_time)),collapse = " ")),type= "l", col = "blue",lwd = 2, xlim = c(0, NumTimesteps),xlab = "Time", ylim = 0:1, ylab = "Frequency")
#title(main = paste(as.vector(Vars[[g]]),collapse = " "),sub = paste(g))
axis(side=1, at=seq(0, NumTimesteps, by=10))
axis(side=2, at=seq(0, 1, by= 0.10))
abline(h = 0.70, v = avg_Herd_time ,col = 'darkgreen', lwd=2, lty=2)
#Simulated Vaccination Frequency
lines(c(0,Time), c(init_tot_vacc, VACC_run_avg), type = "l", lwd = 2)
#Simulated Disease Occurrence
lines(c(0,Time), c(init_tot_infct,DIS_run_avg), type = "l", col = "red", lwd = 2)
#Simulated Recovered
lines(c(0,Time), c(init_tot_recov,Recov_run_avg), type = "l", col = "orange", lwd = 2)
#Primary Series of Vaccination
lines(Time, vacc_data$Percent.of.People.with.Complete.Primary.Series[1:NumTimesteps]/100,type="l", lty=2, lwd = 2)
#At least one dose
lines(Time, vacc_data$Percent.of.Total.Pop.with.at.least.One.Dose[1:NumTimesteps]/100, type="l", lty=3, lwd = 2)
#Surveyed Confidence
lines(Time[seq(10,NumTimesteps,4)],conf_vector[1:length(new_ATT_run_avg[g,])], type = "l", col = "magenta", lwd = 2 )


}#g

### Calculating and indexing minimum difference.

#Printing minimum differences in vaccination and the Vars index
print(c(min(modelvs[,2]),modelvs[match(min(modelvs[,2]),modelvs[,2]),1], min(modelvs[,3]), modelvs[match(min(modelvs[,3]),modelvs[,3]),1]))

#Printing confidence difference and Vars index
print(c(min(diff_new_ATT[,2]), diff_new_ATT[match(min(diff_new_ATT[,2]),diff_new_ATT[,2]),1]))


### To Optimize for at least one dose and confidence
Bestfits <- cbind(modelvs,diff_new_ATT[,2])

for (ff in 1:length(Vars)){

  SumBest[ff] <- sum(Bestfits[ff, 2:4]) #Summing the at least one dose and confidence differences
 }

SumBest2 <- cbind(1:length(Vars), SumBest)#index difference sums

print(c(min(SumBest2[,2]), SumBest2[match(min(SumBest2[,2]),SumBest2[,2]),1]))#print best fit (least difference)

## Collecting calculations and indicies
  output_text <- c(
    sprintf("Primary Vaccination: %.3f, %s", min(modelvs[,2]), modelvs[match(min(modelvs[,2]), modelvs[,2]),1]),
    sprintf("@ Least One Vax: %.3f, %s", min(modelvs[,3]), modelvs[match(min(modelvs[,3]), modelvs[,3]),1]),
    sprintf("Confidence: %.3f, %s", min(diff_new_ATT[,2]), diff_new_ATT[match(min(diff_new_ATT[,2]), diff_new_ATT[,2]),1]),
    sprintf("One Dose + Confidence: %.3f, %s", min(SumBest2[,2]), SumBest2[match(min(SumBest2[,2]), SumBest2[,2]),1])
  )
 
  
## Printing to .pdf   
  # Create a new page for the calculation results
  grid::grid.newpage()
  grid::grid.text("Summary of Minimum Best Fits", gp = grid::gpar(fontsize = 20), y = 0.9)
  
  # Add the output text to the PDF
  y_positions <- seq(0.8, 0.5, length.out = length(output_text))  # Adjust y positions
  for (i in 1:length(output_text)) {
    grid::grid.text(output_text[i], y = y_positions[i])
  }
  
dev.off()  # Close the PDF