#Novel_Vaccine_Vars
#K. Anderson
#9/19/2023

#This script compiles the list of parameters (varying bias distributions) used in NovelVaccine Lines and Novel Vaccine Heatmap

#Equal bias distribution template
template3 = c(0.1, 0.25, 0.26, 0.17, 1/3, 1/3, 1/3, 0.48, 0.28, 0.19)

## Initialize an empty list to store combinations
combinations <- list()

# Define the interval and the range of values
interval <- 0.1
values <- seq(0, 1, by = interval)

# Loop through all possible combinations
for (i in values) {
  for (j in values) {
    for (k in values) {
      if (i + j + k == 1) {
        combinations <- append(combinations, list(c(i, j, k)))
      }
    }
  }
}

template_list<- list()

for (jj in 1:length(combinations)){
  
  template_list[[jj]] <- c(c(0.1, 0.25, 0.26, 0.17), combinations[[jj]], 0.48, 0.28, 0.19)
}

template_list[[jj+1]] <- template3 #Adds equal bias template to end of list


