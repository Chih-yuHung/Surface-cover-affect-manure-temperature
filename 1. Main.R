#Set location, initial date and end time; date origin in R, 1970-1-1

test <- 1  #the test number
Location <- "VA" #VA, VAC, or LA

for (submodels in 0:1) { #1 with cover, 0 is without cover
source("3. Parameters.R",echo = F)  #Parameters we can change
source("4. Constants.R",echo = F)   #Constants no need to change
#The major loop for original model
source("2. Major loop.R",echo = F)
}
source("Result comparison.R",echo = F)
source("stat output.R", echo =  F)
