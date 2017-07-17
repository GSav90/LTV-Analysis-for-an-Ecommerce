##Checking integer values
for(i in 1:ncol(ltv))
{
  if((i==1)|(i==2)|(i==5)|(i==6)|(i==7)|(i==8)|(i==9))
  {
    for(j in 1:nrow(ltv))
    {
      #print("h")
      if(!(is.integer(ltv[j,i])))
      { 
        print(ncol(ltv)[i])
      }
      
      
    }
    if(!((ltv[j,7]==0|1)&(ltv[j,8]==0|1)&(ltv[j,9]==0|1)))
    {
      print(ncol(ltv)[i])
      
    }
  }
  
  
}

##Checking na values 

indx <- apply(ltv, 2, function(x) any(is.na(x) | is.infinite(x)))


#Checking factors
for(i in 1:ncol(ltv))
{
  if((i==3)|(i==4))
  {
    for(j in 1:nrow(ltv))
    {
      #print("h")
      if(!(is.factor(ltv[j,i])))
      { 
        print(ncol(ltv)[i])
      }
      
      
    }
    
  }
  
  
}


agg<-import.csv('aggregate.csv')

##Converting factors to number 
nf<-ncol(agg)
agg[, nf] <- ifelse(agg[, nf] == levels(agg[, nf])[1], 0, 1)##False is 0 and True is 1

agg[, 2] <- ifelse(agg[, 2] == levels(agg[, 2])[1], 0, 1)##Female is 0 ,male is 1

##Dropping dates and purchase cycle

agg<-agg[, -c(3:6,20)]


##checking for correlation between input and output variables
filter.features.by.cor <- function(x)
{
  unsort_output <- vector()
  for (i in 1:(ncol(x) - 1))
  {
    print(i)
    unsort_output <-
      append(unsort_output,
             cor(x[i], x[ncol(x)], method = "pearson"))
  }
  output <-
    cbind.data.frame(colnames(x)[1:(ncol(x) - 1)], sort(unsort_output, decreasing =
                                                          TRUE))
  colnames(output) <- c("Features", "Linear correlation coefficient")
  return(output)
}

filter.features.by.cor(agg)


##Computing prcomp


agg_output <- agg[, 16]

p_comp <-
  prcomp(agg[1:15],
         retx = TRUE,
         center = TRUE,
         scale. = TRUE)
sm_pcomp <- summary(p_comp)


##Computing backwards step regression ,subset of data, forward search
library(leaps)
leaps <-
  regsubsets(
    quit~customer_id+gender+accum_visits+accum_days+accum_subs+accum_pages+
      accum_onsite+accum_entered+accum_completed+entered_holiday+completed_holiday+visit_cycle+purchase_cycle+pages_per_visit+onsite_per_visit,
    data = agg,
    nbest = 1,
    nvmax = 10,
    method = "exhaustive"
  )
summary(leaps)


library(MASS)
temp <- agg[,-ncol(agg)]
final <-
  cbind(as.data.frame(agg[, ncol(agg), drop = FALSE]), temp)
fit <- lm(final, data = final)
step <- stepAIC(fit, direction = "backward")
step$anova

# quit ~ customer_id + gender + accum_visits + accum_subs + accum_entered + 
#   entered_holiday/completed_holiday + visit_cycle + purchase_cycle + pages_per_visit + 
#   onsite_per_visit

step <- stepAIC(fit, direction = "forward")
step$anova
# Final Model:
#   quit ~ customer_id + gender + accum_visits + accum_days + accum_subs + 
#   accum_pages + accum_onsite + accum_entered + accum_completed + 
#   entered_holiday + completed_holiday + visit_cycle + purchase_cycle + 
#   pages_per_visit + onsite_per_visit

