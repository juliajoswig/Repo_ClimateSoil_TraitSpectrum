hierarchical_partitioning_N2 <- function(X0,X1,X2,X12,sel_now){
  
#------------------
# ONE
#------------------
D1_1 <- X1 -0  # how much can ONE explain
D1_2 <- X12 - X2 # how much can only ONE explain WITHOUT TWO
#----------
D1 = D1_1 + D1_2 # how much is shared information to explain this trait by first and second "layer" 
# variance average ONE (shared information to explain this trait  by first and second "layer") 
indep_1 = D1/2

#------------------
# TWO
#------------------
D2_1 <- X2 -0  # how much can TWO explain
D2_2 <- X12 - X1 # how much can only TWO explain WITHOUT ONE

D2 = D2_1 + D2_2 # how much is shared information to explain this trait by first and second "layer" 
# variance average TWO (shared information to explain this trait  by first and second "layer") fraction TWO
indep_2 = D2/2 

#Distribution of indep effects:
round((indep_1+indep_2),digits = 3) == round(X12,digits = 3)
distr_indep1 = indep_1/(indep_1+indep_2)
distr_indep2 = indep_2/(indep_1+indep_2)

#---------------------------------------------------------------------
#JOINT
#---------------------------------------------------------------------
#Joint effect
# joint effect (always equally distributed among components)
joint_1 = X1-indep_1
joint_2 = X2-indep_2

#distrib joint effect
distr_joint_1 = joint_1/(joint_1+joint_2)
distr_joint_2 = joint_2/(joint_1+joint_2)


joint_1+indep_1
joint_2+indep_2
indep_1+indep_2==X12

out <- list()
secondname=paste0(sub(".*A", "", sel_now))
firstname=paste0(substring(text = sel_now,first = 1,last = regexpr("A", sel_now)[1]-1))
out[[paste0("indep_",firstname)]] <- indep_1
out[[paste0("indep_",secondname)]] <- indep_2
out[[paste0("joint_",firstname)]] <- joint_1
out[[paste0("joint_",secondname)]] <- joint_2
out[[paste0("distr_joint_",firstname)]] <- distr_joint_1
out[[paste0("distr_joint_2",secondname)]] <- distr_joint_2
out[[paste0("distr_indep_",firstname)]] <- distr_indep1
out[[paste0("distr_indep_",secondname)]] <- distr_indep2


return(out)
}