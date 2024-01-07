# '''
# Trying to simulate data from a causal graph
# 
# V -> C
# V <- C
# A -> V
# 
# #install following https://github.com/RobinDenz1/simDAG
# 
# #first, at command line
# $ sudo apt install libgsl-dev
# 
# install.packages("remotes")
# library(remotes)
# remotes::install_github("RobinDenz1/simDAG")
# '''

library(tidyverse)
library(simDAG)
library(gridExtra)


#plot params
labssizerel=1.5

modelname='Expressive responding'

if (modelname == 'Expressive responding'){

#Expressive responding model
dag <- empty_dag() +
  node("age", type="rnorm", mean=0, sd=1) +
  node("int", type="gaussian", parents=c("age"), betas=c(1),
       intercept=0, error=0.5) +
  node("con", type="gaussian", parents=c("int"), betas=c(-1),
       intercept=0, error=0.5)

} else { 
#True belief model
modelname = 'True belief'
dag <- empty_dag() +
  node("age", type="rnorm", mean=0, sd=1) +
  node("int", type="gaussian", parents=c("age","con"), betas=c(1,-1),
       intercept=0, error=0.5) +
  node("con", type="rnorm", mean=0,sd =1)
}


sim_dat <- sim_from_dag(dag, n_sim=1000)

p <- ggplot(data=sim_dat,aes(x=con,y=int))
p1 <- p + geom_point(color='red')

p <- ggplot(data=sim_dat,aes(x=con,y=int,color=age))
p2 <- p + geom_point() + labs(x = 'Conspiracy belief',y='Vaccine intention') +
  theme(axis.title.x = element_text(size = rel(labssizerel)),  # Increase the x-axis label font size
        axis.title.y = element_text(size = rel(labssizerel)))  # Increase the y-axis label font size


p <- ggplot(data=sim_dat,aes(x=int,y=age))
p3 <- p + geom_point(color='green') + labs(x = 'Vaccine intention', y = 'Age') +
  theme(axis.title.x = element_text(size = rel(labssizerel)),  # Increase the x-axis label font size
        axis.title.y = element_text(size = rel(labssizerel)))  # Increase the y-axis label font size


p <- ggplot(data=sim_dat,aes(x=age,y=con))
p4 <- p + geom_point(color='blue') +
  labs(x = 'Age', y = 'Conspiracy belief') +
  theme(axis.title.x = element_text(size = rel(labssizerel)),  # Increase the x-axis label font size
        axis.title.y = element_text(size = rel(labssizerel)))  # Increase the y-axis label font size



plot_grid <- gridExtra::grid.arrange(p3, p4, p2, nrow = 1, 
             top = grid::textGrob(modelname, gp = grid::gpar(fontface = "bold", fontsize = 14)))

savename <-  paste0(tolower(substr(modelname, 1, 4)), ".png")
ggsave(savename,plot= plot_grid,dpi=120,width=15, height=5)

cor.test(sim_dat$age,sim_dat$con,method="pearson",conf.level=0.95)
