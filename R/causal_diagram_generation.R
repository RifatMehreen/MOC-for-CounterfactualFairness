library(dagitty)
library(ggdag)
library(ggplot2)

# dag <- dagitty("dag{x <-> z -> y x->y x->w1 x->w2 w1->y w2->y}")
# ggdag(dag, layout = "circle")
# simulateSEM(dag, b.default = NULL, N = 10, verbose = FALSE)
# http://www.dagitty.net/primer/

gen_data = function(n = 100, seed = 123, sex_input = NULL){
  
  set.seed(seed)
  b_gpa = 0.5
  b_lsat = 0.7
  b_fya = 0.3
  b_sex = 0.1
  b_race = 0.1
  
  x = rnorm(n)
  u = rnorm(n)
  race = sample(c(0,1), 1)
  sex = sample(c(0,1), 1)
  
  if(!is.null(sex_input)){
    sex = sex_input
  }
  
  gpa = b_gpa*u + race*b_race + sex *b_sex + x
  lsat = b_lsat*u + race*b_race + sex *b_sex + x 
  fya = b_fya*u + race*b_race + x
  
  data = data.frame(lsat, gpa, race, sex, fya)
}
