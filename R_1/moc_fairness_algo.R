moc_fairness_algo = function(predictor, df, protected_column, row_num, predictor_protected, x_interest, pred_column, target, param_set, lower, upper, sdevs_num_feats, 
                    epsilon, fixed_features, max_changed, mu, n_generations, p_rec, p_rec_gen, p_rec_use_orig,
                    p_mut, p_mut_gen, p_mut_use_orig, k, idx, weights, init_strategy, cond_sampler = NULL) {
  
  # different objectives
  codomain = ParamSet$new(list(
    # ParamDbl$new("dist_target", tags = "minimize"),
    ParamDbl$new("dist_x_interest", tags = "minimize"),
    # maybe not the best constraint?
    ParamInt$new("nr_changed", tags = "minimize"),
    ParamDbl$new("dist_train", tags = "minimize"),
    # write new for maximizing distance btwn protected attributes?
    ParamDbl$new("dist_target", tags = "minimize")
  ))
  
  # TODO: Understand what the utils_fairness_moc::make_fitness_function() does
  fitness_function = make_fitness_function(
    predictor, x_interest, pred_column, target, weights, k, idx, fixed_features, param_set
  )
  
  flex_cols = setdiff(names(x_interest), fixed_features)
  sdevs_flex_num_feats = sdevs_num_feats[names(sdevs_num_feats) %in% flex_cols]
  param_set_flex = param_set$clone()
  param_set_flex$subset(flex_cols) # TODO: know more about the subset() function
  
  #' `ObjectiveRFunDt`: Objective interface where user can pass an R function that works on an data.table().
  #' `fun`: R function that encodes objective and expects an data.table() as input whereas each point
  #' is represented by one row.
  
  objective = bbotk::ObjectiveRFunDt$new(
    fun = fitness_function, 
    domain = param_set_flex, 
    codomain = codomain # The tags of each output "Parameter" define whether it should be minimized or maximized. The default is to minimize.
  )
  
  #' bbotk::OptimInstanceMultiCri Wraps a multi-criteria Objective function with extra services for 
  #' convenient evaluation. Inherits from OptimInstance.
  oi = bbotk::OptimInstanceMultiCrit$new(
    objective, 
    terminator = bbotk::trm("gens", generations = n_generations) # by default set to 175
  )
  
  if (is.null(cond_sampler)) {
    op_m = make_moc_mutator(
      ps = param_set_flex, 
      x_interest = x_interest, 
      max_changed = max_changed, 
      sdevs = sdevs_flex_num_feats, 
      p_mut = p_mut,
      p_mut_gen = p_mut_gen, 
      p_mut_use_orig = p_mut_use_orig
    )
  } else {
    op_m = make_moc_conditional_mutator(
      ps = param_set_flex, 
      x_interest = x_interest,
      max_changed = max_changed, 
      p_mut = p_mut,
      p_mut_gen = p_mut_gen, 
      p_mut_use_orig = p_mut_use_orig,
      cond_sampler = cond_sampler
    )
  }
  
  
  op_r = make_moc_recombinator(
    ps = param_set_flex, 
    x_interest = x_interest, 
    max_changed = max_changed, 
    p_rec = p_rec,
    p_rec_gen = p_rec_gen, 
    p_rec_use_orig = p_rec_use_orig
  )
  
  #' TODO: Replace this by tournament selection
  #' `sel` selects the best
  op_parent = sel("best")
  
  sel_nondom_penalized = ScalorNondomPenalized$new(epsilon)
  op_survival = sel("best", sel_nondom_penalized)   
  
  # population initializer
  pop_initializer = make_moc_pop_initializer(
    ps = param_set_flex, 
    x_interest = x_interest, 
    max_changed = max_changed, 
    init_strategy = init_strategy, 
    flex_cols = flex_cols, 
    sdevs = sdevs_flex_num_feats, 
    lower = lower, 
    upper = upper, 
    predictor = predictor,
    fitness_function = fitness_function,
    mu = mu
  )
  
  mies_prime_operators(
    search_space = oi$search_space, 
    mutators = list(op_m), 
    recombinators = list(op_r),
    selectors = list(op_parent, op_survival)
  )
  
  mies_init_population(
    inst = oi, 
    mu = mu, 
    initializer = pop_initializer
  )
  
  tryCatch({
    repeat {
      offspring = mies_generate_offspring(oi, lambda = mu, op_parent, op_m, op_r)
      mies_evaluate_offspring(oi, offspring)
      mies_survival_plus(oi, mu, op_survival)
    }
  }, terminated_error = function(cond) {
  })
  bbotk::assign_result_default(oi)
  
  # Post-processing of the result
  # Re-attach fixed features
  if (!is.null(fixed_features)) {
    oi$result[, (fixed_features) := x_interest[, fixed_features, with = FALSE]]
  }
  
  # Transform factor column w.r.t to original data
  factor_cols = names(which(sapply(predictor$data$X, is.factor)))
  for (factor_col in factor_cols) {
    fact_col_pred = predictor$data$X[[factor_col]]
    value =  factor(oi$result[[factor_col]], levels = levels(fact_col_pred), ordered = is.ordered(fact_col_pred))
    oi$result[, (factor_col) := value]
  }
  
  int_cols = names(which(sapply(predictor$data$X, is.integer)))
  if (length(int_cols) > 0L) {
    oi$result[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
  }
  
  # setorder: Fast row reordering of a data.table by reference
  setorder(oi$result, dist_target)
  oi
}

