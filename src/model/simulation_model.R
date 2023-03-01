#### MODEL EXECUTION -----------------------------------------------------------

#' function to run simulations until N1 reaches carrying capacity in all 
#' cells for all simulations; assumes disturbance is implemented on ICs only
#' 
#' @param data data list containing elements \code{N1} and \code{N2} 
#' with initial population sizes for each species, respectively
#' @param params vector containing parameter values and names
#' @param disp_prob_mat matrix containing dispersal probabilities
#' @param change_startcol integer new disturbance start column
#' @param change_endcol integer new disturbance end column
#' @param change_distpct1 double new % N1 killed by disturbance
#' @param change_distpct2 double new % N2 killed by disturbance
#' 
#' @return vector of times to reach carrying capacity
calc_spread_time <- function(data, params, disp_prob_mat, 
                             change_startcol, change_endcol, 
                             change_distpct1, change_distpct2){
  # update parameters
  params = update_params(params, 
                         change_startcol, change_endcol, 
                         change_distpct1, change_distpct2)
  # implement management disturbance on initial conditions
  data <- disturb(data, 
                  params$DistColStart, params$DistColEnd, 
                  c(params$Dist1, params$Dist2), 
                  params$dist_type)
  # turn off disturbance parameters for rest of simulation
  params$Dist1 = 0
  params$Dist2 = 0
  # run simulations until all have reached carrying capacity in all cells
  t = 1
  spread_times = vector()
  n_sim = nrow(data$N1)
  while(length(spread_times) < n_sim & t < 501){
    # reformat data to matrix when only one simulation left
    if(length(spread_times) == n_sim-1){
      data = lapply(data, function(i){i = matrix(i, nrow = 1, byrow = TRUE)})
    }
    data = run_single_timestep(data, params, disp_prob_mat)
    # have any simulations reached carrying capacity in all cells?
    at_K = which(rowSums(data$N1) >= params$K1*ncol(data$N1))
    if(length(at_K)>0){
      spread_times = c(spread_times, rep(t, length(at_K)))
      data = lapply(data, function(i){return(i[-at_K,])})
    }
    # update time counter
    t = t + 1
    if(t == 501){browser()}
  }
  return(spread_times)
}

#' function to update disturbance parameters
update_params <- function(params, 
                          change_startcol, change_endcol, 
                          change_distpct1, change_distpct2){
  new_params = params
  new_params[["DistColStart"]] = change_startcol
  new_params[["DistColEnd"]] = change_endcol
  new_params[["Dist1"]] = change_distpct1
  new_params[["Dist2"]] = change_distpct2
  return(new_params)
}


#' execute one time step of spatial Lotka-Volterra simulation, including
#' (1) birth/death
#' (2) dispersal
#' (3) disturbance
#' (4) demographic stochasticity
#' 
#' @param data list containing elements \code{N1} and \code{N2} 
#' with population sizes for each species, respectively, from prior time step
#' @param params vector of parameter values (including names)
#' @param disp_prob_mat list containing elements \code{N1} and \code{N2} 
#' with dispersal probability matrices
#' 
#' @return list with new population sizes for each species
run_single_timestep <- function(data, params, disp_prob_mat){
  if(check_for_param_issues(data, params)){
    return(list("N1" = matrix(NA, nrow = nrow(data$N1), ncol = nrow(data$N2)), 
                "N2" = matrix(NA, nrow = nrow(data$N1), ncol = nrow(data$N2))))
  }
  # reproduction and mortality
  data = birth_death(data, params)
  # dispersal
  data = dispersal(data, disp_prob_mat, c(params$DispDist1, params$DispDist2))
  # disturbance
  if(params$Dist1 + params$Dist2 > 0){         # run only when % killed is > 0
    data = disturb(data, 
                   params$DistColStart, params$DistColEnd, 
                   c(params$Dist1, params$Dist2), 
                   params$dist_type)
  }
  # demographic stochasticity
  data = demog_stoch(data)
  return(data)
}


#' function to calculate the wave front of one set of simulations
#' 
#' @param data matrix of population sizes (rows: each simulation, cols: each cell along landscape)
#' @param detection_lim double colonies/cell after which species can be detected
#' 
#' @details this function is built for N1 wave (i.e., assumes L to right spread)
calc_wave_front <- function(data, detection_lim = 1){
  d = matrix(rep(1:ncol(data), nrow(data)), nrow = nrow(data), byrow = TRUE)
  d[data < detection_lim] = 0
  wf = max.col(d)
}


#### BIRTH DEATH -------------------------------------------------------

#' function to implement birth and death process within a single cell
#' using Lotka-Volterra competition equations
#' 
#' @param data list containing elements \code{N1} and \code{N2} 
#' with current population sizes for each species, respectively
#' @param params vector of parameters (named)
birth_death = function(data, params){
  # implement LV equations for each cell
  with(as.list(params),{
    data$N1 = data$N1 + (data$N1 * r1 * (1 - a11 * data$N1 - a12 * data$N2))
    data$N2 = data$N2 + (data$N2 * r2 * (1 - a22 * data$N2 - a21 * data$N1))
    return(data)
  })
}

#### DISPERSAL -----------------------------------------------------------------

#' function to implement dispersal between cells 
#' (assumes exponential dispersal kernel)
#' 
#' @param data list containing "N1" and "N2" population sizes for each species
#' @param disp_prob_matrix list of two matrices of dispersal probabilities
#' @param max_disp_dist vector of two integers of maximum dispersal distance
#' 
#' @return matrix of dispersed populations
dispersal <- function(data, disp_prob_matrix, max_disp_dist){
  n_col = ncol(data$N1)
  # create new matrix with additional columns for reflecting boundaries
  # first, N1
  l_reflect1 <- matrix(data$N1[,rev(1:max_disp_dist[1])], 
                       ncol = max_disp_dist[1], byrow = TRUE)
  u_reflect1 <- matrix(data$N1[, rev((n_col-max_disp_dist[1]+1):n_col)], 
                       ncol = max_disp_dist[1], byrow = TRUE)
  reflect_bound1 <- cbind(l_reflect1, data$N1, u_reflect1)
  # repeat for N1
  l_reflect2 <- matrix(data$N2[,rev(1:max_disp_dist[2])], 
                       ncol = max_disp_dist[2], byrow = TRUE)
  u_reflect2 <- matrix(data$N2[, rev((n_col-max_disp_dist[2]+1):n_col)], 
                       ncol = max_disp_dist[2], byrow = TRUE)
  reflect_bound2 <- cbind(l_reflect2, data$N2, u_reflect2)
  # matrix multiplication for population sizes after dispersal
  return(list("N1" = reflect_bound1 %*% disp_prob_matrix[[1]], 
              "N2" = reflect_bound2 %*% disp_prob_matrix[[2]]))
}


#' function to generate matrix of dispersal probabilities
#' assumes exponential dispersal kernel
#' 
#' @param n_col integer number of cells in 1D spatial arena (i.e., number of columns in 
#' simulation \code{data.frame})
#' @param max_disp_dist integer furthest dispersal distance
#' @param dist_pct double percent of individuals from a single cell that disperse
#' in one time step
#' @param pct_within double percent of dispersal that occurs within max_disp_dist
#' 
#' @details right now, this function assumes that the grid is defined in 1 unit cells
#' 
#' @return  matrix defining dispersal probabilities
create_disp_mat <- function(n_col, max_disp_dist, dist_pct, pct_within = 0.999){
  # calculate distance to each cell on landscape (include reflecting boundaries)
  dist_mat <- create_dist_mat(n_col, max_disp_dist)
  # calculate probability of dispersing (exponential dispersal kernel) by 
  # distance from cell being dispered to (CDF of dist - CDF of dist-1)
  # I think this assumes all cells are 1 unit wide (bc - 1)
  rate = log(1-pct_within)/-max_disp_dist
  disp_probs_by_dist = pexp(0:max(dist_mat), rate = rate) - pexp((0:max(dist_mat))-1, rate = rate)
  # create disp_mat based on distances defined in dist_mat
  # adjust transition probabilities when there are multiple cells of the same distance
  disp_mat <- apply(dist_mat, 2, function(i){
    num = sapply(0:max(i), function(j){length(which(i == j))});
    new_disp_probs_by_dist = disp_probs_by_dist[0:max(i)+1]/num;
    return(new_disp_probs_by_dist[i+1])
  })
  # update probabilities to account for % dispersing or staying in cell
  disp_mat = disp_mat * dist_pct
  rows_for_reflect_bounds = (max_disp_dist+1):(max_disp_dist+n_col)
  diag(disp_mat[rows_for_reflect_bounds,]) = 1-dist_pct
  return(disp_mat)
}

#' create matrix of distances between cells 
#' 
#' @inheritParams create_disp_mat
#' 
#' @return matrix of distances from each cell including additional 
#' rows to account for reflecting boundaries
create_dist_mat <- function(n_col, max_disp_dist){
  dist_mat <- matrix(nrow = n_col + 2*max_disp_dist, ncol = n_col)
  for(i in 1:ncol(dist_mat)){
    dist_mat[ , i] <- c((max_disp_dist+(i-1)):0, 1:(ncol(dist_mat)+max_disp_dist-i))
  }
  return(dist_mat)
}


#### DISTURBANCE ---------------------------------------------------------------

#' function to calculate new population sizes after disturbance
#' disturbance can be defined by absolute positions on landscape or relative
#' to the wave front of either species
#' 
#' @param data list containing "N1" and "N2" population sizes for each species
#' @param dist_start integer location to start disturbance
#' @param dist_end integer location to end disturbance
#' @param dist_pct vector of percent killed in disturbance for N1 and N2 
#' @param dist_type character to specify how disturbance location is defined
#' 
#' @details  \code{dist_type} can take \code{"abs"} for absolute disturbance 
#' location, \code{"N1"} or \code{"N2"} for disturbance location relative to 
#' N1 or N2 wave front, respectively
#' 
#' @return list containing new population sizes 
#in.DistType: 0 for absolute start/endcol, 1 for relative to N1 wave front
disturb = function(data, dist_start, dist_end, dist_pct, dist_type){
  # if relative to wave front, calculate new start/end locations
  if(dist_type %in% c("N1", "N2")) {
    dist_locations = calc_rel_start_end_col(data, dist_start, dist_end, dist_type)
    dist_start = dist_locations$start
    dist_end = dist_locations$end
  }
  # change population sizes due to disturbance
  # number of surviving individuals in disturbed area = old pop size * (1-dist_pct)
  for(i in 1:nrow(data$N1)){
    x = dist_start[i]:dist_end[i]
    data$N1[i,x] = data$N1[i,x] * (1-dist_pct[1])
    data$N2[i,x] = data$N2[i,x] * (1-dist_pct[2])
  }
  return(data)
}

#' function to calculate the starting and ending column for disturbance 
#' relative to the wave front
#' 
#' @inheritParams Disturb
#' 
#' @return list containing "start" and "end" for starting and ending locations
calc_rel_start_end_col <- function(data, dist_start, dist_end, dist_type){
  n_col = ncol(data$N1)
  width = dist_end - dist_start + 1 
  # identify location of N1 wave
  wave_front = calc_wave_front(data$N1)
  if(any(is.na(wave_front))){
    return(list(start = NA, end = NA))
  }
  # make start and end relative to wave front
  dist_start = dist_start + wave_front
  dist_end = dist_end + wave_front
  # check if start location is negative
  if(any(dist_start < 1)){
    #browser()
    dist_end[dist_start < 1] = ifelse(dist_end[dist_start < 1] > n_col, n_col, width)
    dist_start[dist_start < 1] = 1
  }
  # check if end location is more than n_col
  if(any(dist_end > n_col)) {
    #browser()
    dist_start[dist_end > n_col] = n_col - width + 1
    dist_end[dist_end > n_col] = n_col
  }
  return(list(start = dist_start, end = dist_end))
}

#### STOCHASTICITY -------------------------------------------------------------

#' function to implement demographic stochasticity:
#' (1) negative population sizes are set to 0
#' (2) decimal population sizes are rounded 
#' 
#' @details population sizes are rounded by generating a random number 
#' between 0 and 1. We round down if decimal <= random and round up if 
#' decimal > random. We choose only one random number per replicate. 
#' In this case, the random number is like an indicator for a "good" or "bad year
#' 
#' @return list with new population sizes
demog_stoch <- function(data){
  # population cannot be negative
  data$N1[data$N1 < 0] = 0
  data$N2[data$N2 < 0] = 0
  # population can not be fractional
  rand = matrix(rep(runif(nrow(data$N1)), ncol(data$N1)), ncol = ncol(data$N1))
  N1_decimals <- data$N1 %% 1
  N2_decimals <- data$N2 %% 1
  data$N1[N1_decimals <= rand] = as.integer(data$N1[N1_decimals <= rand])
  data$N2[N2_decimals <= rand] = as.integer(data$N2[N2_decimals <= rand])
  data$N1[N1_decimals > rand] = ceiling(data$N1[N1_decimals > rand])
  data$N2[N2_decimals > rand] = ceiling(data$N2[N2_decimals > rand])
  return(data)
}

#### DATA CHECKS ---------------------------------------------------------------
#' check if disturbance params are off landscape
check_for_param_issues <- function(data, params){
  issues_flag <- FALSE
  if(params$DistColEnd > ncol(data$N1)){issues_flag = TRUE}
  return(issues_flag)
}

