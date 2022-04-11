#' ---
#' title: "Rust Replication"
#' author: "Colleen O'Briant"
#' date: "`r Sys.Date()`"
#' output: html_document
#' ---
#+ message = FALSE
library(tidyverse)
library(maxLik)

#' # 1. Load Data
#' 
#' Data: https://editorialexpress.com/jrust/nfxp.html

create_replacement_data <- function(tb_replacements){
  # This function takes the engine replacement data from Rust's header and makes 
  # a tibble that I can use to join with the other data and get it in a "tidied" 
  # format
  tb_replacements %>%
    as_tibble() %>%
    mutate(replacements = c("first", "second")) %>%
    pivot_longer(!replacements, names_to = "bus", values_to = "replacement_mileage") %>%
    pivot_wider(names_from = replacements, values_from = replacement_mileage)
}

process_data <- function(file, bus, rows, columns){
  bus_type <- matrix(
    data = read_table(
      paste0("data/", file, ".ASC"),
      col_names = F)$X1, nrow = rows, ncol = columns
  )
  colnames(bus_type) <- paste0(bus, "_", bus_type[1, ])
  bus_type_replacements <- bus_type[c(6, 9), ]
  bus_type <- as_tibble(bus_type) %>%
    slice(-11:-1) %>%
    mutate(month = 1:nrow(.)) %>%
    pivot_longer(!month, names_to = "bus", values_to = "mileage")
  bus_type %>%
    left_join(create_replacement_data(bus_type_replacements), by = "bus") %>%
    mutate(bus_group = file)
}

#+ message = FALSE
rust <- bind_rows(
  #  D309.ASC     110x4 matrix for Davidson model 309 buses
  process_data("D309", "Davidson_309", 110, 4),
  #  G870.ASC     36x15 matrix for Grumman model 870 buses
  process_data("G870", "Grumman_870", 36, 15),
  #  RT50.ASC     60x4  matrix for Chance model RT50 buses
  process_data("RT50", "Chance_RT50", 60, 4),
  #  T8H203.ASC   81x48 matrix for GMC model T8H203 buses
  process_data("T8H203", "GMC_T8H203", 81, 48),
  #  A452372.ASC 137x18 matrix for GMC model A4523 buses, model year 1972
  process_data("A452372", "GMC_A4523_72", 137, 18),
  #  A452374.ASC 137x10 matrix for GMC model A4523 buses, model year 1974 
  process_data("A452374", "GMC_A4523_74", 137, 10),
  #  A530872.ASC 137x18 matrix for GMC model A5308 buses, model year 1972 
  process_data("A530872", "GMC_A5308_72", 137, 18),
  #  A530874.ASC 137x12 matrix for GMC model A5308 buses, model year 1974 
  process_data("A530874", "GMC_A5308_74", 137, 12),
  #  A530875.ASC 128x37 matrix for GMC model A5308 buses, model year 1975 
  process_data("A530875", "GMC_A5308_75", 128, 37),
)

rust %>% filter(bus == "GMC_A5308_72_5272")

#' Rust estimates separate models for new buses (groups 1-3), older 1975 buses 
#' (group 4), and pooled model (groups 1-4). I'll just focus on the pooled model 
#' for this analysis. 
#' 
#' Bus group 1: 15  Grumman 870 from 1983
#' 
#' 2: 4 Chance RT50 from 1981
#' 
#' 3: 48 GMC T8H203 from 1979

rust <- rust %>%
  mutate(
    bus_group = case_when(
      bus_group == "G870" ~ 1,
      bus_group == "RT50" ~ 2,
      bus_group == "T8H203" ~ 3,
      bus_group == "A530875" ~ 4,
      bus_group == "A530874" ~ 5,
      bus_group == "A452374" ~ 6,
      bus_group == "A530872" ~ 7,
      bus_group == "A452372" ~ 8
    )
  ) %>%
  filter(!is.na(bus_group)) %>%
  filter(bus_group <= 4)

#' # 2 Discretize the data
#'
#' Transform data into 2 vars: x_t = mileage bucket (regenerates to 0
#' after an engine replacement); replace = T if engine is replaced in the month
#' *after* that odometer reading.
#'
#' Also use the data to get P: transition probability matrices. pi0: the 
#' probability the bus does not step up a mileage bucket; pi1: the prob the bus
#' steps up by 1 mileage bucket; pi2: the prob the bus steps up by 2 mileage
#' buckets.

rust <- rust %>%
  group_by(bus) %>%
  mutate(
    # first = 0 indicates that the bus's engine was never replaced, not that
    # it was replaced at odometer reading 0. Transform this to say 
    # first = max(mileage) + 1, and the same for second engine replacement.
    first = if_else(first > 0, first, max(mileage) + 1),
    second = if_else(second > 0, second, max(mileage) + 1),
    # `replacements` counts the number of replacements the bus engine has
    # experienced at any given observation.
    n_replacements = case_when(mileage < first ~ 0,
                               mileage < second ~ 1,
                               mileage >= second ~ 2),
    # Let mileage regenerate to 0 when the engine is replaced.
    mileage = mileage - case_when(
      n_replacements == 0 ~ 0,
      n_replacements == 1 ~ first,
      n_replacements == 2 ~ second,
    ),
    # replace = T if the engine was replaced in the month after that reading
    replace = n_replacements - lead(n_replacements) < 0,
    # discretize mileage into buckets
    mileage = cut_interval(mileage, length = 5000, labels = F)
  ) %>%
  filter(!is.na(replace)) %>%
  ungroup() %>%
  select(bus, mileage, replace)

probs <- rust %>%
  group_by(bus) %>%
  # Change to mileage: mileage - lag(mileage), except if the engine has been
  # replaced, in which case it's just mileage - 1 bucket (there's no bucket
  # zero, buckets start with 1).
  mutate(
    mileage_change = if_else(
      lag(replace) == F, 
      mileage - lag(mileage), 
      mileage - 1L)
  ) %>%
  ungroup() %>%
  count(mileage_change) %>%
  filter(!is.na(mileage_change)) %>%
  mutate(total = sum(n), pi = n/total)

# When do replacements occur?
rust %>%
  filter(replace == T) %>%
  ggplot(aes(x = mileage)) +
  geom_histogram(bins = 20)


#' # 3 Probability Transition Matrix
#'
#' For the transition probability matrix, Rust calculates 3 conditional 
#' probabilities (for each bus group): Without replacement, what is 
#' the probability in a month:
#' - the bus does not step up a mileage bucket (\pi_0)
#' - the bus steps up 1 mileage bucket (\pi_1)
#' - the bus steps up 2 mileage buckets (\pi_2)

# These probs must sum to 1:
probs$pi
probs$pi %>% sum()

# If the engine is replaced, w.p. pi0 it will be in mileage bucket 1 next pd.
# w.p. pi1 it will be in mileage bucket 2 next pd. And w.p. pi2 it will be in
# mileage bucket 3 next pd.
transitiond1 <- matrix(
  rep(c(probs$pi, rep(0, 90 - 3)), 90),
  nrow = 90, byrow = T
)

transitiond1[1:5, 1:5]

# If the engine is not replaced, wp pi0 it will be in the same mileage bucket
# next period as it is this period. Etc.
transitiond0 <- matrix(
  rep(0, 90*90), nrow = 90
)

for(i in 1:90) {
  if (i < 89) {
    transitiond0[i, i] <- probs$pi[1]
    transitiond0[i, i + 1] <- probs$pi[2]
    transitiond0[i, i + 2] <- probs$pi[3]
  } else if (i < 90) {
    # End state is absorbing
    transitiond0[i, i] <- probs$pi[1]
    transitiond0[i, i + 1] <- 1 - probs$pi[1]
  } else {
    transitiond0[i, i] <- 1
  }
}

transitiond0[1:5, 1:5]
transitiond0[86:90, 86:90]

#' # 4 Helper functions for the nested fixed point algorithm
#' 
#' Define a function `bellman` to do 3 things that will be helpful throughout 
#' this replication: given guesses for EV and parameters theta, 
#' 
#' 1. By calling `bellman(EV, out = "EV")`, it calculates a new guess for EV 
#'    through a contraction iteration: 
#'    $EV = T(EV) = P * \log(\exp(\text{value_keep}) + \exp(\text{value_replace}))$.
#'    It also re-centers EV to avoid computing `exp(x)` with large x that will
#'    result in Inf.
#'    
#' 2. By calling `bellman(EV, out = "pchoose0")`, it calculates the probability 
#'    the agent chooses to keep the engine and not replace it. Rust uses 
#'    $P(0|x_t)$ or $pk$ for this. The formula is 
#'    $\text{pchoose0} = \frac{\exp(\text{value_keep})}{\exp(\text{value_keep}) + \exp(\text{value_replace})} = \frac{1}{1 + \exp(\text{value_replace} - \text{value_keep})}$
#'    
#' 3. By calling `bellman(EV, out = "bellman_deriv")`, it calculates the Frechet 
#'    derivative $T'_\theta = [D_{EV_\theta}T_\theta]$ 
#'    $= [D_{EV_\theta}(\text{transitiond0} * \ln(e^{vk} + e^{vr}))] = \text{transitiond0} * [D_{EV_\theta}(ln(e^{vk} + e^{vr}))]$, 
#'    Where $ln(e^{vk} + e^{vr})$ is the nx1 vector (letting $x = EV_\theta$ 
#'    for notational brevity):
#'   
#' $$
#' \left(\begin{array}{c} 
#'      f1 = ln(e^{-c + \beta x_1} + e^{-tr - c + \beta x_1})\\
#'      f2 = ln(e^{-c + \beta x_2} + e^{-tr - c + \beta x_1})\\
#'      f3 = ln(e^{-c + \beta x_3} + e^{-tr - c + \beta x_1})\\
#'      ...
#'      \end{array}\right)
#' $$
#' 
#' Then $[D_{EV_\theta}(ln(e^{vk} + e^{vr}))]$ is the nxn matrix:
#' 
#' $$
#' \left(\begin{array}{c} 
#'      D_{x_1}f1 & D_{x_2}f1 & D_{x_3}f1 & ...\\
#'      D_{x_1}f2 & D_{x_2}f2 & D_{x_3}f2 & ...\\
#'      D_{x_1}f3 & D_{x_2}f3 & D_{x_3}f3 & ...\\
#'      ...
#'      \end{array}\right) =
#' \left(\begin{array}{c} 
#'      \beta & 0 & 0 & 0 & ...\\
#'      \beta \frac{e^{vr}}{e^{vk_2} + e^{vr}} & \beta \frac{e^{vk_2}}{e^{vk_2} + e^{vr}} & 0 & 0 & ...\\
#'      \beta \frac{e^{vr}}{e^{vk_3} + e^{vr}} & 0 & \beta \frac{e^{vk_3}}{e^{vk_3} + e^{vr}} & 0 & ...\\
#'      ...
#'      \end{array}\right)
#' $$
#' 
#' And since $\text{pchoose0} = \frac{\exp(\text{value_keep})}{\exp(\text{value_keep}) + \exp(\text{value_replace})}$, this simplifies to:
#' 
#' $$
#' \left(\begin{array}{c} 
#'      \beta & 0 & 0 & 0 & ...\\
#'      \beta \frac{e^{vr}}{e^{vk_2} + e^{vr}} & \beta \text{pchoose0}_2 & 0 & 0 & ...\\
#'      \beta \frac{e^{vr}}{e^{vk_3} + e^{vr}} & 0 & \beta \text{pchoose0}_3 & 0 & ...\\
#'      ...
#'      \end{array}\right)
#' $$
#' 
#' Finally, $T'_\theta = \text{transitiond0} * [D_{EV_\theta}(ln(e^{vk} + e^{vr}))] =$
#' 
#' $$
#' \left(\begin{array}{c}
#'      \pi_0 & \pi_1 & \pi_2 & 0 & 0 & ...\\
#'      0 & \pi_0 & \pi_1 & \pi_2 & 0 & ...\\
#'      0 & 0 & \pi_0 & \pi_1 & \pi_2 & ...\\
#'      ...
#' \end{array}\right)
#' \left(\begin{array}{c} 
#'      \beta & 0 & 0 & 0 & ...\\
#'      \beta \frac{e^{vr}}{e^{vk_2} + e^{vr}} & \beta \text{pchoose0}_2 & 0 & 0 & ...\\
#'      \beta \frac{e^{vr}}{e^{vk_3} + e^{vr}} & 0 & \beta \text{pchoose0}_3 & 0 & ...\\
#'      ...
#'      \end{array}\right)
#' $$
#' 
#' Which equals:
#' 
#' $$
#' \left(\begin{array}{c} 
#'      \beta \pi_0 + \pi_1 \frac{\beta e^{vr}}{e^{vk_2} + e^{vr}} + \pi_2 \frac{\beta e^{vr}}{e^{vk_3} + e^{vr}} & \beta \pi_1 \text{pchoose0}_2 & \beta \pi_2 \text{pchoose0}_3 & 0 & ... & 0\\
#'      \pi_0 \frac{\beta e^{vr}}{e^{vk_2} + e^{vr}} + \pi_1 \frac{\beta e^{vr}}{e^{vk_3} + e^{vr}} + \pi_2 \frac{\beta e^{vr}}{e^{vk_4} + e^{vr}} & \beta \pi_0 \text{pchoose0}_2 & \beta \pi_1 \text{pchoose0}_3 & \beta \pi_2 \text{pchoose0}_4 & ... & 0\\
#'      \pi_0 \frac{\beta e^{vr}}{e^{vk_3} + e^{vr}} + \pi_1 \frac{\beta e^{vr}}{e^{vk_4} + e^{vr}} + \pi_2 \frac{\beta e^{vr}}{e^{vk_5} + e^{vr}} & 0 & \beta \pi_0 \text{pchoose0}_3 & ... & ... & 0\\
#'      ...
#'      \end{array}\right)
#' $$
#' 

bellman <- function(EV, costs, replacement_costs, out = "EV") {
  
  # If you don't replace, you pay cost[i] and get future value of being in 
  # state i.
  value_keep <- -1 * costs + beta * EV 
  # If you replace, you pay RC and the process regenerates, so you pay cost[1], 
  # and get future value of being in state 1.
  value_replace <- -1 * replacement_costs - costs[1] + beta * EV[1] 
  
  # Use maxV to center value_keep and value_replace around 0 so that exp() is 
  # computatable using the log-sum-exp trick (since exp(1000) = Inf)
  maxV <- pmax(value_keep, value_replace)
  logsum <- maxV + log(
    exp(value_keep - maxV) + exp(value_replace - maxV)
  )
  EV <- transitiond0 %*% logsum
  
  if (out == "EV") {
    return(EV)
  }
  
  # Also compute the choice probability (Rust's pk):
  # pk = exp(value_keep)/(exp(value_keep) + exp(value_replace))
  #    = 1/(1 + exp(vr - vk))
  # 1/exp(1000) = 0, so centering is not needed here
  
  pchoose0 <- 1 / (1 + exp(value_replace - value_keep))
  
  if (out == "pchoose0") {
    return(pchoose0)
  }
  
  if (out == "bellman_deriv") {
    # Compute the frechet derivative T':
    dv <- beta * (
      transitiond0 * matrix(rep(pchoose0, 90), nrow = 90, byrow = T)
    )
    dv[,1] <- dv[,1] + beta * (transitiond0 %*% (1 - pchoose0))
    # dv <- diag(beta*as.vector(pchoose0), nrow = 78, ncol = 78)
    # dv <- transitiond0 %*% dv
    # dv[,1] <- beta - rowSums(dv[,2:78])
    return(dv)
  }
}

check_convergence <- function(EV_next, EV, tol) {
  max(abs(EV_next - EV)) < tol
}

check_domain_attraction <- function(EV_next, EV, EV_c_change, domain_tol) {
  rtol <- max(abs(EV - EV_next))/EV_c_change
  abs(beta - rtol) < domain_tol
}

#' # 5 Inner Loop: Contraction and NK Iterations
#' 

beta <- .9999 # discount factor

#' 
#' Given a guess for theta, find the fixed point $EV_\theta = T_\theta(EV_\theta)$.
#' 
#' Start with contraction iterations. Perform `min_c_iterations`, breaking only 
#' if convergence is achieved `(max(abs(EV_next - EV)) < 1e-6)`. Continue with 
#' contraction iterations until i = `max_c_iterations`, unless it hits the 
#' domain of attraction: where the EV change ratio 
#' `(max(abs(EV - EV_next))/EV_change previously) < contraction_tol`.
#' Once `max_c_iterations` is hit or the domain of attraction is hit, switch to 
#' NK iterations.
#' 

inner_loop <- function(theta, EV) {
  
  costs <- (1:90)*.001*theta[1]
  replacement_costs <- theta[2]
  EV_c_change <- 1 #arbitrary initialization
  domain_tol <- .01
  min_c_iterations <- 5
  max_c_iterations <- 40
  max_nk_iterations <- 10
  
  # Loop between Contraction Iterations and N-K Iterations at most 5x
  for (i in 1:5) { 
    
    # 1. Contraction Iterations
    for (j in 1:max_c_iterations) {
      
      EV_next <- bellman(EV, costs, replacement_costs, out = "EV")
      
      # If the domain of attraction is achieved, break into N-K iterations.
      if (j >= min_c_iterations & 
          check_domain_attraction(EV_next, EV, EV_c_change, domain_tol)) {
        EV <- EV_next
        break
      }
      
      adj <- ceiling(log10(abs(max(EV_next))))
      
      # If convergence is achieved, break the fixed point loop and return EV
      if (check_convergence(EV_next, EV, 1e-6*(10^adj))) { 
        print(paste0(
          "Convergence achieved after ", i, 
          " inner loops with ", j, "contractions."
        ))
        return(list(EV = EV, "convergence", EV_change = max(abs(EV - EV_next))))
      }
      
      EV <- EV_next
    }
    
    # 2. Newton-Kantorovich Iterations
    for (k in 1:max_nk_iterations) {
      
      EV_next <- EV - (
        solve(
          diag(90) - bellman(EV, costs, replacement_costs, out = "bellman_deriv")
        ) %*% (EV - bellman(EV, costs, replacement_costs, out = "EV"))
      )
      
      # One additional contraction iteration for numerical stability
      EV <- bellman(EV_next, costs, replacement_costs, out = "EV")
      adj <- ceiling(log10(abs(max(EV))))
      
      if (check_convergence(EV, EV_next, 1e-6*(10^adj))) { 
        print(paste0(
          "Convergence achieved after ", i, 
          " inner loops with ", j, " contractions and ", k, " nk iterations."
        ))
        return(list(EV = EV, "convergence", EV_change = max(abs(EV - EV_next))))
      }
      
    }
    
    if (i >= 5) {
      print("No convergence achieved after 5 inner loops.")
    }
  }
}

# Test the inner loop
theta <- c(2.5, 9)
output <- inner_loop(theta, EV = matrix(rep(10, 90), nrow = 90))
output$EV[1:5,]

#' # 6 Adding the outer loop using maxLik::maxBHHH and estimating the model

EV <- matrix(rep(10, 90), nrow = 90) # init

partiallf <- function(param) {
  
  theta_1 <- param[1]
  replacement_costs <- param[2]
  costs <- matrix(theta_1*(1:90)*.001, nrow = 90)
  
  # Inner fixed point loop
  output <- inner_loop(param, EV = matrix(rep(10, 90), nrow = 90))
  EV <<- output$EV
  
  pchoose0 <- bellman(EV, costs, replacement_costs, out = "pchoose0")
  
  pchoose0df <- tibble(
    x = 1:90,
    pchoose0 = as.vector(pchoose0)
  )
  
  # Add log likelihoods of choices to replace or not from the dataset
  l <- rust %>% 
    # choices d_t: `replace`
    select(replace, mileage) %>%
    # compare d_t to pchoose0 (or pchoose1 if d = 1)
    left_join(pchoose0df, by = c("mileage" = "x")) %>%
    mutate(
      pchoose1 = 1 - pchoose0,
      lp = log(if_else(replace == F, pchoose0, pchoose1))
    )
  pll <- l$lp
  return(pll)
}

#+ results = FALSE
ml <- maxBHHH(partiallf, start = c(5, 5))

#+ results = TRUE
summary(ml)

#' $\theta_1$ = 2.78 (cost of accumulating mileage);
#' 
#' $\theta_2$ = 10.16 (cost of replacing the engine)
#'
#' Plot EV: the expected value of being in a certain state

tibble(
  EV = as.vector(EV),
  x = 1:90
) %>%
  ggplot(aes(x = x, y = EV)) +
  geom_line()  +
  ggtitle("Expected Value")

#' Plot pchoose0: the probability the agent will choose to keep the engine
#' instead of replacing it at each mileage state.

bellman(EV, costs = 2.782828*(1:90)*.001, replacement_costs = 10.155617, out = "pchoose0") %>%
  as.vector() %>%
  tibble(
    p = .,
    x = 1:90
  ) %>%
  ggplot(aes(x = x, y = p)) +
  geom_line()  +
  ggtitle("Probability of keeping the engine")
