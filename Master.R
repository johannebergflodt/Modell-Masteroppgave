# =====================================================
# HOVEDSCRIPT
# =====================================================
# Dette scriptet kjører hele modellen

# Laster inn alle deler av modellen
source("Functions.R")
source("Data.R")
source("Scenarios.R")
source("Solver.R")
source("Method.R")
source("Results.R")
source("Sensitivity.R")


# -------------------------
# KJØRER MODELLEN
# -------------------------

# Referansescenario
result_rs <- solve_equilibrium(scenario_rs(params))

# Scenario 1
params_s1 <- scenario_1_ETS2(params)
result_s1 <- solve_equilibrium(params_s1)

# Scenario 2
params_s2 <- scenario_2_ETS2_CO2(params)
result_s2 <- solve_equilibrium(params_s2)

# Scenario 3
params_s3  <- scenario_3(params)
result_s3  <- solve_equilibrium_EL(params_s3)

# Scenario 3A
params_s3A <- scenario_3A(params)
result_s3A <- solve_equilibrium_EL(params_s3A)

# Scenario 3B
params_s3B <- scenario_3B(params)
result_s3B <- solve_equilibrium_EL(params_s3B)
