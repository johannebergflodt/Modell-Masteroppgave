# =====================================================
# HOVEDSCRIPT
# =====================================================

# Laster inn alle deler av modellen
source("Functions.R")
source("Data.R")
source("Scenarios.R")
source("Solver.R")

# -------------------------
# KJØRER MODELLEN
# -------------------------

# Referansescenario BaU
result_BaU <- solve_equilibrium(params)

# Scenario 1
result_ETS2 <- solve_equilibrium(scenario_1_ETS2(params))