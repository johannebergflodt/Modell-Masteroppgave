# =====================================================
# ENDRING I MARGINALKOSTNAD
# =====================================================
# Finner hvor mye marginalkostnaden endrer seg fra
# referanse, som følge av endringer i scenario
# =====================================================


# -----------------------------------------------------
# REFERANSE
# -----------------------------------------------------
MC_baseline <- P_data


# -----------------------------------------------------
# Scenario 1
# -----------------------------------------------------
delta_MC_N_s1 <- params$alpha_N * (params$ETS2 - params$tau_N) +
  (1 - params$alpha_N) * (params$ETS2 - params$tau_U)

delta_MC_U_s1 <- params$alpha_U * (params$ETS2 - params$tau_N) +
  (1 - params$alpha_U) * (params$ETS2 - params$tau_U)


# -----------------------------------------------------
# Scenario 2
# -----------------------------------------------------
delta_MC_N_s2 <- params$alpha_N * params$ETS2 +
  (1 - params$alpha_N) * (params$ETS2 - params$tau_U)

delta_MC_U_s2 <- params$alpha_U * params$ETS2 +
  (1 - params$alpha_U) * (params$ETS2 - params$tau_U)


# -----------------------------------------------------
# Scenario 2 vs 1
# -----------------------------------------------------
delta_MC_N_s2_s1 <- params$alpha_N * params$ETS2
delta_MC_U_s2_s1 <- params$alpha_U * params$ETS2


# -----------------------------------------------------
# PROSENTVIS ENDRING 
# -----------------------------------------------------
pst_MC_N_s1 <- delta_MC_N_s1 / MC_baseline * 100
pst_MC_U_s1 <- delta_MC_U_s1 / MC_baseline * 100

pst_MC_N_s2 <- delta_MC_N_s2 / MC_baseline * 100
pst_MC_U_s2 <- delta_MC_U_s2 / MC_baseline * 100

pst_MC_N_s2_s1 <- delta_MC_N_s2_s1 / MC_baseline * 100
pst_MC_U_s2_s1 <- delta_MC_U_s2_s1 / MC_baseline * 100


# --------------------------------------------------------
# UTSKRIFT
# --------------------------------------------------------

cat("\n=== METODE: ENDRING I MARGINALKOSTNAD ===\n")

cat("\nScenario 1:\n")
cat("Norske:     ", round(delta_MC_N_s1, 5), " (",
    round(pst_MC_N_s1, 3), "%)\n")
cat("Utenlandske:", round(delta_MC_U_s1, 5), " (",
    round(pst_MC_U_s1, 3), "%)\n")

cat("\nScenario 2:\n")
cat("Norske:     ", round(delta_MC_N_s2, 5), " (",
    round(pst_MC_N_s2, 3), "%)\n")
cat("Utenlandske:", round(delta_MC_U_s2, 5), " (",
    round(pst_MC_U_s2, 3), "%)\n")

cat("\nScenario 2 vs 1:\n")
cat("Norske:     ", round(delta_MC_N_s2_s1, 5), " (",
    round(pst_MC_N_s2_s1, 3), "%)\n")
cat("Utenlandske:", round(delta_MC_U_s2_s1, 5), " (",
    round(pst_MC_U_s2_s1, 3), "%)\n\n")