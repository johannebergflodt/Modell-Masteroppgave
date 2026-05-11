# =====================================================
# SCENARIOER
# =====================================================
# Dette scriptet definerer ulike policy-scenarioer som
# påvirker kostnadsstrukturen i modellen


# -----------------------------------------------------
# REFERANSESCENARIO
# -----------------------------------------------------
# Ingen endringer i dagens politikk

scenario_rs <- function(params) {
  return(params)
}


# -----------------------------------------------------
# SCENARIO 1: ETS2 innføres
# -----------------------------------------------------
# ETS2 erstatter CO2-prisen for begge aktører siden 
# denne ikke videreføres

scenario_1_ETS2 <- function(params) {
  params_copy <- params
  
  # CO2-avgift fjernes og erstattes av ETS2
  params_copy$tau_N <- params$ETS2
  params_copy$tau_U <- params$ETS2

return(params_copy)
}

# -----------------------------------------------------
# SCENARIO 2: ETS2 overlapper med CO2-avgift i Norge
# -----------------------------------------------------

scenario_2_ETS2_CO2 <- function(params) {
  
  params_copy <- params
  
  params_copy$tau_N <- params$tau_N + params$ETS2
  params_copy$tau_U <- params$ETS2
  
  return(params_copy)
}


# -----------------------------------------------------
# SCENARIO 3: Introduksjon av elektriske aktører
# -----------------------------------------------------
# Som i scenario 1 hvor ETS2 erstatter CO2-prisen
# men med elektriske lastebiler inkludert 
# Elektriske aktører inkluderes med dagens kostnadsnivå
# og gjeldende subsidie

scenario_3 <- function(params) {
  params_copy <- params
  
  # ETS2 erstatter CO2-prisen
  params_copy$tau_N <- params$ETS2
  params_copy$tau_U <- params$ETS2
  
  # Elektriske aktører: dagens kostnadsnivå og subsidie
  params_copy$a_EL <- params$a_EL
  params_copy$s_EL <- params$s_EL
  
  return(params_copy)
}


# -----------------------------------------------------
# SCENARIO 3A: Pessimistisk 2040
# -----------------------------------------------------
# Antar uendret kostnadsnivå pga. lite teknologisk fremgang
# Subsidie har blitt faset ut
# ETS2 er innført med samme politikk som i scenario 1
# ETS2-prisen er forventet å øke - er det nok til å kompensere for subsidien?

scenario_3A <- function(params) {
  params_copy <- params
  
  # ETS2 erstatter CO2-avgiften for dieselaktører
  params_copy$tau_N <- params$ETS2_2040
  params_copy$tau_U <- params$ETS2_2040
  
  # Kostnadsnivå og subsidie for elektriske aktører
  params_copy$a_EL <- params$a_EL        # uendret fra dagens nivå
  params_copy$s_EL <- 0                  # subsidie faset ut
  
  return(params_copy)
}

# -----------------------------------------------------
# SCENARIO 3B: Optimistisk 2040
# -----------------------------------------------------
# Teknologisk fremgang har gitt lavere kostnadsnivå
# Subsidie faset ut
# ETS2 fortsatt innført med samme politikk som i scenario 1
# ETS2-pris fortsatt forventet å øke

scenario_3B <- function(params) {
  params_copy <- params
  
  # ETS2 erstatter CO2-avgiften for dieselaktører
  params_copy$tau_N <- params$ETS2_2040
  params_copy$tau_U <- params$ETS2_2040
  
  # Kostnadsnivå: differansen mellom a_EL og a_N halveres
  params_copy$a_EL <- params$a_N + (params$a_EL - params$a_N) / 2
  
  return(params_copy)
}