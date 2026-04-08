# =====================================================
# SCENARIER
# =====================================================
# Dette scriptet definerer ulike policy-scenarier som
# påvirker kostnadsstrukturen i modellen.
# =====================================================


# -----------------------------------------------------
# REFERANSESCENARIO (BaU)
# -----------------------------------------------------
# Ingen endringer i dagens politikk

scenario_BaU <- function(params) {
  return(params)
}


# -----------------------------------------------------
# SCENARIO 1: ETS2 innføres
# -----------------------------------------------------
# ETS2 erstatter CO2-prisen for begge aktører siden 
# denne ikke videreføres

scenario_1_ETS2 <- function(params) {
  params_copy <- params
  
  # ETS2 pris (konvertert til kr per tonnkm)
  ETS2 <- 0.05170
  
  # CO2-avgift fjernes og erstattes av ETS2
  params_copy$tau_N <- ETS2
  params_copy$tau_U <- ETS2
  
  return(params_copy)
}


# -----------------------------------------------------
# SCENARIO 2: ETS2 + nasjonal CO2-avgift
# -----------------------------------------------------
# Norge møter både ETS2 og eksisterende CO2-avgift,
# mens utenlandske aktører kun møter ETS2 (forenkling)

scenario_ETS2_CO2 <- function(params) {
  
  params_copy <- params
  
  ETS2_price <- 1115
  
  # Norge: begge avgifter
  params_copy$tau_N <- params_copy$tau_N + ETS2_price
  
  # Utlandet: kun ETS2
  params_copy$tau_U <- ETS2_price
  
  return(params_copy)
}


# -----------------------------------------------------
# SCENARIO 3: Elektriske lastebiler
# -----------------------------------------------------
# Her kan du senere legge inn en tredje tilbudsfunksjon
# for elektriske lastebiler

# scenario_EV <- function(params) {
#   # TODO: implementer egen kostnadsstruktur
# }