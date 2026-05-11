# ============================================================
# RESULTATER OG ANALYSE
# ============================================================

# --------------------------------------------------------
# LASTEBILBEREGNING
# --------------------------------------------------------
km_per_bil     <- 34988
last_per_bil   <- 14.7
levetid        <- 7      # Antall lastebiler må tolkes over levetiden
tonnkm_per_bil <- km_per_bil * last_per_bil * levetid / 1e9

# --------------------------------------------------------
# PROSENTVIS ENDRING
# --------------------------------------------------------

# Scenario 1 og 2 - sammenlignet med referansescenario
pst_P_s1  <- (result_s1["P"]   - result_rs["P"])   / result_rs["P"]   * 100
pst_P_s2  <- (result_s2["P"]   - result_rs["P"])   / result_rs["P"]   * 100
pst_XN_s1 <- (result_s1["X_N"] - result_rs["X_N"]) / result_rs["X_N"] * 100
pst_XN_s2 <- (result_s2["X_N"] - result_rs["X_N"]) / result_rs["X_N"] * 100
pst_XU_s1 <- (result_s1["X_U"] - result_rs["X_U"]) / result_rs["X_U"] * 100
pst_XU_s2 <- (result_s2["X_U"] - result_rs["X_U"]) / result_rs["X_U"] * 100

# Scenario 3 - sammenlignet med scenario 1
pst_P_s3  <- (result_s3["P"]   - result_s1["P"])   / result_s1["P"]   * 100
pst_XN_s3 <- (result_s3["X_N"] - result_s1["X_N"]) / result_s1["X_N"] * 100
pst_XU_s3 <- (result_s3["X_U"] - result_s1["X_U"]) / result_s1["X_U"] * 100

# Scenario 3A og 3B - sammenlignet med scenario 3
pst_P_s3A   <- (result_s3A["P"]    - result_s3["P"])    / result_s3["P"]    * 100
pst_P_s3B   <- (result_s3B["P"]    - result_s3["P"])    / result_s3["P"]    * 100
pst_XN_s3A  <- (result_s3A["X_N"]  - result_s3["X_N"])  / result_s3["X_N"]  * 100
pst_XN_s3B  <- (result_s3B["X_N"]  - result_s3["X_N"])  / result_s3["X_N"]  * 100
pst_XU_s3A  <- (result_s3A["X_U"]  - result_s3["X_U"])  / result_s3["X_U"]  * 100
pst_XU_s3B  <- (result_s3B["X_U"]  - result_s3["X_U"])  / result_s3["X_U"]  * 100
pst_XEL_s3A <- (result_s3A["X_EL"] - result_s3["X_EL"]) / result_s3["X_EL"] * 100
pst_XEL_s3B <- (result_s3B["X_EL"] - result_s3["X_EL"]) / result_s3["X_EL"] * 100

# --------------------------------------------------------
# ABSOLUTT ENDRING (mrd. tonnkm)
# --------------------------------------------------------

# Scenario 1 og 2 - sammenlignet med referansescenario
delta_XN_s1 <- result_s1["X_N"] - result_rs["X_N"]
delta_XN_s2 <- result_s2["X_N"] - result_rs["X_N"]
delta_XU_s1 <- result_s1["X_U"] - result_rs["X_U"]
delta_XU_s2 <- result_s2["X_U"] - result_rs["X_U"]

# Scenario 3 - sammenlignet med scenario 1
delta_XN_s3 <- result_s3["X_N"] - result_s1["X_N"]
delta_XU_s3 <- result_s3["X_U"] - result_s1["X_U"]

# Scenario 3A og 3B - sammenlignet med scenario 3
delta_XN_s3A  <- result_s3A["X_N"]  - result_s3["X_N"]
delta_XN_s3B  <- result_s3B["X_N"]  - result_s3["X_N"]
delta_XU_s3A  <- result_s3A["X_U"]  - result_s3["X_U"]
delta_XU_s3B  <- result_s3B["X_U"]  - result_s3["X_U"]
delta_XEL_s3A <- result_s3A["X_EL"] - result_s3["X_EL"]
delta_XEL_s3B <- result_s3B["X_EL"] - result_s3["X_EL"]

# --------------------------------------------------------
# MARKEDSANDEL
# --------------------------------------------------------
total_rs  <- result_rs["X_N"]  + result_rs["X_U"]
total_s1  <- result_s1["X_N"]  + result_s1["X_U"]
total_s2  <- result_s2["X_N"]  + result_s2["X_U"]
total_s3  <- result_s3["X_N"]  + result_s3["X_U"]  + result_s3["X_EL"]
total_s3A <- result_s3A["X_N"] + result_s3A["X_U"] + result_s3A["X_EL"]
total_s3B <- result_s3B["X_N"] + result_s3B["X_U"] + result_s3B["X_EL"]

andel_rs <- result_rs["X_N"] / total_rs * 100
andel_s1 <- result_s1["X_N"] / total_s1 * 100
andel_s2 <- result_s2["X_N"] / total_s2 * 100

andel_N_s3   <- result_s3["X_N"]   / total_s3  * 100
andel_N_s3A  <- result_s3A["X_N"]  / total_s3A * 100
andel_N_s3B  <- result_s3B["X_N"]  / total_s3B * 100
andel_U_s3   <- result_s3["X_U"]   / total_s3  * 100
andel_U_s3A  <- result_s3A["X_U"]  / total_s3A * 100
andel_U_s3B  <- result_s3B["X_U"]  / total_s3B * 100
andel_EL_s3  <- result_s3["X_EL"]  / total_s3  * 100
andel_EL_s3A <- result_s3A["X_EL"] / total_s3A * 100
andel_EL_s3B <- result_s3B["X_EL"] / total_s3B * 100

# --------------------------------------------------------
# ANTALL LASTEBILER
# --------------------------------------------------------
biler_N_s1   <- result_s1["X_N"]   / tonnkm_per_bil
biler_U_s1   <- result_s1["X_U"]   / tonnkm_per_bil
biler_N_s2   <- result_s2["X_N"]   / tonnkm_per_bil
biler_U_s2   <- result_s2["X_U"]   / tonnkm_per_bil
biler_N_s3   <- result_s3["X_N"]   / tonnkm_per_bil
biler_U_s3   <- result_s3["X_U"]   / tonnkm_per_bil
biler_EL_s3  <- result_s3["X_EL"]  / tonnkm_per_bil
biler_N_s3A  <- result_s3A["X_N"]  / tonnkm_per_bil
biler_U_s3A  <- result_s3A["X_U"]  / tonnkm_per_bil
biler_EL_s3A <- result_s3A["X_EL"] / tonnkm_per_bil
biler_N_s3B  <- result_s3B["X_N"]  / tonnkm_per_bil
biler_U_s3B  <- result_s3B["X_U"]  / tonnkm_per_bil
biler_EL_s3B <- result_s3B["X_EL"] / tonnkm_per_bil

# --------------------------------------------------------
# SUBSIDIE (mrd. kr per år)
# --------------------------------------------------------
subsidie_s3  <- params$s_EL      * result_s3["X_EL"]
subsidie_s3A <- params_s3A$s_EL  * result_s3A["X_EL"]
subsidie_s3B <- 0

# --------------------------------------------------------
# UTSKRIFT
# --------------------------------------------------------

cat("\n=== REFERANSESCENARIO ===\n")
cat("Pris:                ", round(result_rs["P"], 4),   "kr/tonnkm\n")
cat("X_N:                 ", round(result_rs["X_N"], 4), "mrd. tonnkm\n")
cat("X_U:                 ", round(result_rs["X_U"], 4), "mrd. tonnkm\n")
cat("Totalmarked:         ", round(total_rs, 4),         "mrd. tonnkm\n")
cat("Markedsandel norske: ", round(andel_rs, 1),         "%\n")

cat("\n=== SCENARIO 1: ETS2 ===\n")
cat("Pris:                ", round(result_s1["P"], 4),   "kr/tonnkm\n")
cat("X_N:                 ", round(result_s1["X_N"], 4), "mrd. tonnkm\n")
cat("X_U:                 ", round(result_s1["X_U"], 4), "mrd. tonnkm\n")
cat("Totalmarked:         ", round(total_s1, 4),         "mrd. tonnkm\n\n")
cat("Prisendring:         ", round(pst_P_s1, 4),         "% (vs. referanse)\n")
cat("X_N endring:         ", round(delta_XN_s1, 4),      "mrd. tonnkm (", round(pst_XN_s1, 3), "%)\n")
cat("X_U endring:         ", round(delta_XU_s1, 4),      "mrd. tonnkm (", round(pst_XU_s1, 3), "%)\n")
cat("Markedsandel norske: ", round(andel_s1, 1),         "%\n")
cat("Antall lastebiler N: ", round(biler_N_s1, 0),       "\n")
cat("Antall lastebiler U: ", round(biler_U_s1, 0),       "\n")

cat("\n=== SCENARIO 2 ===\n")
cat("Pris:                ", round(result_s2["P"], 4),   "kr/tonnkm\n")
cat("X_N:                 ", round(result_s2["X_N"], 4), "mrd. tonnkm\n")
cat("X_U:                 ", round(result_s2["X_U"], 4), "mrd. tonnkm\n")
cat("Totalmarked:         ", round(total_s2, 4),         "mrd. tonnkm\n\n")
cat("Prisendring:         ", round(pst_P_s2, 4),         "% (vs. referanse)\n")
cat("X_N endring:         ", round(delta_XN_s2, 4),      "mrd. tonnkm (", round(pst_XN_s2, 3), "%)\n")
cat("X_U endring:         ", round(delta_XU_s2, 4),      "mrd. tonnkm (", round(pst_XU_s2, 3), "%)\n")
cat("Markedsandel norske: ", round(andel_s2, 1),         "%\n")
cat("Antall lastebiler N: ", round(biler_N_s2, 0),       "\n")
cat("Antall lastebiler U: ", round(biler_U_s2, 0),       "\n")

cat("\n=== SCENARIO 3: ELEKTRISKE LASTEBILER ===\n")
cat("Pris:                ", round(result_s3["P"], 4),    "kr/tonnkm\n")
cat("X_N:                 ", round(result_s3["X_N"], 4),  "mrd. tonnkm\n")
cat("X_U:                 ", round(result_s3["X_U"], 4),  "mrd. tonnkm\n")
cat("X_EL:                ", round(result_s3["X_EL"], 4), "mrd. tonnkm\n")
cat("Totalmarked:         ", round(total_s3, 4),           "mrd. tonnkm\n\n")
cat("Prisendring:         ", round(pst_P_s3, 4),           "% (vs. scenario 1)\n")
cat("X_N endring:         ", round(delta_XN_s3, 4),        "mrd. tonnkm (", round(pst_XN_s3, 3), "%)\n")
cat("X_U endring:         ", round(delta_XU_s3, 4),        "mrd. tonnkm (", round(pst_XU_s3, 3), "%)\n")
cat("Markedsandel norske: ", round(andel_N_s3, 1),         "%\n")
cat("Markedsandel utenl.: ", round(andel_U_s3, 1),         "%\n")
cat("Markedsandel el.:    ", round(andel_EL_s3, 1),        "%\n")
cat("Antall lastebiler N: ", round(biler_N_s3, 0),         "\n")
cat("Antall lastebiler U: ", round(biler_U_s3, 0),         "\n")
cat("Antall lastebiler EL:", round(biler_EL_s3, 0),        "\n")
cat("Subsidiebeløp:       ", round(subsidie_s3, 4),        "mrd. kr per år\n")

cat("\n=== SCENARIO 3A: ELEKTRISKE LASTEBILER – PESSIMISTISK 2040 ===\n")
cat("Pris:                ", round(result_s3A["P"], 4),    "kr/tonnkm\n")
cat("X_N:                 ", round(result_s3A["X_N"], 4),  "mrd. tonnkm\n")
cat("X_U:                 ", round(result_s3A["X_U"], 4),  "mrd. tonnkm\n")
cat("X_EL:                ", round(result_s3A["X_EL"], 4), "mrd. tonnkm\n")
cat("Totalmarked:         ", round(total_s3A, 4),           "mrd. tonnkm\n\n")
cat("Prisendring:         ", round(pst_P_s3A, 4),           "% (vs. scenario 3)\n")
cat("X_N endring:         ", round(delta_XN_s3A, 4),        "mrd. tonnkm (", round(pst_XN_s3A, 3), "%)\n")
cat("X_U endring:         ", round(delta_XU_s3A, 4),        "mrd. tonnkm (", round(pst_XU_s3A, 3), "%)\n")
cat("X_EL endring:        ", round(delta_XEL_s3A, 4),       "mrd. tonnkm (", round(pst_XEL_s3A, 3), "%)\n")
cat("Markedsandel norske: ", round(andel_N_s3A, 1),         "%\n")
cat("Markedsandel utenl.: ", round(andel_U_s3A, 1),         "%\n")
cat("Markedsandel el.:    ", round(andel_EL_s3A, 1),        "%\n")
cat("Antall lastebiler N: ", round(biler_N_s3A, 0),         "\n")
cat("Antall lastebiler U: ", round(biler_U_s3A, 0),         "\n")
cat("Antall lastebiler EL:", round(biler_EL_s3A, 0),        "\n")
cat("Subsidiebeløp:       ", round(subsidie_s3A, 4),        "mrd. kr per år\n")

cat("\n=== SCENARIO 3B: ELEKTRISKE LASTEBILER – OPTIMISTISK 2040 ===\n")
cat("Pris:                ", round(result_s3B["P"], 4),    "kr/tonnkm\n")
cat("X_N:                 ", round(result_s3B["X_N"], 4),  "mrd. tonnkm\n")
cat("X_U:                 ", round(result_s3B["X_U"], 4),  "mrd. tonnkm\n")
cat("X_EL:                ", round(result_s3B["X_EL"], 4), "mrd. tonnkm\n")
cat("Totalmarked:         ", round(total_s3B, 4),           "mrd. tonnkm\n\n")
cat("Prisendring:         ", round(pst_P_s3B, 4),           "% (vs. scenario 3)\n")
cat("X_N endring:         ", round(delta_XN_s3B, 4),        "mrd. tonnkm (", round(pst_XN_s3B, 3), "%)\n")
cat("X_U endring:         ", round(delta_XU_s3B, 4),        "mrd. tonnkm (", round(pst_XU_s3B, 3), "%)\n")
cat("X_EL endring:        ", round(delta_XEL_s3B, 4),       "mrd. tonnkm (", round(pst_XEL_s3B, 3), "%)\n")
cat("Markedsandel norske: ", round(andel_N_s3B, 1),         "%\n")
cat("Markedsandel utenl.: ", round(andel_U_s3B, 1),         "%\n")
cat("Markedsandel el.:    ", round(andel_EL_s3B, 1),        "%\n")
cat("Antall lastebiler N: ", round(biler_N_s3B, 0),         "\n")
cat("Antall lastebiler U: ", round(biler_U_s3B, 0),         "\n")
cat("Antall lastebiler EL:", round(biler_EL_s3B, 0),        "\n")
cat("Subsidiebeløp:       ", round(subsidie_s3B, 4),        "mrd. kr per år\n")