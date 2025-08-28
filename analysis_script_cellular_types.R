
# ==============================================
# SCRIPT COMPLETO - ANÁLISIS CELULAR POR MUESTRA
# ==============================================

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(car)
library(openxlsx)
library(nortest)
library(broom)
library(FSA)
library(multcomp)
library(stringr)

# ============
# Cargar datos
# ============
data <- read_delim("types_combined_FILTERED.csv", delim = ";")

# Extraer el ID de muestra (ej. 2PEL1 de 2PEL1_3_invert.tif)
data <- data %>% mutate(Sample_ID = str_extract(Name, "^[^_]+"))

# =========================================================
# Paso 1: Recuento de células por muestra y tipo celular
# =========================================================
counts <- data %>% 
  group_by(Sample_ID, Phenotype, Cellular_Type, Name) %>% 
  summarise(Cell_Count = n(), .groups = "drop") %>%
  group_by(Sample_ID, Phenotype, Cellular_Type) %>%
  summarise(Mean_Cell_Count = mean(Cell_Count), .groups = "drop")

# =========================================================
# Paso 2: Promedios por muestra en variables morfológicas
# =========================================================
variables <- c("Area", "Circularity", "Max_diameter", "Min_diameter")

data_avg <- data %>% 
  group_by(Sample_ID, Phenotype, Cellular_Type) %>% 
  summarise(across(all_of(variables), mean, na.rm = TRUE), .groups = "drop")

# =========================================================
# Paso 3: Unir para análisis y visualizar
# =========================================================
data_combined <- left_join(data_avg, counts, by = c("Sample_ID", "Phenotype", "Cellular_Type"))

# Reordenar columnas
data_combined <- data_combined %>% relocate(Sample_ID, Phenotype, Cellular_Type)

# ===============
# Funciones utils
# ===============

p_to_stars <- function(p) {
  if (is.na(p)) return("n.s.")
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("n.s.")
}

evaluate_and_analyze <- function(df, variable) {
  formula <- reformulate("Phenotype * Cellular_Type", response = variable)
  df$Phenotype <- factor(df$Phenotype)
  df$Cellular_Type <- factor(df$Cellular_Type)
  
  normality <- df %>%
    group_by(Phenotype, Cellular_Type) %>%
    summarise(
      n = n(),
      shapiro_p = ifelse(n > 3, shapiro.test(.data[[variable]])$p.value, NA),
      ad_p = ifelse(n > 7, ad.test(.data[[variable]])$p.value, NA),
      normal = ifelse(!is.na(shapiro_p) && shapiro_p > 0.05, TRUE,
                      ifelse(!is.na(ad_p) && ad_p > 0.05, TRUE, FALSE)),
      .groups = "drop"
    )

  normal_all <- all(normality$normal, na.rm = TRUE)
  levene <- broom::tidy(leveneTest(formula, data = df))
  homogeneity <- if (!is.null(levene)) levene$p.value[1] > 0.05 else FALSE

  if (normal_all && homogeneity) {
    model <- aov(formula, data = df)
    tukey <- glht(model, linfct = mcp(`Phenotype:Cellular_Type` = "Tukey"))
    summary_res <- summary(tukey)$test
    tukey_df <- data.frame(
      Comparison = rownames(summary_res$coefficients),
      P.adj = summary_res$pvalues,
      Z = summary_res$tstat,
      Metodo = "Tukey"
    )
    return(list(method = "ANOVA", results = tukey_df, normality = normality, levene = levene))
  } else {
    kruskal <- kruskal.test(formula, data = df)
    dunn_res <- dunnTest(formula, data = df, method = "bonferroni")$res
    dunn_res$Metodo <- "Dunn"
    return(list(method = "Kruskal", results = dunn_res[, c("Comparison", "P.adj", "Z", "Metodo")],
                normality = normality, levene = levene))
  }
}

# ===============================
# Paso 4: Aplicar a todas variables
# ===============================

resumen_final <- data.frame()
posthoc_results <- list()
normality_all <- list()
levene_all <- list()

for (v in variables) {
  result <- evaluate_and_analyze(data_combined, v)
  resumen_final <- rbind(resumen_final, data.frame(
    Variable = v,
    Metodo = result$method,
    Comparaciones = nrow(result$results),
    Comparaciones_significativas = sum(result$results$P.adj < 0.05, na.rm = TRUE)
  ))
  posthoc_results[[paste0("PostHoc_", v)]] <- result$results
  normality_all[[v]] <- result$normality
  levene_all[[v]] <- result$levene
}

# ====================
# Paso 5: Exportar Excel
# ====================

write.xlsx(c(
  list(Resumen_Significancia = resumen_final),
  list(Recuentos_Promedios = counts),
  list(Promedios_Medidas = data_combined),
  list(Supuestos_Normalidad = bind_rows(normality_all, .id = "Variable")),
  list(Supuestos_Levene = bind_rows(levene_all, .id = "Variable")),
  posthoc_results
), file = "RESULTADOS_FINAL_ANALISIS.xlsx")
