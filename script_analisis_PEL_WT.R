
# SCRIPT COMPLETO PARA ANALISIS PEL vs WT - Comparación de tipos celulares
# Autor: Generado por ChatGPT
# Fecha: Actual
# Output: Excel + Gráficos .svg y .jpg

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(openxlsx)
library(car)
library(nortest)
library(broom)
library(FSA)
library(multcomp)

# ========================
# 1. Cargar datos
# ========================
data <- read_csv("types_combined_FILTERED.csv")

# Extraer ID de muestra
data <- data %>%
  mutate(Sample_ID = sub("(_\\d+).*", "", Name))

# ========================
# 2. Recuento de células por muestra
# ========================
frecuencia_celulas <- data %>%
  group_by(Sample_ID, Phenotype, Cellular_Type) %>%
  summarise(N_Celulas = n(), .groups = "drop")

# ========================
# 3. Promedio por muestra
# ========================
vars <- c("Area", "Circularity", "Max_diameter", "Min_diameter")
data_promediada <- data %>%
  group_by(Sample_ID, Phenotype, Cellular_Type) %>%
  summarise(across(all_of(vars), mean, na.rm = TRUE), .groups = "drop")

# ========================
# 4. Función: Test normalidad y homogeneidad
# ========================
test_supuestos <- function(df, variable) {
  normalidad <- df %>%
    group_by(Phenotype, Cellular_Type) %>%
    summarise(
      n = n(),
      shapiro_p = ifelse(n > 3, shapiro.test(.data[[variable]])$p.value, NA),
      ad_p = ifelse(n > 7, ad.test(.data[[variable]])$p.value, NA),
      normal = ifelse(!is.na(shapiro_p) && shapiro_p > 0.05, TRUE,
                      ifelse(!is.na(ad_p) && ad_p > 0.05, TRUE, FALSE)),
      .groups = "drop"
    )

  formula <- reformulate(c("Phenotype", "Cellular_Type"), response = variable)
  levene <- broom::tidy(leveneTest(formula, data = df))

  list(normalidad = normalidad, levene = levene)
}

# ========================
# 5. Función: ANOVA / Kruskal + Posthoc
# ========================
run_analysis <- function(df, variable) {
  fml <- reformulate(c("Phenotype", "Cellular_Type"), response = variable)
  sup <- test_supuestos(df, variable)
  normal_all <- all(sup$normalidad$normal, na.rm = TRUE)
  homog <- sup$levene$p.value[1] > 0.05

  if (normal_all & homog) {
    model <- aov(fml, data = df)
    tukey <- glht(model, linfct = mcp(`Phenotype:Cellular_Type` = "Tukey"))
    res <- summary(tukey)$test
    df_posthoc <- data.frame(
      Comparison = rownames(res$coefficients),
      P.adj = res$pvalues,
      Z = res$tstat,
      Metodo = "Tukey"
    )
    method <- "ANOVA"
  } else {
    kw <- kruskal.test(fml, data = df)
    dunn <- dunnTest(fml, data = df, method = "bonferroni")$res
    df_posthoc <- dunn[, c("Comparison", "P.adj", "Z")]
    df_posthoc$Metodo <- "Dunn"
    method <- "Kruskal"
  }

  list(method = method, posthoc = df_posthoc, normalidad = sup$normalidad, levene = sup$levene)
}

# ========================
# 6. Análisis por variable
# ========================
resumen_final <- list()
posthoc_all <- list()
sup_normalidad_all <- list()
sup_levene_all <- list()

for (v in vars) {
  res <- run_analysis(data_promediada, v)
  resumen_final[[v]] <- data.frame(
    Variable = v,
    Metodo = res$method,
    Comparaciones = nrow(res$posthoc),
    Significativas = sum(res$posthoc$P.adj < 0.05, na.rm = TRUE)
  )
  posthoc_all[[paste0("PostHoc_", v)]] <- res$posthoc %>%
    mutate(Significativo = ifelse(P.adj < 0.05, "Sí", "No"))
  sup_normalidad_all[[v]] <- res$normalidad
  sup_levene_all[[v]] <- res$levene
}

# ========================
# 7. Gráficos
# ========================
dir.create("graficos", showWarnings = FALSE)

for (v in vars) {
  p <- ggplot(data_promediada, aes(x = interaction(Phenotype, Cellular_Type),
                                   y = .data[[v]], fill = Cellular_Type)) +
    geom_violin(trim = FALSE, alpha = 0.3) +
    geom_boxplot(width = 0.2, outlier.shape = 16, outlier.size = 1.5) +
    theme_classic(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste("Comparación:", v)) +
    ylab(v) + xlab("Phenotype + Cellular Type")
  ggsave(paste0("graficos/grafico_", v, ".svg"), p, width = 7, height = 6)
  ggsave(paste0("graficos/grafico_", v, ".jpg"), p, width = 7, height = 6, dpi = 300)
}

# ========================
# 8. Exportar a Excel
# ========================
export <- c(
  list(Frecuencia_Celulas = frecuencia_celulas),
  list(Datos_Promediados = data_promediada),
  list(Supuestos_Normalidad = bind_rows(sup_normalidad_all, .id = "Variable")),
  list(Supuestos_Levene = bind_rows(sup_levene_all, .id = "Variable")),
  list(Resumen_Significancia = bind_rows(resumen_final)),
  posthoc_all
)

write.xlsx(export, file = "RESULTADOS_ANALISIS_PEL_WT.xlsx")
