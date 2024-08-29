#Close-view 56.7 images

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggsignif)
library(broom)
library(car)
library(openxlsx)
library(nortest)
library(ggpubr)

# Función para generar comparaciones
generate_comparisons <- function(data, group_column) {
  unique_samples <- unique(data[[group_column]])
  comparisons <- combn(unique_samples, 2, simplify = FALSE)
  comparisons
}

# Función para evaluar normalidad y homogeneidad
evaluate_normality_homogeneity <- function(data, variable) {
  data <- data %>%
    mutate(Tratamiento = as.factor(Tratamiento))
  
  results <- data %>%
    group_by(Tratamiento, Zone) %>%
    summarise(
      Observaciones = n(),
      shapiro_p = tryCatch(shapiro.test(get(variable))$p.value, error = function(e) NA),
      ad_p = ifelse(n() > 7, ad.test(get(variable))$p.value, NA),
      Normality = ifelse(!is.na(shapiro_p) && shapiro_p > 0.05, "Shapiro Normal",
                         ifelse(!is.na(ad_p) && ad_p > 0.05, "AD Normal", "Not Normal")),
      .groups = "drop"
    )
  
  levene_test <- tryCatch(leveneTest(get(variable) ~ Tratamiento, data = data), error = function(e) NULL)
  levene_result <- if (!is.null(levene_test)) tidy(levene_test) else NULL
  
  results_treatment <- data %>%
    group_by(Tratamiento) %>%
    summarise(
      Homogeneity = ifelse(!is.null(levene_result) && levene_result$p.value > 0.05, "Homogeneous", "Not Homogeneous"),
      .groups = "drop"
    )
  
  list(results = results, treatment_results = results_treatment, levene_result = levene_result)
}

# Función para realizar ANOVA o prueba de Kruskal-Wallis
perform_analysis <- function(data, variable, homogeneity_results) {
  result <- list()
  
  if (all(homogeneity_results$Homogeneity == "Homogeneous")) {
    anova_result <- aov(get(variable) ~ Tratamiento, data = data)
    result$anova <- summary(anova_result)
    result$significance <- ifelse(result$anova[[1]]["Pr(>F)"][1, 1] < 0.05, "Significant", "Not Significant")
    result$test <- "ANOVA"
  } else {
    kruskal_test <- kruskal.test(get(variable) ~ Tratamiento, data = data)
    result$kruskal <- kruskal_test
    result$significance <- ifelse(kruskal_test$p.value < 0.05, "Significant", "Not Significant")
    result$test <- "Kruskal-Wallis"
  }
  
  return(result)
}

# Leer los datos
file_path <- "E:/TEM_oocitos/selected/RESULTS_56.7/ARCHIVOS FINALES/DATA_COMBINED_GLOBAL.xlsx"
data <- read_excel(file_path)

# Evaluar normalidad y homogeneidad para Circularity
circularity_results <- evaluate_normality_homogeneity(data, "Circularity")

# Evaluar normalidad y homogeneidad para Area
area_results <- evaluate_normality_homogeneity(data, "Area")

# Realizar análisis para Circularity
circularity_analysis <- perform_analysis(data, "Circularity", circularity_results$treatment_results)

# Realizar análisis para Area
area_analysis <- perform_analysis(data, "Area", area_results$treatment_results)

# Convertir resultados de análisis a data frames
convert_analysis_to_df <- function(analysis_result) {
  if (analysis_result$test == "ANOVA") {
    df <- as.data.frame(analysis_result$anova[[1]])
  } else {
    df <- data.frame(
      statistic = analysis_result$kruskal$statistic,
      parameter = analysis_result$kruskal$parameter,
      p.value = analysis_result$kruskal$p.value,
      method = analysis_result$kruskal$method
    )
  }
  df$significance <- analysis_result$significance
  return(df)
}

circularity_analysis_df <- convert_analysis_to_df(circularity_analysis)
area_analysis_df <- convert_analysis_to_df(area_analysis)

# Función para generar gráficos de comparación dentro de cada tratamiento
generate_within_treatment_plots <- function(data, variable) {
  treatments <- unique(data$Tratamiento)
  significance_results <- list()
  
  for (treatment in treatments) {
    data_treatment <- data %>% filter(Tratamiento == treatment)
    comparisons <- generate_comparisons(data_treatment, "Zone")
    
    observations_plot_within <- ggplot(data_treatment, aes(x = Zone, y = !!sym(variable), fill = Zone)) +
      geom_boxplot() +
      geom_violin(alpha = 0.3) +
      stat_compare_means(method = "t.test", label = "p.signif", comparisons = comparisons, method.args = list(exact = FALSE)) +
      ggtitle(paste("Observations within", variable, "-", treatment)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(observations_plot_within)
    ggsave(paste0("E:/TEM_oocitos/selected/RESULTS_56.7/observations_plot_within_", variable, "_", treatment, ".png"), plot = observations_plot_within, create.dir = TRUE)
    
    # Guardar resultados de significancia
    for (comparison in comparisons) {
      result <- t.test(data_treatment[[variable]][data_treatment$Zone == comparison[1]], 
                       data_treatment[[variable]][data_treatment$Zone == comparison[2]],
                       exact = FALSE)
      significance_results <- append(significance_results, list(
        data.frame(
          Tratamiento = treatment,
          Variable = variable,
          Comparison = paste(comparison, collapse = " vs "),
          p.value = result$p.value,
          method = result$method
        )
      ))
    }
  }
  
  significance_results_df <- bind_rows(significance_results)
  return(significance_results_df)
}

# Función para generar gráficos de comparación entre tratamientos
generate_between_treatment_plots <- function(data, variable, comparisons_between) {
  plot_between <- ggplot(data, aes(x = Tratamiento, y = !!sym(variable), fill = Tratamiento)) +
    geom_boxplot() +
    geom_violin(alpha = 0.3) +
    stat_compare_means(comparisons = comparisons_between, label = "p.signif", method.args = list(exact = FALSE)) +
    ggtitle(paste("Between Treatments:", variable)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(plot_between)
  ggsave(paste0("E:/TEM_oocitos/selected/RESULTS_56.7/plot_between_", variable, ".png"), plot = plot_between, create.dir = TRUE)
  
  # Guardar resultados de significancia
  significance_results <- list()
  for (comparison in comparisons_between) {
    result <- t.test(data[[variable]][data$Tratamiento == comparison[1]], 
                     data[[variable]][data$Tratamiento == comparison[2]],
                     exact = FALSE)
    significance_results <- append(significance_results, list(
      data.frame(
        Variable = variable,
        Comparison = paste(comparison, collapse = " vs "),
        p.value = result$p.value,
        method = result$method
      )
    ))
  }
  
  significance_results_df <- bind_rows(significance_results)
  return(significance_results_df)
}

# Generar gráficos de comparación dentro de cada tratamiento para Circularity
circularity_within_significance <- generate_within_treatment_plots(data, "Circularity")

# Generar gráficos de comparación dentro de cada tratamiento para Area
area_within_significance <- generate_within_treatment_plots(data, "Area")

# Comparaciones entre tratamientos para Circularity y Area
circularity_comparisons_between <- list(c("DARK", "PALE"))
area_comparisons_between <- list(c("DARK", "PALE"))

# Generar gráficos de comparación entre tratamientos para Circularity
circularity_between_significance <- generate_between_treatment_plots(data, "Circularity", circularity_comparisons_between)

# Generar gráficos de comparación entre tratamientos para Area
area_between_significance <- generate_between_treatment_plots(data, "Area", area_comparisons_between)

# Guardar los resultados en un archivo Excel
write.xlsx(
  list(
    Circularity_Results = circularity_results$results,
    Area_Results = area_results$results,
    Treatment_Results = circularity_results$treatment_results,
    Circularity_Observations = data %>% select(Tratamiento, Zone, Circularity),
    Area_Observations = data %>% select(Tratamiento, Zone, Area),
    Count_Observations = data %>% group_by(Tratamiento, Zone) %>% summarise(Observaciones = n(), .groups = "drop"),
    Circularity_Analysis = circularity_analysis_df,
    Area_Analysis = area_analysis_df,
    Circularity_Within_Significance = circularity_within_significance,
    Area_Within_Significance = area_within_significance,
    Circularity_Between_Sig = circularity_between_significance,
    Area_Between_Significance = area_between_significance
  ),
  "E:/TEM_oocitos/selected/RESULTS_56.7/analisis_comparaciones_con_significancia.xlsx"
)
