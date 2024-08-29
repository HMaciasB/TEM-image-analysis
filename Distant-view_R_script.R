#Distant-view 12.3 images

library(readxl)
library(dplyr)
library(ggplot2)
library(ggsignif)
library(broom)
library(car)
library(openxlsx)
library(nortest)
library(ggpubr)

# Función para contar observaciones por muestra
count_observations_per_sample <- function(data) {
  data %>%
    group_by(Tratamiento, Muestra) %>%
    summarise(Observaciones = n(), .groups = "drop")
}

# Función para generar comparaciones
generate_comparisons <- function(data, group_column) {
  unique_samples <- unique(data[[group_column]])
  if (length(unique_samples) < 2) {
    return(list())
  }
  comparisons <- combn(unique_samples, 2, simplify = FALSE)
  comparisons
}

# Función para evaluar normalidad y homogeneidad
evaluate_normality_homogeneity <- function(data, variable) {
  data <- data %>%
    mutate(Tratamiento = as.factor(Tratamiento))
  
  results <- data %>%
    group_by(Tratamiento) %>%
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

# Función para convertir resultados de análisis a data frames
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

# Función para generar gráficos de comparación entre tratamientos
generate_between_treatment_plots <- function(data, variable, comparisons_between) {
  plot_between <- ggplot(data, aes(x = Tratamiento, y = !!sym(variable), fill = Tratamiento)) +
    geom_boxplot() +
    geom_violin(alpha = 0.3) +
    stat_compare_means(comparisons = comparisons_between, label = "p.signif", method.args = list(exact = FALSE)) +
    ggtitle(paste("Between Treatments:", variable)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(plot_between)
  ggsave(paste0("E:/TEM_oocitos/selected/RESULTS_12.3/plot_between_", variable, ".svg"), plot = plot_between, create.dir = TRUE)
  
  # Guardar resultados de significancia
  significance_results <- list()
  for (comparison in comparisons_between) {
    result <- wilcox.test(data[[variable]][data$Tratamiento == comparison[1]], 
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

# Función para generar gráficos de comparación dentro de cada tratamiento (actualizada)
generate_within_treatment_plots <- function(data, variable) {
  treatments <- unique(data$Tratamiento)
  significance_results <- list()
  
  for (treatment in treatments) {
    treatment_data <- data %>%
      filter(Tratamiento == treatment)
    
    if (nrow(treatment_data) == 0) {
      message(paste("No hay suficientes datos para el tratamiento:", treatment))
      next  # No hay suficientes datos para este tratamiento
    }
    
    comparisons_within <- generate_comparisons(treatment_data, "Muestra")
    
    if (length(comparisons_within) == 0) {
      message(paste("No hay comparaciones válidas para el tratamiento:", treatment))
      next
    }
    
    # Determinar el tipo de gráfico según la variable
    if (variable == "Observaciones") {
      plot_within <- ggplot(treatment_data, aes(x = Muestra, y = Observaciones, fill = Muestra)) +
        geom_bar(stat = "identity", position = "dodge") +
        stat_compare_means(comparisons = comparisons_within, label = "p.signif", method.args = list(exact = FALSE)) +
        ggtitle(paste("Within Treatment:", treatment, "-", variable)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylim(0, NA)
    } else {
      plot_within <- ggplot(treatment_data, aes(x = Muestra, y = !!sym(variable), fill = Muestra)) +
        geom_boxplot() +
        geom_violin(alpha = 0.3) +
        stat_compare_means(comparisons = comparisons_within, label = "p.signif", method.args = list(exact = FALSE)) +
        ggtitle(paste("Within Treatment:", treatment, "-", variable)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    print(plot_within)
    ggsave(paste0("E:/TEM_oocitos/selected/RESULTS_12.3/plot_within_", treatment, "_", variable, ".svg"), plot = plot_within, create.dir = TRUE)
    
    # Guardar resultados de significancia
    for (comparison in comparisons_within) {
      group1 <- treatment_data %>% filter(Muestra == comparison[1]) %>% pull(!!sym(variable))
      group2 <- treatment_data %>% filter(Muestra == comparison[2]) %>% pull(!!sym(variable))
      
      if (length(group1) > 0 && length(group2) > 0) {
        result <- wilcox.test(group1, group2, exact = FALSE)
        significance_results <- append(significance_results, list(
          data.frame(
            Variable = variable,
            Treatment = treatment,
            Comparison = paste(comparison, collapse = " vs "),
            p.value = result$p.value,
            method = result$method
          )
        ))
      } else {
        message(paste("Falta de datos para comparación entre", comparison[1], "y", comparison[2], "en el tratamiento:", treatment))
      }
    }
  }
  
  significance_results_df <- bind_rows(significance_results)
  return(significance_results_df)
}

# Leer los datos
file_path <- "E:/TEM_oocitos/selected/RESULTS_12.3/ARCHIVOS FINALES/combined_edit.xlsx"
data <- read_excel(file_path)

# Filtrar datos para solo incluir tratamientos "PALE" y "DARK"
filtered_data <- data %>%
  filter(Tratamiento %in% c("PALE", "DARK"))

# Calcular el número de observaciones por muestra
observations_data <- count_observations_per_sample(filtered_data)

# Unir datos de observaciones con los datos filtrados
filtered_data <- filtered_data %>%
  left_join(observations_data, by = c("Tratamiento", "Muestra"))

# Evaluar normalidad y homogeneidad para Observaciones
observaciones_results <- evaluate_normality_homogeneity(observations_data, "Observaciones")

# Evaluar normalidad y homogeneidad para Area
area_results <- evaluate_normality_homogeneity(filtered_data, "Area")

# Evaluar normalidad y homogeneidad para Distance_to_ROI
distance_results <- evaluate_normality_homogeneity(filtered_data, "Distance_to_ROI")

# Realizar análisis para Observaciones
observaciones_analysis <- perform_analysis(observations_data, "Observaciones", observaciones_results$treatment_results)

# Realizar análisis para Area
area_analysis <- perform_analysis(filtered_data, "Area", area_results$treatment_results)

# Realizar análisis para Distance_to_ROI
distance_analysis <- perform_analysis(filtered_data, "Distance_to_ROI", distance_results$treatment_results)

# Convertir resultados de análisis a data frames
observaciones_df <- convert_analysis_to_df(observaciones_analysis)
area_df <- convert_analysis_to_df(area_analysis)
distance_df <- convert_analysis_to_df(distance_analysis)

# Generar comparaciones entre tratamientos
comparisons_between <- generate_comparisons(filtered_data, "Tratamiento")

# Generar gráficos de comparación entre tratamientos
observaciones_between_significance <- generate_between_treatment_plots(observations_data, "Observaciones", comparisons_between)
area_between_significance <- generate_between_treatment_plots(filtered_data, "Area", comparisons_between)
distance_between_significance <- generate_between_treatment_plots(filtered_data, "Distance_to_ROI", comparisons_between)

# Generar gráficos de comparación dentro de cada tratamiento (actualizado)
observaciones_within_significance <- generate_within_treatment_plots(filtered_data, "Observaciones")
area_within_significance <- generate_within_treatment_plots(filtered_data, "Area")
distance_within_significance <- generate_within_treatment_plots(filtered_data, "Distance_to_ROI")

# Guardar los resultados en archivos Excel
write.xlsx(list(
  Observaciones_Results = observaciones_results$results,
  Observaciones_Treatment_Results = observaciones_results$treatment_results,
  Observaciones_Analysis = observaciones_df,
  Observaciones_Between_Sig = observaciones_between_significance,
  Observaciones_Within_Signi = observaciones_within_significance,
  Area_Results = area_results$results,
  Area_Treatment_Results = area_results$treatment_results,
  Area_Analysis = area_df,
  Area_Between_Significance = area_between_significance,
  Area_Within_Significance = area_within_significance,
  Distance_Results = distance_results$results,
  Distance_Treatment_Re = distance_results$treatment_results,
  Distance_Analysis = distance_df,
  Distance_Between_Significance = distance_between_significance,
  Distance_Within_Significance = distance_within_significance
), file = "E:/TEM_oocitos/selected/RESULTS_12.3/statistical_results_updated.xlsx")
