# --- INSTALLATION AND LOADING PACKAGES ---
packages_requis <- c(
  "shiny", "pROC", "shinythemes", "glmnet", "DT", 
  "ggplot2", "stats", "caret", "arm", "rmarkdown", 
  "dplyr", "gridExtra", "PRROC", "readxl", "tools", 
  "scales", "umap", "pheatmap", "cluster", 
  "dbscan", "isotree", "tidyr", "janitor",
  "shinyWidgets", "plotly", "MLmetrics"
)
for (pkg in packages_requis) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)
}

# --- CUSTOM CARET MODEL: DYNAMIC ANOVA FILTER INSIDE CV (FDR < 0.05) ---
glmnet_anova <- getModelInfo("glmnet", regex = FALSE)[[1]]
glmnet_anova$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  x_df <- as.data.frame(x)
  pvals <- sapply(x_df, function(v) {
    tryCatch({
      df_tmp <- data.frame(v = v, y = y)
      summary(aov(v ~ y, data = df_tmp))[[1]][["Pr(>F)"]][1]
    }, error=function(e) 1)
  })
  
  # Selection dynamique via False Discovery Rate (FDR)
  pvals_adj <- p.adjust(pvals, method = "fdr")
  top_vars <- names(pvals_adj)[which(pvals_adj < 0.05)]
  
  # Sécurité si aucune variable ne passe le test strict
  if (length(top_vars) < 2) { top_vars <- names(sort(pvals))[1:5] }
  
  x_filt <- as.matrix(x_df[, top_vars, drop = FALSE])
  fit_obj <- glmnet::glmnet(x = x_filt, y = y, alpha = param$alpha, lambda = param$lambda, ...)
  fit_obj$anova_features <- top_vars
  return(fit_obj)
}
orig_predict <- glmnet_anova$predict
glmnet_anova$predict <- function(modelFit, newdata, ...) {
  if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
  sel_vars <- modelFit$anova_features
  sel_vars <- sel_vars[sel_vars %in% colnames(newdata)]
  newdata_filt <- newdata[, sel_vars, drop = FALSE]
  orig_predict(modelFit, newdata_filt, ...)
}
orig_prob <- glmnet_anova$prob
glmnet_anova$prob <- function(modelFit, newdata, ...) {
  if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
  sel_vars <- modelFit$anova_features
  sel_vars <- sel_vars[sel_vars %in% colnames(newdata)]
  newdata_filt <- newdata[, sel_vars, drop = FALSE]
  orig_prob(modelFit, newdata_filt, ...)
}

# --- THEME COLORS ---
col_bg_main <- "#111625"
col_bg_card <- "#1A2133"
col_border <- "#2B354D"
col_text_main <- "#FFFFFF"
col_text_muted <- "#8290A8"
col_accent_blue <- "#00A6EA"
col_accent_green <- "#07C27F"
col_accent_red <- "#F83A59"

# --- USER INTERFACE (UI) ---
ui <- navbarPage(
  title = span("ODNOM MULTICLASS ELASTIC NET", style=paste0("color: ", col_text_main, "; font-weight: 800; letter-spacing: 1.5px; font-size: 20px;")),
  theme = shinytheme("slate"),
  
  header = tagList(
    tags$head(
      tags$style(HTML(paste0("
        body { background-color: ", col_bg_main, " !important; color: ", col_text_main, " !important; font-family: '-apple-system', BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }
        .navbar { background-color: ", col_bg_card, " !important; border-bottom: 2px solid ", col_accent_blue, " !important; }
        .navbar-nav > li > a { color: ", col_text_muted, " !important; font-weight: 600; }
        .navbar-nav > li > a:hover, .navbar-nav > .active > a { color: ", col_accent_blue, " !important; background-color: transparent !important; }
        a, a:hover, a:focus, a:active { text-decoration: none !important; outline: none !important; }
        .modern-card { background: ", col_bg_card, " !important; border-radius: 12px; padding: 25px; box-shadow: 0 8px 24px rgba(0,0,0,0.4); border: 1px solid ", col_border, "; margin-bottom: 22px; color: ", col_text_main, "; }
        .sidebar-panel { background: ", col_bg_card, " !important; border: 1px solid ", col_border, " !important; border-radius: 12px !important; }
        .well { background: ", col_bg_card, " !important; border: 1px solid ", col_border, " !important; }
        h1, h2, h3, h4, h5, h6 { color: ", col_text_main, " !important; font-weight: 700; }
        hr { border-top: 1px solid ", col_border, "; }
        label { color: ", col_text_main, " !important; font-weight: 600; }
        .form-control { background-color: #232D42 !important; color: ", col_text_main, " !important; border: 1px solid ", col_border, " !important; }
        .metric-table th { background-color: #232D42 !important; color: ", col_text_main, " !important; border-bottom: 2px solid ", col_accent_blue, " !important; }
        .metric-table td { border-bottom: 1px solid ", col_border, " !important; }
        .prob-row { display: flex; justify-content: space-between; align-items: center; padding: 12px; border-bottom: 1px solid ", col_border, "; }
        .prob-row:last-child { border-bottom: none; }
        .prob-class { font-weight: bold; font-size: 1.2rem; color: ", col_text_muted, "; }
        .prob-value { font-weight: 800; font-size: 1.5rem; }
        .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate { color: ", col_text_main, " !important; }
        table.dataTable tbody tr { background-color: ", col_bg_card, " !important; color: ", col_text_main, " !important; }
        table.dataTable tbody tr:hover { background-color: #232D42 !important; }
        table.dataTable tbody tr.selected { background-color: ", col_accent_blue, " !important; color: white !important; }
        table.dataTable thead th { color: ", col_text_main, " !important; }
        .btn-primary { background-color: ", col_accent_blue, " !important; border-color: ", col_accent_blue, " !important; }
        .btn-success { background-color: ", col_accent_green, " !important; border-color: ", col_accent_green, " !important; color: #111625 !important; font-weight:bold; }
        .btn-warning { background-color: #f59e0b !important; border-color: #f59e0b !important; color: #111625 !important; font-weight:bold; }
        .summary-box { background-color: #232D42; border-left: 4px solid ", col_accent_blue, "; padding: 15px; border-radius: 5px; margin-top: 15px; }
        .timeline-step { background: #232D42; border-left: 5px solid ", col_accent_blue, "; border-radius: 8px; padding: 20px; margin-bottom: 0; text-align: left; position: relative; transition: all 0.3s ease; display: block; cursor: pointer; text-decoration: none !important; }
        .timeline-step:hover { transform: scale(1.02); background-color: #2D3954 !important; border-left-color: ", col_accent_green, "; box-shadow: 0 10px 20px rgba(0,0,0,0.3); text-decoration: none !important; }
        .timeline-step:hover .timeline-title { color: ", col_accent_green, " !important; text-decoration: none !important; }
        .timeline-icon { background: ", col_accent_blue, "; color: #111625; width: 45px; height: 45px; border-radius: 50%; display: inline-flex; align-items: center; justify-content: center; font-size: 20px; margin-right: 15px; flex-shrink: 0; transition: background 0.3s ease; }
        .timeline-step:hover .timeline-icon { background: ", col_accent_green, " !important; }
        .timeline-title { font-size: 1.2rem; font-weight: bold; color: ", col_text_main, "; display: flex; align-items: center; margin-bottom: 0; transition: color 0.3s ease; text-decoration: none !important; }
        .timeline-arrow { text-align: center; font-size: 28px; color: ", col_text_muted, "; margin: 15px 0; }
        .timeline-step ul { margin-top: 8px; padding-left: 20px; color: ", col_text_muted, " !important; transition: color 0.3s ease; }
        .timeline-step li { margin-bottom: 5px; color: ", col_text_muted, " !important; transition: color 0.3s ease; }
        .timeline-step:hover ul, .timeline-step:hover li { color: ", col_text_main, " !important; text-decoration: none !important; }
      ")))
    ),
    useSweetAlert()
  ),
  
  tabPanel("1. MULTICLASS MODELING",
           sidebarLayout(
             sidebarPanel(
               h4("1. Data Import", style=paste0("color: ", col_text_main, ";")),
               fileInput("file_csv", "Import Cohort (CSV/Excel)", accept = c(".csv", "text/csv", ".xlsx", ".xls")),
               uiOutput("target_col_ui"),
               uiOutput("id_col_ui"),
               
               hr(),
               h4("2. Model Training", style=paste0("color: ", col_text_main, ";")),
               p("80/20 Split Conformal & Leakage-Free Optimization.", style=paste0("font-size:0.85em; color:", col_text_muted, "; margin-top:-5px;")),
               
               actionButton("update_model", "START TRAINING",
                            class = "btn-success btn-lg btn-block", 
                            style = "border-radius: 8px; margin-top: 15px;",
                            icon = icon("cogs")),
               
               uiOutput("automl_summary_ui")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Performances (Internal Validation)", 
                          br(),
                          uiOutput("patient_count_ui"),
                          fluidRow(
                            column(12, div(class="modern-card", style=paste0("border-left: 4px solid ", col_accent_blue, ";"),
                                           h5("Accuracy by Diagnosis (80% Train Set)", style=paste0("color:", col_accent_blue, ";")),
                                           plotOutput("plot_pred_stats", height = "350px")))
                          ),
                          fluidRow(
                            column(6, div(class="modern-card", style=paste0("border-left: 4px solid ", col_accent_green, ";"), 
                                          h5("ROC Curves (OOF - 80% Set)", style=paste0("color:", col_accent_green, ";")), plotOutput("plot_roc_multi"))),
                            column(6, div(class="modern-card", style=paste0("border-left: 4px solid ", col_accent_green, ";"), 
                                          h5("Precision-Recall Curves (OOF - 80% Set)", style=paste0("color:", col_accent_green, ";")), plotOutput("plot_pr_multi")))
                          ),
                          fluidRow(
                            column(5, div(class="modern-card", style=paste0("border-left: 4px solid ", col_accent_green, ";"), h5("Confusion Matrix (OOF - 80% Set)", style=paste0("color:", col_accent_green, ";")), plotOutput("plot_confusion"))),
                            column(7, div(class="modern-card", style=paste0("border-left: 4px solid ", col_accent_green, ";"), h5("Detailed Metrics (OOF - 80% Set)", style=paste0("color:", col_accent_green, ";")), uiOutput("metrics_detailed_ui")))
                          )
                 ),
                 tabPanel("Linear Coefficients", 
                          br(),
                          div(class="modern-card", style=paste0("border-left: 4px solid ", col_accent_blue, ";"), h5("Final Optimal Weights (Retrained on Full 80% Set)", style=paste0("color:", col_accent_blue, ";")), DTOutput("coef_table"))
                 ),
                 tabPanel("Visual Exploration (UMAP)",
                          br(),
                          fluidRow(
                            column(12, div(class="modern-card", style=paste0("border-left: 4px solid ", col_accent_blue, ";"),
                                           h5("UMAP Projection & Clustering", style=paste0("color:", col_accent_blue, ";")),
                                           p(strong("Info:"), " Applied on raw global standardized profiles (100% of cohort) prior to any feature selection to map true heterogeneity.", style=paste0("color:", col_accent_green, "; font-size:0.85em;")),
                                           plotOutput("plot_umap_hdbscan", click = "umap_click", height = "500px"),
                                           br(),
                                           fluidRow(
                                             column(7, h5("Cohort Registry", style=paste0("color:", col_text_main, ";")), DTOutput("table_all_patients")),
                                             column(5, h5("Statistical Anomalies", style=paste0("color:", col_accent_red, ";")), DTOutput("table_atypical"))
                                           )
                            ))
                          )
                 )
               )
             )
           )
  ),
  
  tabPanel("2. PATIENT DIAGNOSTIC",
           sidebarLayout(
             sidebarPanel(
               h4("Importation Rapide", style=paste0("color: ", col_text_main, ";")),
               p("Importez un fichier Excel/CSV. Les colonnes seront reconnues automatiquement.", style=paste0("font-size:0.85em; color:", col_text_muted, "; margin-top:-5px;")),
               fileInput("file_patient", "Données Patient", accept = c(".csv", "text/csv", ".xlsx", ".xls")),
               hr(),
               h4("Saisie des Biomarqueurs", style=paste0("color: ", col_text_main, ";")),
               uiOutput("dynamic_inputs"),
               hr(),
               actionButton("predict", " CALCULER PROBABILITÉS", 
                            class = "btn-primary btn-lg btn-block", style = "font-weight: 700; border-radius: 8px;", icon = icon("user-md")),
               br(),
               downloadButton("export_pdf", " EXPORTER RAPPORT", 
                              class = "btn-success btn-lg btn-block", style = "font-weight: 700; border-radius: 8px; margin-top:10px;")
             ),
             mainPanel(
               uiOutput("result_box")
             )
           )
  ),
  
  tabPanel("3. METHODOLOGY",
           fluidRow(
             column(8, offset=2,
                    div(class="modern-card", style="margin-top: 20px; padding: 40px;",
                        
                        div(style="text-align:center; font-size: 60px; color: #00A6EA; margin-bottom: 20px;", 
                            icon("dna")
                        ),
                        h2("Analysis Pipeline Architecture", style="text-align:center; font-weight:800; margin-bottom:30px;"),
                        
                        div(style="background-color: #232D42; padding: 20px; border-left: 5px solid #00A6EA; border-radius: 8px; margin-bottom: 30px;",
                            h4(icon("brain"), " Strict Leakage-Free Workflow", style="color: #FFFFFF; margin-top: 0;"),
                            p(HTML("This pipeline strictly enforces separation of concerns. <b>1)</b> It isolates 20% of patients into a Calibration Set. <b>2)</b> It runs a 5-fold CV on the remaining 80% to find optimal hyperparameters (averaging scores across folds) and plot blind OOF AUCs. <b>3)</b> It retrains a FINAL single model on the full 80% to freeze coefficients and Z-scores. <b>4)</b> It applies this frozen model to the isolated 20% to calculate the confidence threshold. <b>5)</b> It applies the same frozen weights to any new patient."), style="color: #8290A8; font-size: 0.95em;")
                        ),
                        
                        actionLink("step1", div(class="timeline-step", 
                                                div(class="timeline-title", div(class="timeline-icon", icon("project-diagram")), "1. Feature Engineering (100% of Data)"),
                                                div(style=paste0("margin-top: 10px; padding-left: 60px;"),
                                                    HTML("<ul>
                      <li>Logarithmic Transformation: <code>sign(x) * log1p(abs(x))</code></li>
                      <li>Random generation of 300 complex variables (Ratios, Differences, Products, Log-Ratios) from raw biomarkers.</li>
                    </ul>")
                                                )
                        )),
                        div(class="timeline-arrow", icon("angle-double-down")),
                        
                        actionLink("step2", div(class="timeline-step", 
                                                div(class="timeline-title", div(class="timeline-icon", icon("shield-alt")), "2. Hyperparameter Tuning & OOF (80% Train Set)"),
                                                div(style=paste0("margin-top: 10px; padding-left: 60px;"),
                                                    HTML("<em style='color: #00A6EA;'>The 80% cohort is split into 5 groups. The model trains on 4 and tests on the 5th to find optimal settings:</em>
                    <ul>
                      <li><b>a. Dynamic ANOVA Filter (Intra-fold):</b> Retains variables with an FDR adjusted p-value &lt; 0.05.</li>
                      <li><b>b. Double Process Z-Score:</b> Standardization is computed strictly within folds during this tuning phase.</li>
                      <li><b>c. Optimal Alpha & Lambda:</b> The model averages the error across the 5 folds for each hyperparameter combination and selects the best overall average.</li>
                      <li><b>d. ROC Curves (OOF):</b> Constructed by aggregating the blind predictions made on the 1/5 hold-out groups during tuning.</li>
                    </ul>")
                                                )
                        )),
                        div(class="timeline-arrow", icon("angle-double-down")),
                        
                        actionLink("step3", div(class="timeline-step", 
                                                div(class="timeline-title", div(class="timeline-icon", icon("lock")), "3. Final Retraining & Freezing (Full 80% Set)"),
                                                div(style=paste0("margin-top: 10px; padding-left: 60px;"),
                                                    HTML("<ul>
                      <li>The algorithm does <b>not</b> keep weights from a single validation fold.</li>
                      <li>It retrains one final model on the <b>entirety of the 80%</b> training set using the optimal Alpha and Lambda found in Step 2.</li>
                      <li>The Z-Score reference (mean/sd) is recalculated on this full 80% set. These final weights and Z-Scores are frozen.</li>
                    </ul>")
                                                )
                        )),
                        div(class="timeline-arrow", icon("angle-double-down")),
                        
                        actionLink("step4", div(class="timeline-step", 
                                                div(class="timeline-title", div(class="timeline-icon", icon("user-shield")), "4. Conformal Prediction (20% Calibration Set)"),
                                                div(style=paste0("margin-top: 10px; padding-left: 60px;"),
                                                    HTML("<ul>
                      <li>The completely frozen model is applied to the 20% of patients kept in the vault.</li>
                      <li>NO training occurs here. We simply extract the <b>95th percentile error threshold (1-q)</b> to establish a clinical safety guardrail.</li>
                    </ul>")
                                                )
                        )),
                        div(class="timeline-arrow", icon("angle-double-down")),
                        
                        actionLink("step5", div(class="timeline-step", 
                                                div(class="timeline-title", div(class="timeline-icon", icon("map-marked-alt")), "5. Spatial Anomalies (100% of Data)"),
                                                div(style=paste0("margin-top: 10px; padding-left: 60px;"),
                                                    HTML("<ul>
                      <li><b>UMAP Projection:</b> Non-linear dimensionality reduction mapping true patient heterogeneity, computed on raw standardized profiles.</li>
                      <li><b>HDBSCAN Clustering:</b> Density-based algorithm identifying intrinsic patient groups.</li>
                      <li><b>Isolation Forest:</b> Computes an anomaly score; patients isolating too quickly are flagged as 'Atypical'.</li>
                    </ul>")
                                                )
                        )),
                        div(class="timeline-arrow", icon("angle-double-down")),
                        
                        actionLink("step6", div(class="timeline-step", 
                                                div(class="timeline-title", div(class="timeline-icon", icon("user-md")), "6. Patient Diagnostic Module"),
                                                div(style=paste0("margin-top: 10px; padding-left: 60px;"),
                                                    HTML("<ul>
                      <li>Applies the exact same frozen weights and global Z-Scores to the new patient.</li>
                      <li>Generates direct multiclass probabilities via Softmax function.</li>
                      <li>Checks the prediction's uncertainty against the 20% Calibration Set threshold.</li>
                    </ul>")
                                                )
                        ))
                    )
             )
           )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  dark_theme <- theme_minimal(base_family="sans") + 
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      text = element_text(color = "#FFFFFF"),
      axis.text = element_text(color = "#8290A8"),
      axis.title = element_text(color = "#FFFFFF", face = "bold"),
      panel.grid.major = element_line(color = "#2B354D", linewidth = 0.5),
      panel.grid.minor = element_line(color = "#2B354D", linewidth = 0.25),
      legend.background = element_rect(fill = "transparent", color=NA),
      legend.text = element_text(color = "#FFFFFF")
    )
  
  palette_logo <- c("#00A6EA", "#07C27F", "#F83A59", "#f59e0b", "#8b5cf6")
  
  m_res <- reactiveValues()
  diag_res <- reactiveValues()
  
  clean_data <- reactive({
    req(input$file_csv)
    ext <- tools::file_ext(input$file_csv$name)
    if(tolower(ext) %in% c("xls", "xlsx")) { df <- as.data.frame(readxl::read_excel(input$file_csv$datapath)) } 
    else { df <- read.csv(input$file_csv$datapath); if(ncol(df) == 1) df <- read.csv2(input$file_csv$datapath) }
    df <- janitor::clean_names(df, case = "parsed")
    return(df)
  })
  
  observeEvent(clean_data(), { sendSweetAlert(session = session, title = "File imported!", type = "success") }, ignoreInit = TRUE)
  
  output$target_col_ui <- renderUI({ req(clean_data()); selectInput("target_col", "Diagnostic Column:", choices = names(clean_data())) })
  output$id_col_ui <- renderUI({ req(clean_data()); selectInput("id_col", "Identifier:", choices = c("Generate", names(clean_data()))) })
  
  observeEvent(input$update_model, {
    req(clean_data(), input$target_col)
    set.seed(42)
    
    PARAM_CV_FOLDS <- 5
    PARAM_ALPHA_GRID <- seq(0.1, 1, by = 0.1) 
    
    df_check <- clean_data()
    target <- input$target_col
    
    df_check <- df_check[!is.na(df_check[[target]]), ]
    class_counts <- table(df_check[[target]])
    valid_classes <- names(class_counts[class_counts > PARAM_CV_FOLDS])
    df_check <- df_check[df_check[[target]] %in% valid_classes, ]
    y_check <- factor(df_check[[target]])
    levels(y_check) <- make.names(levels(y_check), unique = TRUE)
    
    withProgress(message = "Training pipeline...", value = 0, {
      incProgress(0.1, detail = "Pre-processing...")
      df <- df_check
      if(input$id_col == "Generate") { df$id_interne <- paste0("Patient_", 1:nrow(df)) } else { df$id_interne <- df[[input$id_col]] }
      
      y <- y_check
      m_res$classes <- levels(y)
      
      df_numeric <- df[, sapply(df, is.numeric)]
      x_raw_base <- df_numeric[, setdiff(names(df_numeric), c(target, input$id_col, "id_interne"))]
      m_res$marker_names_simple <- colnames(x_raw_base) 
      
      x_generated <- sign(x_raw_base) * log1p(abs(x_raw_base))
      
      n_random_features <- 300
      cols_base <- colnames(x_generated)
      operations <- c("div", "diff", "prod", "log_ratio")
      
      feature_pool <- data.frame(
        f1 = sample(cols_base, n_random_features, replace=TRUE),
        f2 = sample(cols_base, n_random_features, replace=TRUE),
        op = sample(operations, n_random_features, replace=TRUE),
        stringsAsFactors = FALSE
      )
      feature_pool <- feature_pool[feature_pool$f1 != feature_pool$f2, ]
      feature_pool$name <- paste0(feature_pool$f1, "_", feature_pool$op, "_", feature_pool$f2)
      feature_pool <- feature_pool[!duplicated(feature_pool$name), ]
      
      eps <- 1e-6 
      for(i in 1:nrow(feature_pool)) {
        v1 <- x_generated[[feature_pool$f1[i]]]
        v2 <- x_generated[[feature_pool$f2[i]]]
        op <- feature_pool$op[i]
        if(op == "div") { val <- v1 / (abs(v2) + eps) }
        if(op == "diff") { val <- v1 - v2 }
        if(op == "prod") { val <- v1 * v2 }
        if(op == "log_ratio") { val <- sign(v1 / (abs(v2)+eps)) * log1p(abs(v1 / (abs(v2)+eps))) }
        x_generated[[feature_pool$name[i]]] <- val
      }
      
      m_res$track_n_generated <- ncol(x_generated)
      m_res$all_features <- colnames(x_generated)
      m_res$surviving_complex_features <- feature_pool
      
      incProgress(0.3, detail = "Split Calibration (Conformal)...")
      # SPLIT CONFORMAL PREDICTION IMPLEMENTATION (80/20)
      calib_idx <- createDataPartition(y, p = 0.2, list = FALSE)
      
      x_train <- x_generated[-calib_idx, , drop = FALSE]
      y_train <- y[-calib_idx]
      
      x_calib <- x_generated[calib_idx, , drop = FALSE]
      y_calib <- y[calib_idx]
      
      incProgress(0.5, detail = "CV Tuning & Final Retraining on 80% Set...")
      ctrl <- trainControl(method = "cv", number = PARAM_CV_FOLDS, savePredictions = "final", classProbs = TRUE, summaryFunction = multiClassSummary)
      tune_grid <- expand.grid(alpha = PARAM_ALPHA_GRID, lambda = 10^seq(-4, 0, length = 50))
      
      # L'algorithme fait la moyenne des plis pour trouver Alpha/Lambda, puis ré-entraîne un UNIQUE modèle final sur 100% du jeu d'entraînement (les 80%).
      fit_caret <- suppressWarnings(train(x = x_train, y = y_train, method = glmnet_anova, family = "multinomial", type.multinomial = "grouped", trControl = ctrl, preProcess = c("nzv", "medianImpute", "center", "scale"), tuneGrid = tune_grid, maxit = 100000))
      
      m_res$fit_caret <- fit_caret
      m_res$fit <- fit_caret$finalModel # <- Le modèle ré-entraîné final sur la totalité des 80%
      m_res$optimal_alpha <- fit_caret$bestTune$alpha
      m_res$optimal_lambda <- fit_caret$bestTune$lambda
      m_res$track_n_anova <- length(m_res$fit$anova_features)
      
      # Calibration 95% evaluation over dedicated 20% set (Apply frozen model)
      preds_calib <- predict(fit_caret, newdata = x_calib, type = "prob")
      true_probs_calib <- sapply(1:nrow(preds_calib), function(i) preds_calib[i, as.character(y_calib[i])])
      m_res$q_conformal <- quantile(1 - true_probs_calib, 0.95, na.rm=TRUE)
      
      # Out of Fold metrics strictly on the 80% Training set
      preds_oof <- fit_caret$pred
      preds_oof <- preds_oof[order(preds_oof$rowIndex), ]
      m_res$preds_matrix <- preds_oof[, m_res$classes]
      m_res$df_preds <- data.frame(ID = rownames(preds_oof), Real = preds_oof$obs, Predicted = preds_oof$pred)
      m_res$conf_mat_obj <- confusionMatrix(preds_oof$pred, preds_oof$obs)
      
      coef_list <- coef(m_res$fit, s = m_res$optimal_lambda)
      df_coef <- data.frame(Marker = rownames(coef_list[[1]]))
      for(c_name in m_res$classes) { df_coef[[paste0("Coef_", c_name)]] <- round(as.numeric(coef_list[[c_name]]), 4) }
      df_coef <- df_coef[df_coef$Marker != "(Intercept)", ]
      df_coef <- df_coef[rowSums(abs(df_coef[,-1]) > 1e-4) > 0, ]
      m_res$df_coef <- df_coef
      
      incProgress(0.8, detail = "Leakage-Free UMAP Spatial Projections on 100% of Cohort...")
      # LEAKAGE-FREE UMAP: Using raw standardized initial metrics (100% of data)
      preProc_umap <- preProcess(x_raw_base, method = c("medianImpute", "center", "scale"))
      x_sc_spatial <- predict(preProc_umap, x_raw_base)
      
      umap_res <- umap(x_sc_spatial, n_neighbors = min(15, nrow(x_sc_spatial) - 1))
      hdb_res <- dbscan::hdbscan(umap_res$layout, minPts = max(5, floor(nrow(umap_res$layout) * 0.05)))
      cluster_labels <- ifelse(hdb_res$cluster == 0, "Noise", paste("Cluster", hdb_res$cluster))
      
      iso_scores <- predict(isolation.forest(umap_res$layout, ntrees=100), umap_res$layout)
      is_atypical <- iso_scores > quantile(iso_scores, 0.95, na.rm = TRUE)
      m_res$df_plot_pca <- data.frame(ID = df$id_interne, UMAP1 = umap_res$layout[,1], UMAP2 = umap_res$layout[,2], Diagnostic = y, Cluster = cluster_labels, Atypical = is_atypical)
    })
    sendSweetAlert(session = session, title = "Complete", type = "success")
  })
  
  output$automl_summary_ui <- renderUI({
    req(m_res$optimal_alpha, m_res$track_n_generated)
    div(class = "summary-box", h5("🔍 Debug Pipeline & Stats", style="margin-top:0;"),
        tags$table(style="width:100%; font-size:0.9em; margin-top:10px;",
                   tags$tr(tags$td(strong("1. Features Générés:")), tags$td(m_res$track_n_generated)),
                   tags$tr(tags$td(strong("2. Post-ANOVA intra-pli (FDR<0.05):")), tags$td(m_res$track_n_anova)),
                   tags$tr(tags$td(strong("3. Seuil Calibration (1-q) sur 20%:")), tags$td(round(m_res$q_conformal, 3))),
                   tags$tr(tags$td(strong("4. Coefs Non-Nuls Finaux (Full 80%):")), tags$td(nrow(m_res$df_coef)))
        ))
  })
  
  output$patient_count_ui <- renderUI({
    req(m_res$conf_mat_obj)
    acc <- round(m_res$conf_mat_obj$overall["Accuracy"] * 100, 1)
    div(class = "modern-card", style = paste0("border-left: 4px solid ", col_accent_blue, "; padding: 15px; margin-bottom: 20px;"),
        p(HTML(paste0("Internal Validation Accuracy (80% Train Set OOF): <b style='font-size:1.2em; color:", col_accent_blue, ";'>", acc, "%</b>.")))
    )
  })
  
  output$plot_pred_stats <- renderPlot({
    req(m_res$df_preds)
    df_plot <- m_res$df_preds %>% mutate(Status = ifelse(Real == Predicted, "True Positive", "Error")) %>% group_by(Real, Status) %>% summarise(Count = n(), .groups = "drop")
    ggplot(df_plot, aes(x = Real, y = Count, fill = Status)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("True Positive" = col_accent_green, "Error" = col_accent_red)) +
      dark_theme + labs(x = "Real Diagnosis", y = "Count") + theme(legend.position = "top", legend.title = element_blank())
  }, bg="transparent")
  
  output$plot_confusion <- renderPlot({
    req(m_res$conf_mat_obj)
    cm_data <- as.data.frame(m_res$conf_mat_obj$table)
    ggplot(cm_data, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile(color = "#1A2133") +
      scale_fill_gradient(low = "#232D42", high = col_accent_green) +
      geom_text(aes(label = Freq), fontface="bold", color="white") +
      dark_theme + labs(x = "Clinical Reality (80% Set)", y = "Model Prediction (OOF)") + theme(legend.position="none")
  }, bg="transparent")
  
  output$metrics_detailed_ui <- renderUI({
    req(m_res$conf_mat_obj)
    by_class <- m_res$conf_mat_obj$byClass
    perf <- as.data.frame(by_class)
    if(length(m_res$classes) == 2) { perf <- data.frame(t(perf)); rownames(perf) <- m_res$classes[1] } else { rownames(perf) <- gsub("Class: ", "", rownames(perf)) }
    html_res <- paste0("<table class='metric-table' style='width:100%;'><tr><th>Class</th><th>Sens.</th><th>Spec.</th><th>PPV</th><th>F1</th></tr>")
    for(i in 1:nrow(perf)) {
      html_res <- paste0(html_res, "<tr><td><b>", rownames(perf)[i], "</b></td><td>", round(perf$Sensitivity[i]*100, 1), "%</td><td>", round(perf$Specificity[i]*100, 1), "%</td><td>", round(perf$Pos.Pred.Value[i]*100, 1), "%</td><td>", round(perf$F1[i], 2), "</td></tr>")
    }
    HTML(paste0(html_res, "</table>"))
  })
  
  output$plot_roc_multi <- renderPlot({
    req(m_res$preds_matrix)
    par(bg = NA)
    plot(NULL, xlim=c(1,0), ylim=c(0,1), xlab="Specificity", ylab="Sensitivity", main="ROC Curves (OOF 80%)", col.axis="white", col.main="white", col.lab="white", fg="white")
    for(i in 1:length(m_res$classes)) {
      binary_obs <- ifelse(m_res$df_preds$Real == m_res$classes[i], 1, 0)
      plot(roc(binary_obs, m_res$preds_matrix[, m_res$classes[i]], quiet=TRUE), add=TRUE, col=palette_logo[i], lwd=2)
    }
  }, bg="transparent")
  
  output$plot_pr_multi <- renderPlot({
    req(m_res$preds_matrix)
    par(bg = NA)
    plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab="Recall", ylab="Precision", main="PR Curves (OOF 80%)", col.axis="white", col.main="white", col.lab="white", fg="white")
    for(i in 1:length(m_res$classes)) {
      binary_obs <- ifelse(m_res$df_preds$Real == m_res$classes[i], 1, 0)
      pr_obj <- pr.curve(scores.class0 = m_res$preds_matrix[, m_res$classes[i]][binary_obs==1], scores.class1 = m_res$preds_matrix[, m_res$classes[i]][binary_obs==0], curve=TRUE)
      lines(pr_obj$curve[,1], pr_obj$curve[,2], col=palette_logo[i], lwd=2)
    }
  }, bg="transparent")
  
  output$coef_table <- renderDT({ datatable(m_res$df_coef, options = list(pageLength = 15, scrollX = TRUE, dom='ftpi')) %>% formatRound(columns = 2:ncol(m_res$df_coef), digits = 4) })
  
  observeEvent(input$umap_click, {
    req(m_res$df_plot_pca)
    res <- nearPoints(m_res$df_plot_pca, input$umap_click, xvar="UMAP1", yvar="UMAP2", maxpoints=1)
    if(nrow(res) > 0) {
      idx <- which(m_res$df_plot_pca$ID == res$ID[1])
      dataTableProxy("table_all_patients") %>% selectRows(idx)
    } else {
      dataTableProxy("table_all_patients") %>% selectRows(NULL)
    }
  })
  
  output$plot_umap_hdbscan <- renderPlot({
    req(m_res$df_plot_pca)
    p <- ggplot(m_res$df_plot_pca, aes(x=UMAP1, y=UMAP2, color=Diagnostic)) +
      stat_ellipse(aes(group=Cluster, fill=Cluster), geom="polygon", alpha=0.1, show.legend=FALSE, linetype=2) +
      geom_point(aes(shape=Atypical, size=Atypical, stroke=Atypical), alpha=0.9) +
      scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 4)) +
      scale_size_manual(values = c("FALSE" = 3, "TRUE" = 6)) +
      scale_discrete_manual(aesthetics = "stroke", values = c("FALSE" = 0.5, "TRUE" = 2.5)) +
      scale_color_manual(values = palette_logo) +
      dark_theme + labs(title="UMAP Space (Raw Profile Leakage-Free)")
    
    sel <- input$table_all_patients_rows_selected
    if(length(sel) > 0) { p <- p + geom_point(data=m_res$df_plot_pca[sel, , drop=FALSE], color="#FFFFFF", size=8, shape=21, stroke=3) }
    p
  }, bg="transparent")
  
  output$table_all_patients <- renderDT({ datatable(m_res$df_plot_pca[, c("ID", "Diagnostic", "Cluster")], selection = "single", options=list(pageLength=5)) })
  output$table_atypical <- renderDT({ datatable(m_res$df_plot_pca[m_res$df_plot_pca$Atypical == TRUE, c("ID", "Diagnostic")], selection = "none", options=list(pageLength=5, dom='t')) })
  
  observeEvent(input$file_patient, {
    req(input$file_patient, m_res$marker_names_simple)
    ext <- tools::file_ext(input$file_patient$name)
    df_pat <- if(tolower(ext) %in% c("xls", "xlsx")) as.data.frame(readxl::read_excel(input$file_patient$datapath)) else read.csv(input$file_patient$datapath)
    df_pat <- janitor::clean_names(df_pat, case = "parsed")
    names_pat <- colnames(df_pat)
    
    for(m in m_res$marker_names_simple) {
      match_col <- names_pat[tolower(names_pat) == tolower(m)]
      if(length(match_col) > 0) {
        val <- df_pat[1, match_col[1]]
        updateTextInput(session, paste0("input_", m), value = as.character(val))
      }
    }
  })
  
  output$dynamic_inputs <- renderUI({ req(m_res$marker_names_simple); do.call(tagList, lapply(m_res$marker_names_simple, function(marker) { textInput(paste0("input_", marker), label = marker, value = "0") })) })
  
  observeEvent(input$predict, {
    req(m_res$fit_caret)
    new_data_raw <- data.frame(matrix(ncol = length(m_res$marker_names_simple), nrow = 1))
    colnames(new_data_raw) <- m_res$marker_names_simple
    for(m in m_res$marker_names_simple) {
      val <- input[[paste0("input_", m)]]; val_str <- trimws(gsub(",", ".", as.character(val)))
      val_num <- suppressWarnings(as.numeric(val_str)); new_data_raw[1, m] <- ifelse(is.na(val_num), 0, val_num)
    }
    new_data_log <- as.data.frame(lapply(new_data_raw, function(x) sign(x) * log1p(abs(x))))
    new_data_full <- new_data_log
    if(!is.null(m_res$surviving_complex_features) && nrow(m_res$surviving_complex_features) > 0) {
      eps <- 1e-6
      for(i in 1:nrow(m_res$surviving_complex_features)) {
        c1 <- m_res$surviving_complex_features$f1[i]; c2 <- m_res$surviving_complex_features$f2[i]; op <- m_res$surviving_complex_features$op[i]
        v1 <- new_data_log[[c1]]; v2 <- new_data_log[[c2]]
        val <- if(op == "div") { v1 / (abs(v2) + eps) } else if(op == "diff") { v1 - v2 } else if(op == "prod") { v1 * v2 } else { sign(v1 / (abs(v2)+eps)) * log1p(abs(v1 / (abs(v2)+eps))) }
        new_data_full[[m_res$surviving_complex_features$name[i]]] <- val
      }
    }
    
    # Application du modèle figé (poids optimaux de l'Elastic Net et Z-scores globaux) au patient
    raw_probs <- predict(m_res$fit_caret, newdata = new_data_full[, m_res$all_features, drop=FALSE], type = "prob")
    final_probs <- as.numeric(raw_probs[1, ]); names(final_probs) <- colnames(raw_probs)
    diag_res$probs <- data.frame(Classe = names(final_probs), Probabilite = final_probs)
    diag_res$predicted_class <- names(final_probs)[which.max(final_probs)]
    diag_res$max_prob <- max(final_probs)
  })
  
  output$result_box <- renderUI({
    req(diag_res$probs)
    nc_patient <- 1 - max(diag_res$probs$Probabilite)
    
    # Comparaison de l'incertitude du patient au seuil (q) extrait de la Calibration 20%
    is_certain <- nc_patient <= m_res$q_conformal
    
    conformal_status <- if(is_certain) {
      paste0("<div style='background-color:rgba(7, 194, 127, 0.15); color:", col_accent_green, "; padding:10px; border-radius:8px; font-weight:bold; margin-top:15px; border: 1px solid ", col_accent_green, ";'><i class='fa fa-check-circle'></i> Empirical Confidence Threshold Met (1-q &le; ", round(m_res$q_conformal, 3), ")</div>")
    } else {
      paste0("<div style='background-color:rgba(248, 58, 89, 0.15); color:", col_accent_red, "; padding:10px; border-radius:8px; font-weight:bold; margin-top:15px; border: 1px solid ", col_accent_red, ";'><i class='fa fa-exclamation-triangle'></i> Uncertainty Warning (1-q > ", round(m_res$q_conformal, 3), ")</div>")
    }
    
    html_content <- paste0(
      "<div class='modern-card' style='border-top: 4px solid ", col_accent_blue, ";'>",
      "<h3 style='margin-top:0;'>Diagnostic Final : <span style='color:", col_accent_blue, ";'>", diag_res$predicted_class, "</span></h3>",
      "<div style='padding: 15px 0; font-size: 1.8rem; font-weight: 800; color: ", col_accent_green, ";'>Probabilité retenue : ", round(diag_res$max_prob * 100, 1), "%</div>",
      conformal_status, "<hr><h4>Autres diagnostics :</h4>"
    )
    other_classes <- diag_res$probs[diag_res$probs$Classe != diag_res$predicted_class, ]
    for(i in 1:nrow(other_classes)) {
      html_content <- paste0(html_content, "<div class='prob-row'><span class='prob-class'>", other_classes$Classe[i], "</span><span class='prob-value' style='color:", col_text_muted, ";'>", round(other_classes$Probabilite[i] * 100, 1), "%</span></div>")
    }
    HTML(paste0(html_content, "</div>"))
  })
}
shinyApp(ui = ui, server = server)