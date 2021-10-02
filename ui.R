library(shiny)
library(heatmaply)
library(plotly)
library(networkD3)
library(visNetwork)


fluidPage(
  
  titlePanel("R-EDA1 (R Exploratory Data Analysis)"),
  p("conducted by Tetsuro Sugihara"),
  HTML('<div id="fb-root"></div><script async defer crossorigin="anonymous" src="https://connect.facebook.net/ja_JP/sdk.js#xfbml=1&version=v10.0" nonce="Y3i7onlm"></script>'),
  #HTML('<a href="https://twitter.com/share" class="twitter-share-button")>Twitter</a><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
  HTML('<a href="https://ecodata222.shinyapps.io/R-EDA1" class="twitter-share-button" data-show-count="false">Tweet</a><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
  HTML('<div class="fb-like" data-href="https://ecodata222.shinyapps.io/R-EDA1" data-width="" data-layout="button" data-action="like" data-size="small" data-share="true"></div>'),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file1", "Choose CSV File", multiple = TRUE,accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Variables_and_Categories1'",
        
        conditionalPanel(
          condition = "input.Similarity_of_Variables_and_Categories == 'Among_all_columns1'",
          
          conditionalPanel(
            condition = "input.Among_all_columns == 'Stratifeid_graph1'",
            
            selectInput("Gtype", "Graph type",  choices = c("histgram", "scatter", "box_plot", "line_graph", "bar", "3D-scatter")),
            conditionalPanel(
              condition = "input.Gtype == '3D-scatter'",
              numericInput('Xcol12', 'X axis', "1"),
              numericInput('Ycol12', 'Y axis', "1"),
              numericInput('Zcol12', 'Z axis', "1"),
              numericInput('Ccol12', 'Coloring (if "0" do not used colors)', "0"),
            ),
            
            
            conditionalPanel(
              condition = "input.Gtype != '3D-scatter'",
              
              numericInput('Lcol', 'Label', "1"),
              conditionalPanel(
                condition = "input.Gtype != 'histgram'",
                conditionalPanel(
                  condition = "input.Gtype != 'bar'",
                  conditionalPanel(
                    condition = "input.Gtype != 'line_graph'",
                    numericInput('Xcol', 'X axis', "1"),
                  ),
                ),
              ),
            
              
              
              numericInput('Scol', 'area separate 1 (if "0" not separate)', "0"),
              conditionalPanel(
                condition = "input.Gtype != 'scatter'",
                
                conditionalPanel(
                  condition = "input.Gtype != 'line_graph'",
                  conditionalPanel(
                    condition = "input.Scol > 0",
                    numericInput('Scol2', 'area separate 2 (if "0" not separate)', "0"),
                  ),
                ),
              ),
              conditionalPanel(
                condition = "input.Gtype == 'histgram'",
                conditionalPanel(
                  condition = "input.Scol == 0",
                  sliderInput("Prediction_Interval_Probability2",
                              "Prediction interval probability",
                              min = 0,  max = 1, value = 0.95, step = 0.01)
                ),
                conditionalPanel(
                  condition = "input.Scol != 0",
                  checkboxInput("Using_hypothesis_testing1", "Using hypothesis testing", FALSE),
                ),
              ),
              conditionalPanel(
                condition = "input.Gtype == 'box_plot'",
                
                conditionalPanel(
                  condition = "input.Xcol != 0",
                  checkboxInput("Using_hypothesis_testing2", "Using hypothesis testing", FALSE),
                ),
              ),
              conditionalPanel(
                condition = "input.Gtype == 'scatter' || input.Gtype == 'line_graph'",
                numericInput('Ccol', 'Coloring (if "0" do not used colors)', "0"),
                conditionalPanel(
                  condition = "input.Ccol > 0",
                  checkboxInput("NumericalToCategorcalSColor2", "If coloring varaiable is numerical, changing categorical varaiable", TRUE),
                  checkboxInput("NumericalToCategorcalSColor1", "If coloring is numerical, changing categorical coloring", FALSE),
                ),
                conditionalPanel(
                  condition = "input.Gtype == 'scatter'",
                  conditionalPanel(
                    condition = "input.Ccol == 0 && input.Scol == 0",
                    checkboxInput("Using_Prediction_Interval", "Using prediction interval", FALSE),
                  ),
                  conditionalPanel(
                    condition = "input.Using_Prediction_Interval == 1",
                    sliderInput("Prediction_Interval_Probability",
                                "Prediction interval probability",
                                min = 0,  max = 1, value = 0.95, step = 0.01)
                  ),
                  
                  checkboxInput("Using_GLM", "Using GLM", FALSE),
                  conditionalPanel(
                    condition = "input.Using_GLM == 1",
                    radioButtons("family_link2", "family_link",
                                 choices = c(gaussian_identity = "gaussian_identity",
                                             poisson_log = "poisson_log",
                                             binomial_logit = "binomial_logit",
                                             binomial_probit = "binomial_probit"),
                                 selected = "gaussian_identity"),
                    p("'gaussian_identity' = Simple mutli regression analysis"),
                    p("'poisson_log' = Regression analysis for count data of Y"),
                    p("'binomial_logit' = Logistic regression analysis")
                  ),
                  
                  conditionalPanel(
                    condition = "input.Ccol == 0 && input.Scol == 0",
                    checkboxInput("Check_difference_between_two_variablesl", "Check difference between two variables", FALSE),
                  ),
                ),
                
              )
            )
          ),
        )
      ),
      
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Variables_and_Categories1'",
        conditionalPanel(
          condition = "input.Similarity_of_Variables_and_Categories == 'Among_all_columns1'",
          conditionalPanel(
            condition = "input.Among_all_columns == 'Variable_Network1'",
            conditionalPanel(
              condition = "input.Variable_Network == 'Bayesian_Network1'",
              radioButtons("Structure_Learning", "Structure_Learning",
                           choices = c(stable_version  = "stable_version1",
                                       Grow_Shrink = "Grow_Shrink1",
                                       Incremental_Association_Markov_Blanket  = "Incremental_Association_Markov_Blanket1",
                                       Fast_Incremental_Association  = "Fast_Incremental_Association1",
                                       Interleaved_Incremental_Association = "Interleaved_Incremental_Association1",
                                       Incremental_Association_with_FDR_Correction = "Incremental_Association_with_FDR_Correction1",
                                       Max_Min_Parents_and_Children = "Max_Min_Parents_and_Children1",
                                       Semi_Interleaved_Hiton_PC = "Semi_Interleaved_Hiton_PC1",
                                       Hybrid_Parents_and_Children  = "Hybrid_Parents_and_Children1",
                                       Hill_Climbing  = "Hill_Climbing1",
                                       Tabu_Search = "Tabu_Search1",
                                       Max_Min_Hill_Climbing = "Max_Min_Hill_Climbing1",
                                       Hybrid_HPC = "Hybrid_HPC1",
                                       General_2_Phase_Restricted_Maximization = "General_2_Phase_Restricted_Maximization1",
                                       Chow_Liu = "Chow_Liu1",
                                       ARACNE = "ARACNE1"))
            ),
          ),
        ),
      ),
      
      
      
      
      
      
      
      
      radioButtons("analysis", "Analysis",
                   choices = c(Basic_EDA ="Basic_EDA1",
                                Similarity_of_Variables_and_Categories = "Similarity_of_Variables_and_Categories1",
                               Similarity_of_Samples = "Similarity_of_Samples1",
                               Similarity_of_Names_in_Rows_and_Columns = "Similarity_of_Names_in_Rows_and_Columns1"),
                   selected = "Basic_EDA1"),
      
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Variables_and_Categories1'",
        
        radioButtons("Similarity_of_Variables_and_Categories", "Method type",
                     choices = c(Among_all_columns = "Among_all_columns1",
                                 Between_label_column_and_others = "Between_label_column_and_others1"
                     )
        ),
        
        conditionalPanel(
          condition = "input.Similarity_of_Variables_and_Categories == 'Among_all_columns1'",
          radioButtons("Among_all_columns", "Method",
                       choices = c(Heat_map = "Heat_map1",
                                   Line_graph = "Line_graph1",
                                   Stratifeid_graph = "Stratifeid_graph1",
                                   Variable_Network = "Variable_Network1",
                                   Using_MDS = "Using_MDS1",
                                   Factor_Analysis = "Factor_Analysis1",
                                   Log_Linear = "Log_Linear1")
          ),
          
          conditionalPanel(
            condition = "input.Among_all_columns == 'Using_MDS1'",
            radioButtons("Using_MDS", "What_Using_MDS",
                         choices = c(PCA_MDS = "PCA_MDS1",
                                     Correspondence_MDS_Categories = "Correspondence_MDS_Categories1"))
          ),
          conditionalPanel(
            condition = "input.Among_all_columns == 'Heat_map1'",
            radioButtons("Normalization_use", "Normalization_use",
                         choices = c(No = "Heat_map_Raw1",
                                     Yes = "Heat_map_Noramalized1")),
            radioButtons("Change_categorical_data_into", "Change_categorical_data_into",
                         choices = c(Factor_Level = "Factor_Level1",
                                     Dummy_Varaiables = "Dummy_Varaiables1"))
          ),
          conditionalPanel(
            condition = "input.Among_all_columns == 'Line_graph1'",
            radioButtons("Line_graph", "Box_Type",
                         choices = c(Box_Integrated = "Box_Integrated1",
                                     Box_Separated = "Box_Separated1"))
          ),
          
          conditionalPanel(
            condition = "input.Among_all_columns == 'Variable_Network1'",
            radioButtons("Variable_Network", "Link Type",
                         choices = c(Correlation_Network = "Correlation_Network1",
                                     Graphical_Lasso = "Graphical_Lasso1",
                                     Cramer_Network = "Cramer_Network1",
                                     Bayesian_Network = "Bayesian_Network1",
                                     Association_rules = "Association1")),
                                     #LiNGAM = "LiNGAM1"
            
            
            conditionalPanel(
              condition = "input.Variable_Network == 'Correlation_Network1'",
              sliderInput("correlation_limit",
                          "Use absolute value of correlation coefficient more than",
                          min = 0,  max = 1, value = 0.9, step = 0.1)
            ),
            conditionalPanel(
              condition = "input.Variable_Network == 'Graphical_Lasso1'",
              sliderInput("inverse_covarianve_limit",
                          "Use absolute value of estimated inverse covariance more than",
                          min = 0,  max = 1, value = 0.9, step = 0.1),
              sliderInput("RHO",
                          "RHO (regularization)",
                          min = 0,  max = 1, value = 0.2, step = 0.01)
            ),
            conditionalPanel(
              condition = "input.Variable_Network == 'Cramer_Network1'",
              sliderInput("association_limit",
                          "Use absolute value of Cramer's coefficient of association more than",
                          min = 0,  max = 1, value = 0.9, step = 0.1)
            ),
            
            conditionalPanel(
              condition = "input.Variable_Network == 'Association1'",
              sliderInput("association_limit2",
                          "Choose set number for each evaluation (Confidense, Support and Lift)",
                          min = 0,  max = 100, value = 20, step = 1)
            )
          ),
          
          conditionalPanel(
            condition = "input.Among_all_columns == 'Factor_Analysis1'",
            numericInput('Factors', 'Number of factors', "2"),
            radioButtons("Factor_Rotation", "Rotation_Type",
                         choices = c(varimax = "varimax",
                                     quartimax = "quartimax",
                                     geominT = "geominT",
                                     promax = "promax",
                                     cluster = "cluster",
                                     oblimin = "oblimin",
                                     geominQ = "geominQ"))
          ),
          
        ),
        
        conditionalPanel(
          condition = "input.Similarity_of_Variables_and_Categories == 'Between_label_column_and_others1'",
          numericInput('Label_column', 'Label_column', "1"),
          radioButtons("Between_label_column_and_others", "Method",
                       choices = c(Scatter_plot = "Scatter_plot1",
                                   GLMM__Regression_analysis = "GLMM1",
                                   PCRA__Regression_analysis = "PCRA1",
                                   Decision_Tree = "Decision_Tree1",
                                   One_class_classification = "One_class1",
                                   Hidden = "Hidden1")
          ),
          
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'Decision_Tree1'",
            radioButtons("Decision_Tree", "Tree_Method",
                         choices = c(C5.0 = "C501",
                                     RandomForest = "RandomForest1"))
          ),
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'GLMM1'",
            radioButtons("family_link", "family_link",
                         choices = c(gaussian_identity = "gaussian_identity",
                                     poisson_log = "poisson_log",
                                     binomial_logit = "binomial_logit",
                                     binomial_probit = "binomial_probit"),
                         selected = "gaussian_identity"),
            p("'gaussian_identity' = Simple mutli regression analysis"),
            p("'poisson_log' = Regression analysis for count data of Y"),
            p("'binomial_logit' = Logistic regression analysis")
          ),
          
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'One_class1'",
            
            radioButtons("One_class", "Method",
                         choices = c(Basic_test_All_Varaiables = "Basic_test_All_Varaiables1",
                                     MT_All_Varaiables = "MT_All_Varaiables1",
                                     MT_Selected_Varaiables = "MT_Selected_Varaiables1",
                                     PCA_MT_All_Varaiables = "PCA_MT_All_Varaiables1",
                                     PCA_MT_Selected_Varaiables = "PCA_MT_Selected_Varaiables1",
                                     Kernel_PCA_MT = "Kernel_PCA_MT1",
                                     One_Class_SVM_All_Varaiables = "One_Class_SVM_All_Varaiables1",
                                     One_Class_SVM_Selected_Varaiables = "One_Class_SVM_Selected_Varaiables1",
                                     Minimum_Distance_All_Varaiables = "Minimum_Distance_All_Varaiables1",
                                     Minimum_Distance_Selected_Varaiables = "Minimum_Distance_Selected_Varaiables1"),
                         selected = "PCA_MT_All_Varaiables1"),
            
            conditionalPanel(
              condition = "input.One_class == 'Kernel_PCA_MT1'",
              sliderInput("kpar_value",
                          "kpar of Kernel_PCA",
                          min = 0,  max = 10, value = 0.1, step = 0.1),
              radioButtons("Kernel2", "Kernel",
                           choices = c(anovadot= "anovadot",
                                       rbfdot= "rbfdot",
                                       laplacedot= "laplacedot",
                                       besseldot= "besseldot"),
                           selected = "rbfdot")
              
            ),
            conditionalPanel(
              condition = "input.One_class == 'One_Class_SVM_Selected_Varaiables1'",
              sliderInput("nu2",
                          "nu (Adjust outliers)",
                          min = 0.0001,  max = 1, value = 0.1, step = 0.0001)
            ),
            conditionalPanel(
              condition = "input.One_class == 'One_Class_SVM_All_Varaiables1'",
              radioButtons("Kernel4", "Kernel_library",
                           choices = c(anovadot= "anovadot",
                                       rbfdot= "rbfdot",
                                       polydot= "polydot",
                                       vanilladot= "vanilladot",
                                       tanhdot= "tanhdot",
                                       laplacedot= "laplacedot",
                                       besseldot= "besseldot",
                                       splinedot= "splinedot"),
                           selected = "rbfdot"),
              sliderInput("nu4",
                          "nu (Adjust outliers)",
                          min = 0.0001,  max = 1, value = 0.1, step = 0.0001)
            )
          ),
          
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'Hidden1'",
            radioButtons("finder", "finder",
                         choices = c(Hidden_PCA = "Hidden_PCA1",
                                     Hidden_ICA = "Hidden_ICA1"))
          ),
        ),
      ),
      
      
      
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Samples1'",
        
        radioButtons("Dimension_for_clustering", "Dimension_for_clustering",
                     choices = c(Dimension_2 = "Dimension_2",
                                 Dimension_All = "Dimension_All"),
        ),
        
        
        conditionalPanel(
          condition = "input.Dimension_for_clustering == 'Dimension_2'",   
          radioButtons("Dimension_Reduction", "Dimension Reduction",
                       choices = c(None = "None1",
                                   Factor = "Factor1",
                                    MDS = "MDS1",
                                   tSNE = "MDS2",
                                   nMDS = "nMDS1"),
                       selected = "None1"),
          
          conditionalPanel(
            condition = "input.Dimension_Reduction == 'None1'",
            numericInput('Xcol11', 'X axis', "1"),
            numericInput('Ycol11', 'Y axis', "2"),
            numericInput('Scol11', 'area separate 2 (if "0" not separate)', "0"),
            conditionalPanel(
              condition = "input.Scol11 > '0'",
              p("If using methods for categorical variable, this tool changes numerical variable into categorical.
                    For example, if '5 is the input, numerical variable is divided into 5 ranges.
                    And names of the ranges are used as categories."),
              
              numericInput('NumericalToCategorcalS11', 'No of ranges', "3"),
            ),
          ),
          conditionalPanel(
            condition = "input.Dimension_Reduction ==  'Factor1'",
            numericInput('Xcol13', 'X axis', "1"),
            numericInput('Ycol13', 'Y axis', "2"),
            numericInput('Factors2', 'Number of factors', "2"),
            radioButtons("Factor_Rotation2", "Rotation_Type",
                         choices = c(varimax = "varimax",
                                     quartimax = "quartimax",
                                     geominT = "geominT",
                                     promax = "promax",
                                     cluster = "cluster",
                                     oblimin = "oblimin",
                                     geominQ = "geominQ"))
          ),
          
          
          conditionalPanel(
            condition = "input.Dimension_Reduction == 'MDS2'",
            sliderInput("perplexity_value",
                        "perplexity of t-SNE",
                        min = 1,  max = 1000, value = 1, step = 1)
          ),
        
          
          conditionalPanel(
            condition = "input.Dimension_Reduction != 'nMDS1'",
            conditionalPanel(
              condition = "input.AddClustering == 0",
              radioButtons("plot_type2", "Plot type",
                           choices = c(Name = "G11",
                                       Index = "G12",
                                       Plot_and_Name = "G13",
                                       Plot_and_Index = "G14",
                                       Only_Plot = "G15"),
                           selected = "G15")
            ),
            conditionalPanel(
              condition = "input.Dimension_Reduction != 'None1' && input.Dimension_Reduction != 'Facotr1'",
              checkboxInput("AddClustering", "Add clustering methods", FALSE),
              
            
              conditionalPanel(
                condition = "input.AddClustering == 1",
                  
                radioButtons("Clustering", "Clustering",
                             choices = c(k_Means = "clust3",
                                         GMM= "clust1",
                                         DBSCAN = "clust2",
                                         One_class_SVM_Clustering = "One_class_SVM_Clustering1"),
                             selected = "clust1"),
                conditionalPanel(
                  condition = "input.Clustering == 'clust3' || input.Clustering == 'clust1'",
                  numericInput('k', 'Number of Clusters', 2)
                ),
                
                conditionalPanel(
                  condition = "input.Clustering == 'clust2'",
                  sliderInput("eps_value",
                              "eps of DBSCAN",
                              min = 0,  max = 1, value = 0.1, step = 0.01)
                ),
                
                conditionalPanel(
                  condition = "input.Clustering == 'One_class_SVM_Clustering1'",
                  radioButtons("Kernel_library", "Kernel_library",
                               choices = c(anovadot_kernlab= "anovadot",
                                           rbfdot_kernlab= "rbfdot",
                                           polydot_kernlab= "polydot",
                                           vanilladot_kernlab= "vanilladot",
                                           tanhdot_kernlab= "tanhdot",
                                           laplacedot_kernlab= "laplacedot",
                                           besseldot_kernlab= "besseldot",
                                           splinedot_kernlab= "splinedot"),
                               selected = "rbfdot"),
                  sliderInput("nu1",
                              "nu (Adjust outliers)",
                              min = 0.001,  max = 1, value = 0.2, step = 0.001)
                ),
                
              
                radioButtons("plot_type", "Plot type",
                             choices = c(Name_and_Clusering= "G1",
                                         Index_and_Clusering1 = "G2",
                                         Index_and_Clusering2 = "G3",
                                         Only_Clustering = "G4"),
                             selected = "G4"),
                
              ),
            ),
          ),
          
          conditionalPanel(
            condition = "input.Dimension_Reduction == 'nMDS1'",
            sliderInput("Distance_to_use",
                        "Use absolute value of distance more than",
                        min = 0,  max = 10, value = 9, step = 0.1)
          ),
        ),
        
        conditionalPanel(
          condition = "input.Dimension_for_clustering == 'Dimension_All'",
           
          radioButtons("Method_Dimension_All", "Method",
                       choices = c(hclust = "hclust1",
                                   DBSCAN = "DBSCAN1"),
                       selected = "hclust1"),
        
        
        
          conditionalPanel(
            condition = "input.Method_Dimension_All == 'DBSCAN1'",  
            sliderInput("eps_value2",
                        "eps of DBSCAN",
                        min = 0,  max = 1, value = 0.1, step = 0.01)
          ),
        ),
      ),
      
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Names_in_Rows_and_Columns1'",
        
        radioButtons("Similarity_of_Names_in_Rows_and_Columns", "Method",
                     choices = c(Bipartite_graph = "Bipartite_graph1",
                                 Correspondence_MDS_Names = "Correspondence_MDS_Names1",
                                 Independence_Test = "Independence_Test1",
                                 Two_way_GLM = "Two_way_GLM1",
                                 Heat_map_Clustering = "Heat_map_Clustering1"),
                     selected = "Two_way_GLM1"),
        
        conditionalPanel(
          condition = "input.Similarity_of_Names_in_Rows_and_Columns == 'Bipartite_graph1'",
          radioButtons("Layout2", "Layout",
                       choices = c(layout_default = "layout_default1",
                                   layout_as_bipartite = "layout_as_bipartite1",
                                   layout_as_star = "layout_as_star1"),
                       selected = "layout_default1")
        ),
        conditionalPanel(
          condition = "input.Similarity_of_Names_in_Rows_and_Columns == 'Two_way_GLM1'",
          radioButtons("family_link3", "family_link",
                       choices = c(gaussian_identity = "gaussian_identity",
                                   poisson_log = "poisson_log"),
                       selected = "gaussian_identity"),
          p("'gaussian_identity' = Similar to 'two-way anova without interaction' and 'Quantification theory type I'"),
          p("'poisson_log' = Log_Linear_model. Y is count data")
        ),
      ),
      
      
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Variables_and_Categories1'",
        conditionalPanel(
          condition = "input.Similarity_of_Variables_and_Categories == 'Among_all_columns1'",
          
          conditionalPanel(
            condition = "input.Among_all_columns == 'Variable_Network1'",
          
            
            conditionalPanel(
              condition = "input.Variable_Network == 'Cramer_Network1'",
              numericInput('NumericalToCategorcalC', 'No of ranges', "3"),
              
            ),
            conditionalPanel(
              condition = "input.Variable_Network == 'Bayesian_Network1'",
              numericInput('NumericalToCategorcalB', 'No of ranges', "3"),
              p("If 0 is input, numerical variables are not changed into categorical variables.")
            ),
            conditionalPanel(
              condition = "input.Variable_Network == 'Association1'",
              numericInput('NumericalToCategorcalA', 'No of ranges', "3"),
            ),
          ),
          
          conditionalPanel(
            condition = "input.Among_all_columns == 'Stratifeid_graph1'",
            
            p("If using methods for categorical variable, this tool changes numerical variable into categorical.
                  For example, if '5 is the input, numerical variable is divided into 5 ranges.
                  And names of the ranges are used as categories."),
            
            numericInput('NumericalToCategorcalS', 'No of ranges', "3"),
            
          ),
          
          conditionalPanel(
            condition = "input.Among_all_columns == 'Using_MDS1'",
            
            p("If using methods for categorical variable, this tool changes numerical variable into categorical.
                  For example, if '5 is the input, numerical variable is divided into 5 ranges.
                  And names of the ranges are used as categories."),
            
            numericInput('NumericalToCategorcalU', 'No of ranges', "3"),
            
          ),
          conditionalPanel(
            condition = "input.Among_all_columns == 'Log_Linear1'",
            
            p("If using methods for categorical variable, this tool changes numerical variable into categorical.
                  For example, if '5 is the input, numerical variable is divided into 5 ranges.
                  And names of the ranges are used as categories."),
            
            numericInput('NumericalToCategorcalL', 'No of ranges', "3"),
            
          ),
            
        ),
        
        conditionalPanel(
          condition = "input.Similarity_of_Variables_and_Categories == 'Between_label_column_and_others1'",
         
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'One_class1'",
            p("If 'One_class' is chosed,"),
            p("label column of the CSV-data must be 0 or 1,"),
            p("0 labeled samples are used to make model (as the 'One class').")
          )
        )
      ),
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Samples1'",
        conditionalPanel(
          condition = "input.Dimension_for_clustering == 'Dimension_2'",
          p("If 'Dimension_2' is chosed,"),
          p("label column of the CSV-data must be the sample name or index No.")
        )
      ),
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Names_in_Rows_and_Columns1'",
        p("If 'Similarity_of_Names_in_Rows_and_Columns' is chosed,"),
        p("first column (left side) of the CSV-data must be names,"),
        p("This analysis uses names of first rows and first columns.")
      ),
      a("Guide (English)   ",href="http://data-science.tokyo/R-E/about_R-EDA1.html"),
      a(" (Japanese)   ",href="http://data-science.tokyo/R-J/about_R-EDA1.html"),
      
      selectInput("sep2", "Separator of CSV",  choices = c("Separator_Comma", "Separator_Semicolon", "Separator_Tab")),
      
      conditionalPanel(
        condition = "input.analysis != 'Similarity_of_Samples1'",
        checkboxInput("DoNotUseFirst", "Do not use the first column of CSV", FALSE),
      ),
      
    ),
    
    
    
    
    
    mainPanel(
      
      conditionalPanel(
        condition = "input.analysis == 'Basic_EDA1'",
        dataTableOutput("text00")
      ),
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Variables_and_Categories1'",
        conditionalPanel(
          condition = "input.Similarity_of_Variables_and_Categories == 'Among_all_columns1'",
          conditionalPanel(
            condition = "input.Among_all_columns == 'Heat_map1'",
            
            h3("Heat map"),
            p("Visualization of Data Table"),
            #plotOutput("plot03"),
            plotlyOutput("plot03"),
            
            a("Code (English)",href="http://data-science.tokyo/R-E/R-E1-01.html"),
            a(" (Japanese)",href="http://data-science.tokyo/R-J/R-J1-01.html"),br(),
            a("About Heat map(English)",href="http://data-science.tokyo/ed-e/ede1-4-3-2.html"),
            a("(Japanese)",href="http://data-science.tokyo/ed/edj1-4-3-2.html"),
            
          ),
          conditionalPanel(
            condition = "input.Among_all_columns == 'Line_graph1'",
            
            h3("Line graph of all variables"),
            plotlyOutput("plot01"),
            p("Categorical variables are changed into dummy variables"),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-01.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-01.html")
            
          ),
          
          conditionalPanel(
            condition = "input.Among_all_columns == 'Stratifeid_graph1'",
            h3("Stratifeid_graph"),
            #conditionalPanel(
            #  condition = "input.Scol == 0",
            #  checkboxInput("Using_interactive_graph1", "Using interactive graph", FALSE),
            #),
            conditionalPanel(
              condition = "input.Gtype == 'bar'",
              p("n of categories"),
            ),
            #conditionalPanel(
            #  condition = "input.Using_interactive_graph1 == 0",
            plotlyOutput("plot14"),
            #),
            #conditionalPanel(
            #  condition = "input.Using_interactive_graph1 == 1",
            #  plotlyOutput("plot538"),
            #),
            
            conditionalPanel(
              condition = "input.Gtype == 'scatter'",
              
              conditionalPanel(
                condition = "input.Scol == 0",
                conditionalPanel(
                  condition = "input.Ccol == 0",
                  h3("Check correlation"),
                  verbatimTextOutput("text518"),
                  conditionalPanel(
                    condition = "input.Using_GLM == 1",
                    p("Generalized linear model (GLM)"),
                    verbatimTextOutput("text519"),
                    verbatimTextOutput("text520"),
                  ),
                  conditionalPanel(
                    condition = "input.Check_difference_between_two_variablesl == 1",
                    h3("Check difference between two variables"),
                    plotOutput("plot537"),
                    verbatimTextOutput("text537"),
                  ),
                ),
              
                conditionalPanel(
                  condition = "input.Ccol != 0",
                  
                  conditionalPanel(
                    condition = "input.Scol == 0",
                    conditionalPanel(
                      condition = "input.Using_GLM == 1",
                      p("Linear mixed model (LMM)"),
                      verbatimTextOutput("text526"),
                      p("Generalized linear mixed model (GLMM) (if 'gaussian is chossed, same to LMM)"),
                      verbatimTextOutput("text527"),
                      verbatimTextOutput("text528"),
                    )
                  ),
                ),
              ),
              conditionalPanel(
                condition = "input.Scol != 0",
                conditionalPanel(
                  condition = "input.Ccol == 0",
                  conditionalPanel(
                    condition = "input.Using_GLM == 1",
                    p("Linear mixed model (LMM)"),
                    verbatimTextOutput("text529"),
                    p("Generalized linear mixed model (GLMM) (if 'gaussian is chossed, same to LMM)"),
                    verbatimTextOutput("text530"),
                    verbatimTextOutput("text531"),
                  )
                ),
                
                conditionalPanel(
                  condition = "input.Ccol != 0",
                  
                  conditionalPanel(
                    condition = "input.Scol != 0",
                    conditionalPanel(
                      condition = "input.Using_GLM == 1",
                      p("Linear mixed model (LMM)"),
                      verbatimTextOutput("text532"),
                      verbatimTextOutput("text535"),
                      p("Generalized linear mixed model (GLMM) (if 'gaussian is chossed, same to LMM)"),
                      verbatimTextOutput("text533"),
                      verbatimTextOutput("text534"),
                    )
                  ),
                ),
              ),
            ),
            conditionalPanel(
              condition = "input.Gtype == 'box_plot'",
              
              conditionalPanel(
                condition = "input.Xcol > 0",
                conditionalPanel(
                  condition = "input.Scol == 0",
                  h3("Check the diffence of center (mean)"),
                  verbatimTextOutput("text517"),
                  conditionalPanel(
                    condition = "input.Using_hypothesis_testing2 == 1",
                    p("by one-way ANOVA"),
                    verbatimTextOutput("text505"),
                    verbatimTextOutput("text506"),
                  ),
                  h3("Check the diffence of variation (variance)"),
                  verbatimTextOutput("text521"),
                  conditionalPanel(
                    condition = "input.Using_hypothesis_testing2 == 1",
                    p("by Bartlett's test"),
                    verbatimTextOutput("text507"),
                    verbatimTextOutput("text508"),
                  ),
                ),
                conditionalPanel(
                  condition = "input.Scol != 0",
                  conditionalPanel(
                    condition = "input.Scol2 == 0",
                    h3("Check the diffence of center (mean)"),
                    verbatimTextOutput("text522"),
                    conditionalPanel(
                      condition = "input.Using_hypothesis_testing2 == 1",
                      p("by two-way ANOVA with interaction"),
                      verbatimTextOutput("text513"),
                      verbatimTextOutput("text514"),
                      p("by two-way ANOVA without interaction"),
                      verbatimTextOutput("text515"),
                      verbatimTextOutput("text516"),
                    ),
                  ),
                  conditionalPanel(
                    condition = "input.Scol2 != 0",
                    h3("Check the diffence of center (mean)"),
                    verbatimTextOutput("text541"),
                    conditionalPanel(
                      condition = "input.Using_hypothesis_testing2 == 1",
                      p("by three-way ANOVA with interaction"),
                      verbatimTextOutput("text542"),
                      verbatimTextOutput("text543"),
                      p("by three-way ANOVA without interaction"),
                      verbatimTextOutput("text544"),
                      verbatimTextOutput("text545"),
                    ),
                  ),
                ),
              ),
            ),
            conditionalPanel(
              condition = "input.Gtype == 'histgram'",
              
              conditionalPanel(
                condition = "input.Scol != 0",
                conditionalPanel(
                  condition = "input.Scol2 == 0",
                  h3("Check the diffence of center (mean)"),
                  verbatimTextOutput("text523"),
                  conditionalPanel(
                    condition = "input.Using_hypothesis_testing1 == 1",
                    p("by one-way ANOVA"),
                    verbatimTextOutput("text501"),
                    verbatimTextOutput("text502"),
                  ),
                  h3("Check the diffence of variation (variance)"),
                  verbatimTextOutput("text524"),
                  conditionalPanel(
                    condition = "input.Using_hypothesis_testing1 == 1",
                    p("by Bartlett's test"),
                    verbatimTextOutput("text503"),
                    verbatimTextOutput("text504"),
                  ),
                ),
                conditionalPanel(
                  condition = "input.Scol2 != 0",
                  h3("Check the diffence of center (mean)"),
                  verbatimTextOutput("text525"),
                  conditionalPanel(
                    condition = "input.Using_hypothesis_testing1 == 1",
                    p("by two-way ANOVA with interaction"),
                    verbatimTextOutput("text509"),
                    verbatimTextOutput("text510"),
                    p("by two-way ANOVA without interaction"),
                    verbatimTextOutput("text511"),
                    verbatimTextOutput("text512"),
                  ),
                ),
              ),
              conditionalPanel(
                condition = "input.Scol == 0",
                conditionalPanel(
                  condition = "input.Scol2 == 0",
                  h3("Prediction interval"),
                  verbatimTextOutput("text546"),
                ),
              ),
            ),
            
            conditionalPanel(
              condition = "input.Gtype == 'bar'",
              conditionalPanel(
                condition = "input.Scol != 0",
                conditionalPanel(
                  condition = "input.Scol2 == 0",
                  h3("Independence Test"),
                  p("Independence between two categorical varaiables"),
                  verbatimTextOutput("text5362"),
                ),
                h3("Log linear model"),
                verbatimTextOutput("text536"),
              ),
            ),
            conditionalPanel(
              condition = "input.Gtype == '3D-scatter'",
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E5-02.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J5-02.html")
            ),
            conditionalPanel(
              condition = "input.Gtype != '3D-scatter'",
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E5-01.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J5-01.html")
            ),
          ),
          
          conditionalPanel(
            condition = "input.Among_all_columns == 'Variable_Network1'",
            
            conditionalPanel(
              condition = "input.Variable_Network == 'Correlation_Network1'",
              h3("Correlation Coefficient --> Network graph"),
              selectInput("network_library1", "Library of network",  choices = c("igraph", "networkD3")),
              conditionalPanel(
                condition = "input.network_library1 == 'igraph'",
                plotOutput("plot06"),
              ),
              conditionalPanel(
                condition = "input.network_library1 == 'networkD3'",
                simpleNetworkOutput("plot06b"),
              ),
              dataTableOutput("Data_Output2"),
              
              downloadButton("downloadData2", "Download analyzed data"),
              
              h4("Algorithm"),
              p("1. All categorical variables are changed into dummy variables"),
              p("2. Calculate correlation coefficient"),
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-02.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-02.html"),br(),
              a("About Correlation of variables (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3.html"),br(),
              a("About Correlation Coefficient Network graph (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3-1.html"),
              a("(Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3-1.html")
            ),
            
            conditionalPanel(
              condition = "input.Variable_Network == 'Graphical_Lasso1'",
              h3("Graphical Lasso"),
              selectInput("network_library2", "Library of network",  choices = c("igraph", "networkD3")),
              conditionalPanel(
                condition = "input.network_library2 == 'igraph'",
                plotOutput("plot07"),
              ),
              conditionalPanel(
                condition = "input.network_library2 == 'networkD3'",
                simpleNetworkOutput("plot07b"),
              ),
              dataTableOutput("Data_Output3"),
              downloadButton("downloadData3", "Download analyzed data"),
              h4("Algorithm"),
              p("1. All categorical variables are changed into dummy variables"),
              p("2. Graphical lasso"),
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-02.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-02.html"),br(),
              a("About Correlation of variables (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3.html"),br(),
              a("About Correlation Coefficient Network graph (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3-1-1.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3-1-1.html")
            ),
            
            conditionalPanel(
              condition = "input.Variable_Network == 'Cramer_Network1'",
              h3("Cramer's coefficient of association --> Network graph"),
              selectInput("network_library3", "Library of network",  choices = c("igraph", "networkD3")),
              conditionalPanel(
                condition = "input.network_library3 == 'igraph'",
                plotOutput("plot09"),
              ),
              conditionalPanel(
                condition = "input.network_library3 == 'networkD3'",
                simpleNetworkOutput("plot09b"),
              ),
              dataTableOutput("Data_Output4"),
              downloadButton("downloadData4", "Download analyzed data"),
              
              
              h4("Algorithm"),
              p("1. All numerical variables are changed into categorical variables"),
              p("2. Calculate Cramer's coefficient of association of all sets"),
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-02.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-02.html"),br(),
              a("About Correlation of categorical variables (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3.html"),
              a("(Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3.html")
            ),
            conditionalPanel(
              condition = "input.Variable_Network == 'Bayesian_Network1'",
              h3("Bayesian Network"),
              plotOutput("plot16"),
              
              a("About Structure Analysis by Bayesian Network (English)   ",href="http://data-science.tokyo/ed-e/ede1-8-3-2.html"),
              a("(Japanese)   ",href="http://data-science.tokyo/ed/edj1-8-3-2.html")
              
              
            ),
            conditionalPanel(
              condition = "input.Variable_Network == 'LiNGAM1'",
              h3("LiNGAM --> Network graph"),
              plotOutput("plot17"),
              dataTableOutput("Data_Output17"),
              
              downloadButton("downloadData17", "Download analyzed data"),
              
              h4("Algorithm"),
              p("1. All numerical variables are standardized"),
              p("2. Calculate LiNGAM"),
              p("3. Absolute values of coefficients are calculated"),
              p("4. Draw network graph"),
              p("In this method, we cannot use categorical variables"),
              a("Code (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3-1-2.html"),
              a("(Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3-1-2.html")
            ),
            
            conditionalPanel(
              condition = "input.Variable_Network == 'Association1'",
              h3("Associations Rules"),
              p("Similarity among Categories"),
              selectInput("network_library4", "Library of network",  choices = c("visNetwork", "igraph")),
              #selectInput("network_library4", "Library of network",  choices = c("igraph","visNetwork")),
              #selectInput("network_library4", "Library of network",  choices = c( "visNetwork")),
              #selectInput("network_library4", "Library of network",  choices = c( "igraph")),
              h4("Sets of high confidence"),
              #plotOutput("plot12"),
              conditionalPanel(
                condition = "input.network_library4 == 'igraph'",
                plotOutput("plotap32a"),
              ),
              conditionalPanel(
                condition = "input.network_library4 == 'visNetwork'",
                visNetworkOutput("plotap32b"),
              ),
              #plotOutput("plotap32"),
              dataTableOutput("ap221"),
              downloadButton("downloadData221", "Download analyzed data"),
              
              h4("Sets of high support"),
              conditionalPanel(
                condition = "input.network_library4 == 'igraph'",
                plotOutput("plotap31a"),
                
              ),
              conditionalPanel(
                condition = "input.network_library4 == 'visNetwork'",
                #simpleNetworkOutput("plotap31b"),
                visNetworkOutput("plotap31b"),
                h4("Graph of support is the unfirected network. But this graph is directed network because of bug"),
              ),
              #plotOutput("plotap31a"),
              dataTableOutput("ap211"),
              downloadButton("downloadData211", "Download analyzed data"),
              
              h4("Sets of high lift"),
              conditionalPanel(
                condition = "input.network_library4 == 'igraph'",
                plotOutput("plotap33a"),
              ),
              conditionalPanel(
                condition = "input.network_library4 == 'visNetwork'",
                #simpleNetworkOutput("plotap33b"),
                visNetworkOutput("plotap33b"),
              ),
              #plotOutput("plotap33"),
              dataTableOutput("ap231"),
              downloadButton("downloadData231", "Download analyzed data"),
              
              
              h4("Algorithm"),
              p("1. All numerical variables are changed into categorical variables"),
              p("2. All categorical variables are changed into dummy variables"),
              p("3. Association analysis."),
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-04.html"),
              a("(Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-04.html"),br(),
              a("About similariy of categories (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-2.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-2.html"),br(),
              a("About association analysis (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-2-1.html"),
              a("(Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-2-1.html")
            ),
          ),
          
          conditionalPanel(
            condition = "input.Among_all_columns == 'Using_MDS1'",
            conditionalPanel(
              condition = "input.Using_MDS == 'PCA_MDS1'",
              h3("PCA --> MDS"),
              plotlyOutput("plot08"),
              h4("Algorithm"),
              p("1. All categorical variables are changed into dummy variables"),
              p("2. PCA to calculate factor loading. Output is multi-dimensional data"),
              p("3. MDS(sammon) as dimension reduction. Output is 2-dimension data"),
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-02.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-02.html"),br(),
              a("About similariy of categories (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-2.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-2.html"),br(),
              a("About the reason to use MDS after PCA (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3-2.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3-2.html")
            ),
            
            conditionalPanel(
              condition = "input.Using_MDS == 'Correspondence_MDS_Categories1'",
              h3("Correspondence and MDS Analysis for Categories"),
              plotlyOutput("plot13"),
              
              h4("Contribution rate of eigenvalue"),
              textOutput("text131"),
              h4("No of dimensions used in the model"),
              p("If contribution rate of eigenvalue is over 0.01, the dimension is used."),
              textOutput("text132"),
              h4("Algorithm"),
              p("1. All numerical variables are changed into categorical variables"),
              p("2. All categorical variables are changed into dummy variables"),
              p("3. Correspondence analysis. Output is multi-dimensional data"),
              p("4. MDS(sammon) as dimension reduction. Output is 2-dimension data"),
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-04.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-04.html"),br(),
              a("About similariy of categories (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-2.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-2.html"),br(),
              a("About the reason to use MDS after Correspondence analysis (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-4-2-4.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-4-2-4.html")
            ),
          ),
          
          conditionalPanel(
            condition = "input.Among_all_columns == 'Factor_Analysis1'",
            h3("Factor analysis"),
            verbatimTextOutput("text406"),
            plotlyOutput("plot406"),
            
            
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E4-09.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J4-09.html"),br(),
            a("About Factor analysis (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-4.html"),br(),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-4.html"),br(),
          ),
            
          conditionalPanel(
            condition = "input.Among_all_columns == 'Log_Linear1'",
            h3("Log Linear model"),
            verbatimTextOutput("text404"),
            
            h4("Algorithm"),
            p("1. All numerical variables are changed into categorical variables"),
            p("2. Make contingency table. (= Make count data of categories)"),
            p("3. GLM. Y is count data. Xs are categories"),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-02.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-02.html"),br(),
            a("About similariy of categories (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-2.html"),br(),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-2.html"),br(),
            a("About Log Linear model (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3-3-2.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3-3-2.html")
          ),
          
        ),  
        conditionalPanel(
          condition = "input.Similarity_of_Variables_and_Categories == 'Between_label_column_and_others1'",
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'Scatter_plot1'",
            h3("Scatter plot (label column vs Others)"),
            p("Similarity among Variables"),
            plotlyOutput("plot05"),
            p("Categorical data is changed into dummy variables"),
            p("If label column is numerical, 2 dimension scatter plot. 
              If label column is categorical, 1 dimension scatter plot stratidied by categories."),
            p("Categorical variables except label column are changed into dummy variables"),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-02.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-02.html")
          ),
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'GLMM1'",
            h3("Generalized Linear Mixed Model (Regression Analysis)"),
            p("Features of interaction (product of two variables) are included. 
              So this method could be Linear Mixed Effect model (LME) when categorical variables are included."),
            p("Features in the model are automatically choosed using AIC."),
            verbatimTextOutput("text113"),
            a("About GLMM (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-1-4.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-1-4.html")
          ),
          
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'PCRA1'",
            h3("Principal Component Regression Analysis"),
            
            verbatimTextOutput("text114"),
            plotOutput("plot18"),
            a("About PCRA (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-1-2-1-1.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-1-2-1-1.html")
          ),
          
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'Decision_Tree1'",
            h3("Decision Tree"),
            plotOutput("plot15"),
            a("About Decision_Tree (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-1.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-1.html")
          ),
          
          
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'One_class1'",
            conditionalPanel(
              condition = "input.One_class != 'Basic_test_All_Varaiables1'",
              h3("One class classification to evaluate outliers"),
            ),
            conditionalPanel(
              condition = "input.One_class == 'Basic_test_All_Varaiables1'",
              h3("Check differences between label'0' and label'1'"),
            ),
            plotlyOutput("plot301"),
            conditionalPanel(
              condition = "input.One_class == 'Basic_test_All_Varaiables1'",
              plotOutput("plot601"),
              plotOutput("plot602"),
              plotOutput("plot603"),
            ),
            conditionalPanel(
              condition = "input.One_class == 'PCA_MT_All_Varaiables1'",
              plotlyOutput("plotPCAMT"),
              h3("Factor loading (Correlation coefficient between variables and PCs)"),
              dataTableOutput("Data_OutputPCAMT"),
            ),
            conditionalPanel(
              condition = "input.One_class == 'MT_Selected_Varaiables1'",
              dataTableOutput("Data_OutputMTselected"),
            ),
            conditionalPanel(
              condition = "input.One_class == 'PCA_MT_Selected_Varaiables1'",
              dataTableOutput("Data_OutputPCAMTselected"),
            ),
            conditionalPanel(
              condition = "input.One_class == 'One_Class_SVM_Selected_Varaiables1'",
              dataTableOutput("Data_OutputOneClassSVMselected"),
            ),
            conditionalPanel(
              condition = "input.One_class == 'Minimum_Distance_Selected_Varaiables1'",
              dataTableOutput("Data_OutputMDselected"),
            ),
            conditionalPanel(
              condition = "input.One_class == 'MT_All_Varaiables1'",
              p("If there is multicollinearity among variables, MT does not work.")
            ),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E3-01.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J3-01.html"),br(),
            a("About one-class model (English)   ",href="http://data-science.tokyo/ed-e/ede1-6-4-2.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-6-4-2.html"),
            
            conditionalPanel(
              condition = "input.One_class != 'Minimum_Distance_All_Varaiables1'",
              
              conditionalPanel(
                condition = "input.One_class != 'Minimum_Distance_Selected_Varaiables1'",
                conditionalPanel(
                  condition = "input.One_class != 'Basic_test_All_Varaiables1'",
                  h4("Algorithm"),
                  p("1. All categorical variables are changed into dummy variables (except MT)"),
                  p("2. Make model with the samples of label = 0 in the label column"),
                  p("3. Calculate distance from the average of label = 0 samples"),
                  a("About MT method (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-2-4.html"),
                  a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-2-4.html"),br(),
                  conditionalPanel(
                    condition = "input.One_class != 'PCA_MT1'",
                    a("About the reason to use PCA before MT method (English)   ",href="http://data-science.tokyo/ed/edj1-2-2-4-3-2.html"),
                    a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-2-4-3-2.html"),br(),
                  ),
                ),
              ),
            ),
            conditionalPanel(
              condition = "input.One_class == 'Minimum_Distance_All_Varaiables1'",
              h4("Algorithm"),
              p("1. All categorical variables are changed into dummy variables (except MT)"),
              p("2. PCA for dimension reduction"),
              p("3. Calculate distance from the nearest sample of label = 0 samples"),
              a("About Minimum Distance method (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-5-3.html"),
              a("(Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-5-3.html"),
            ),
          ),
          
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'Hidden1'",
            conditionalPanel(
              condition = "input.finder == 'Hidden_PCA1'",
              h3("Label column vs (Other variables and Principal components)"),
              
              plotlyOutput("plot10"),
              
              h4("Algorithm"),
              p("1. All categorical variables except label column are changed into dummy variables"),
              p("2. Principal  components are calculated except the label column variable"),
              p("to find hidden factor."),
              br(),
              p("If label column is numerical, 2 dimension scatter plot. 
                If label column is categorical, 1 dimension scatter plot stratidied by categories."),
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-05.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-05.html"),br(),
              a("About hidden variable (English)   ",href="http://data-science.tokyo/ed-e/ede1-9-1-2-1-1.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-9-1-2-1-1.html")
            ),
            conditionalPanel(
              condition = "input.finder == 'Hidden_ICA1'",
              h3("Label column vs (Other variables and Independent components)"),
              plotlyOutput("plot11"),
              h4("Algorithm"),
              p("1. All categorical variables except label column are changed into dummy variables"),
              p("2. Independent components are calculated except the label column variable"),
              p("to find hidden factor."),
              br(),
              p("If label column is numerical, 2 dimension scatter plot. 
                If label column is categorical, 1 dimension scatter plot stratidied by categories."),
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-05.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-05.html"),br(),
              a("Abput hidden variable (English)   ",href="http://data-science.tokyo/ed-e/ede1-9-1-2-1-1.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-9-1-2-1-1.html"),br(),
              a("Abput ICA (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-4-2.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-4-2.html")
            ),
          ),
        ),
      ),
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Samples1'",
        conditionalPanel(
          condition = "input.Dimension_for_clustering == 'Dimension_2'",   
          h3("Similarity of samples"),
          conditionalPanel(
            condition = "input.Dimension_Reduction == 'None1'",
            plotlyOutput("plot204"),
          ),
          conditionalPanel(
            condition = "input.Dimension_Reduction == 'Factor1'",
            h3("Factor analysis"),
            verbatimTextOutput("text407"),
            plotlyOutput("plot407"),
            plotlyOutput("plot205"),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E4-09.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J4-09.html"),br(),
            a("About Factor analysis(English)   ",href="http://data-science.tokyo/ed-e/ede1-2-4.html"),
            a("(Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-4.html"),br(),
          ),
          conditionalPanel(
            condition = "input.Dimension_Reduction != 'nMDS1'",
            conditionalPanel(
              condition = "input.Dimension_Reduction != 'None1'",
              plotlyOutput("plot201"),
            ),
          ),
          conditionalPanel(
            condition = "input.Dimension_Reduction == 'nMDS1'",
            selectInput("network_library5", "Library of network",  choices = c("igraph", "networkD3")),
            conditionalPanel(
              condition = "input.network_library5 == 'igraph'",
              plotOutput("plot203"),
            ),
            conditionalPanel(
              condition = "input.network_library5 == 'networkD3'",
              simpleNetworkOutput("plot203b"),
            ),
            #plotOutput("plot203"),
          ),
          conditionalPanel(
            condition = "input.Dimension_Reduction != 'None1'",
            p("Axis1 and axis2 do not have physical meaning"),
            p("Change from High dimension data into 2 dimension data."),
          ),
        
        
          conditionalPanel(
            condition = "input.Dimension_Reduction != 'nMDS1'",
            conditionalPanel(
              condition = "input.AddClustering == 1",
              p("Then find clusters for the 2 dimension data."),
              conditionalPanel(
                condition = "input.Clustering == 'clust2'",
                p("If DBSCAN is used, clust name 0 is the samples judged as outliers")
              ),
              p("Download analyzed data for the next step.
              For example, if clust column is put on the first (left side) column,
              we can analyze with the function 'Similarity_of_Variables_and_Categories'"),
              downloadButton("downloadData", "Download analyzed data")
            ),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-03.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-03.html"),br(),
            a("About dimension reduction(English)   ",href="http://data-science.tokyo/ed-e/ede1-3-3-1.html"),
            a("(Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-3-1.html"),br(),
            
          ),
          conditionalPanel(
            condition = "input.Dimension_Reduction == 'nMDS1'",
            p("Change from High dimension data into 2 dimension data using network graph algrithm. "),
            
            a("About nMDS (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-3-1-1-1.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-3-1-1-1.html"),br(),
          )
        )
      ),
        
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Samples1'",
        
        conditionalPanel(
          condition = "input.Dimension_for_clustering == 'Dimension_All'", 
          h3("Similarity of samples"),
          plotlyOutput("plot202"),   
          p("Categorical variables are changed into dummy variables"),
          p("Numerical variables are normalized after dummy changed"),
          p("Clust name 0 is the samples judged as outliers"),
          conditionalPanel(
            condition = "input.Method_Dimension_All == 'dbscan1'", 
            p("Download analyzed data for the next step.
                For example, if clust column is put on the first (left side) column,
                we can analyze with the function 'Similarity_of_Variables_and_Categories'"),
            downloadButton("downloadData5", "Download analyzed data")
          ),
          
          a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E4-03.html"),
          a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J4-03.html"),br(),
        ),
        
        a("About similariy of samples (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-3-1.html"),
        a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-3-1.html"),br(),
        a("About clustering (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-3-2.html"),
        a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-3-2.html"),
      ),
      
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Names_in_Rows_and_Columns1'",
        
        h3("Similarity of Names in Rows and Columns"),
        conditionalPanel(
          condition = "input.Similarity_of_Names_in_Rows_and_Columns == 'Bipartite_graph1'",
          p("Analysis of count data (0, 1, 2, 3,,,,), Not use for negative values and categorical values"),
          p("Values are used as thickness of lines. Small values are not used as lines."),
          plotOutput("plot401"),
          
          a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-06.html"),
          a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-06.html")
        ),
        conditionalPanel(
          condition = "input.Similarity_of_Names_in_Rows_and_Columns == 'Correspondence_MDS_Names1'",
          p("Analysis of count data (0, 1, 2, 3,,,,), Not use for negative values and categorical values"),
          plotlyOutput("plot405"),
          h4("Contribution rate of eigenvalue"),
          textOutput("text4051"),
          h4("No of dimensions used in the model"),
          p("If contribution rate of eigenvalue is over 0.01, the dimension is used."),
          textOutput("text4052"),
          h4("Algorithm"),
          p("1. Correspondence analysis. Output is multi-dimensional data."),
          p("2. MDS(sammon) to change from high dimension into 2 dimension"),
          a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-06.html"),
          a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-06.html"),br(),
          a("About the reason to use MDS after Correspondence analysis (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-4-2-4.html"),
          a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-4-2-4.html")
        ),
        conditionalPanel(
          condition = "input.Similarity_of_Names_in_Rows_and_Columns == 'Independence_Test1'",
          p("Analysis of count data (0, 1, 2, 3,,,,), Not use for negative values and categorical values"),
          
          verbatimTextOutput("text403"),
          
          a("About Two_way_GLM (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3-3-1.html"),
          a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3-3-1.html")
        ),
        conditionalPanel(
          condition = "input.Similarity_of_Names_in_Rows_and_Columns == 'Two_way_GLM1'",
          verbatimTextOutput("text402"),
          
          
          conditionalPanel(
            condition = "input.family_likn3 == 'gaussian_identity'",
            p("Values are used as Label(Y) of the model.
            Names are used as features(X)."),
            
            a("About Quantification theory type I (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-6-1.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-6-1.html")
          ),
          conditionalPanel(
            condition = "input.family_likn3 != 'gaussian_identity'",
            p("Analysis of count data (0, 1, 2, 3,,,,), Not use for negative values and categorical values"),
            p("Values are used as Label(Y) of the model.
            Names are used as features(X)."),
            p("Count values are used as Label(Y) of the model.
            Names are used as features(X)."),
            
            a("About Loglinear_model (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3-3-2.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3-3-2.html")
          ),
          a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-06.html"),
          a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-06.html")
        ),
        conditionalPanel(
          condition = "input.Similarity_of_Names_in_Rows_and_Columns == 'Heat_map_Clustering1'",
          plotlyOutput("plot403"),
          
          a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-06.html"),
          a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-06.html")
        ),
        
      )
      
      
    )
  )
)