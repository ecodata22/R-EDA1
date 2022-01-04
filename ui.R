library(shiny)
library(heatmaply)
library(plotly)
library(networkD3)
library(visNetwork)

fluidPage(
  
  titlePanel("R-EDA1 (R Exploratory Data Analysis)"),
  p("made by Tetsuro Sugihara"),
  HTML('<div id="fb-root"></div><script async defer crossorigin="anonymous" src="https://connect.facebook.net/ja_JP/sdk.js#xfbml=1&version=v10.0" nonce="Y3i7onlm"></script>'),
  #HTML('<a href="https://twitter.com/share" class="twitter-share-button")>Twitter</a><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
  HTML('<a href="https://ecodata222.shinyapps.io/R-EDA1" class="twitter-share-button" data-show-count="false">Tweet</a><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
  HTML('<div class="fb-like" data-href="https://ecodata222.shinyapps.io/R-EDA1" data-width="" data-layout="button" data-action="like" data-size="small" data-share="true"></div>'),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file1", "Choose CSV File", multiple = TRUE,accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
      
      conditionalPanel(
        condition = "input.analysis == 'Heat_map1'",
        checkboxInput("Use_one_column_as_sample_name1", "Use one of the column as sample name", FALSE),
        
        conditionalPanel(
          condition = "input.Use_one_column_as_sample_name1 == 1",
          numericInput('sample_row1', 'Column number for  sample name', "1"),
        ),
        conditionalPanel(
          condition = "input.Use_one_column_as_sample_name1 == 0",
          p("Index is used as sample name.")
        ),
      ),
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Samples1'",
        
        checkboxInput("Use_one_column_as_sample_name2", "Use one of the column as sample name", FALSE),
        
        conditionalPanel(
          condition = "input.Use_one_column_as_sample_name2 == 1",
          numericInput('sample_row2', 'Column number for  sample name', "1"),
        ),
        
        conditionalPanel(
          condition = "input.Use_one_column_as_sample_name2 == 0",
          p("Index is used as sample name.")
        ),
      ),
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Names_in_Rows_and_Columns1'",
        
        checkboxInput("Use_one_column_as_sample_name3", "Use one of the column as sample name", FALSE),
        
        conditionalPanel(
          condition = "input.Use_one_column_as_sample_name3 == 1",
          numericInput('sample_row3', 'Column number for  sample name', "1"),
        ),
        
        conditionalPanel(
          condition = "input.Use_one_column_as_sample_name3 == 0",
          p("Index is used as sample name.")
        ),
      ),
      
      
      conditionalPanel(
        condition = "input.analysis != 'Similarity_of_Samples1'",
        
        conditionalPanel(
          condition = "input.analysis != 'Similarity_of_Names_in_Rows_and_Columns1'",
          conditionalPanel(
            condition = "input.analysis != 'Heat_map1'",
            checkboxInput("DoNotUseFirst", "Do not use the first column of CSV", FALSE),
          ),
        ),
      ),
      
      
      selectInput("analysis", "Analysis",
                   choices = c(Basic_EDA ="Basic_EDA1",
                               Heat_map = "Heat_map1",
                                Similarity_of_Variables_and_Categories = "Similarity_of_Variables_and_Categories1",
                               Similarity_of_Samples = "Similarity_of_Samples1",
                               Many_vs_many = "Similarity_of_Names_in_Rows_and_Columns1",
                               Time_series = "Time_series1"),
                   selected = "Basic_EDA1"),
      
      conditionalPanel(
        condition = "input.analysis == 'Heat_map1'",
        

        selectInput("Change_categorical_data_into", "Change categorical data into",
                     choices = c(Factor_Level = "Factor_Level1",
                                 Dummy_Varaiables = "Dummy_Varaiables1")),
        #checkboxInput("Normalization_use", "Use normalization", FALSE),
        
        selectInput("Use_scale_transformation1", "Scaling",
                    choices = c(None = "None1",
                                Normalization = "Normalization1",
                                Standardization = "Standardization1")),
        checkboxInput("AddClusteringCol", "Add clustering methods for columns", FALSE),
        checkboxInput("AddClusteringRow", "Add clustering methods for rows", FALSE),
        
        
        conditionalPanel(
          condition = "input.AddClusteringRow == 0",
            checkboxInput("Use_decreasing1", "Use decreasing", FALSE),
          
          conditionalPanel(
            condition = "input.Use_decreasing1 == 1",
              numericInput('Row_number_decreasing1', 'Column number for decreasing', "1"),
          ),
        ),
      ),
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Variables_and_Categories1'",
        
        selectInput("Similarity_of_Variables_and_Categories", "Method type",
                     choices = c(Among_all_columns = "Among_all_columns1",
                                 Between_label_column_and_others = "Between_label_column_and_others1"
                     )
        ),
        
        conditionalPanel(
          condition = "input.Similarity_of_Variables_and_Categories == 'Among_all_columns1'",
          selectInput("Among_all_columns", "Method",
                       choices = c(Stratifeid_graph = "Stratifeid_graph1",
                                   Variable_Network = "Variable_Network1",
                                   Using_Dimension_reduction = "Using_MDS1",
                                   Log_Linear = "Log_Linear1")
          ),
          conditionalPanel(
            condition = "input.Among_all_columns == 'Stratifeid_graph1'",
            
            selectInput("Gtype", "Graph type",  choices = c("histgram", "scatter", "box_plot", "bar", "3D-scatter")),
            conditionalPanel(
              condition = "input.Gtype == '3D-scatter'",
              numericInput('Xcol12', 'Column number for X axis', "1"),
              numericInput('Ycol12', 'Column number for Y axis', "1"),
              numericInput('Zcol12', 'Column number for Z axis', "1"),
              numericInput('Ccol12', 'Column number for coloring (if "0" do not used colors)', "0"),
            ),
            
            
            conditionalPanel(
              condition = "input.Gtype != '3D-scatter'",
              
              numericInput('Lcol', 'Column number for main variable', "1"),
              conditionalPanel(
                condition = "input.Gtype != 'histgram'",
                conditionalPanel(
                  condition = "input.Gtype != 'bar'",
                  conditionalPanel(
                    condition = "input.Gtype != 'line_graph'",
                    numericInput('Xcol', 'Column number for X axis', "1"),
                  ),
                ),
              ),
              
              
              
              numericInput('Scol', 'Column number for area separate 1 (if "0" not separate)', "0"),
              conditionalPanel(
                condition = "input.Gtype != 'scatter'",
                
                conditionalPanel(
                  condition = "input.Gtype != 'line_graph'",
                  conditionalPanel(
                    condition = "input.Scol > 0",
                    numericInput('Scol2', 'Column number for area separate 2 (if "0" not separate)', "0"),
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
                numericInput('Ccol', 'Column number for coloring (if "0" do not used colors)', "0"),
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
                    selectInput("family_link2", "family_link",
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
          
          conditionalPanel(
            condition = "input.Among_all_columns == 'Using_MDS1'",
            selectInput("Using_MDS", "Analysis_of_variables",
                         choices = c(PCA = "PCA_MDS1",
                                     Factor_Analysis = "Factor_Analysis1",
                                     Correspondence_Analysis = "Correspondence_MDS_Categories1")),
            conditionalPanel(
              condition = "input.Using_MDS == 'Factor_Analysis1'",
              numericInput('Factors', 'Number of factors', "2"),
              selectInput("Factor_Rotation", "Rotation_Type",
                           choices = c(varimax = "varimax",
                                       quartimax = "quartimax",
                                       geominT = "geominT",
                                       promax = "promax",
                                       cluster = "cluster",
                                       oblimin = "oblimin",
                                       geominQ = "geominQ"))
            ),
            
            selectInput("Dimension_reduction6", "Change into 2 dimension data",
                         choices = c(MDS = "MDS6",
                                     tSNE = "tSNE6",
                                     UMAP = "UMAP6")),
            conditionalPanel(
              condition = "input.Dimension_reduction6 == 'tSNE6'",
              numericInput('perplexity_value6', 'perplexity of t-SNE', "1"),
              #sliderInput("perplexity_value6",
              #            "perplexity of t-SNE",
              #            min = 1,  max = 100, value = 1, step = 1)
            ),
          ),
          conditionalPanel(
            condition = "input.Among_all_columns == 'Line_graph1'",
            selectInput("Line_graph", "Box_Type",
                         choices = c(Box_Integrated = "Box_Integrated1",
                                     Box_Separated = "Box_Separated1"))
          ),
          
          conditionalPanel(
            condition = "input.Among_all_columns == 'Variable_Network1'",
            selectInput("Variable_Network", "Link Type",
                         choices = c(Correlation_Network = "Correlation_Network1",
                                     Graphical_Lasso = "Graphical_Lasso1",
                                     Cramer_Network = "Cramer_Network1",
                                     Bayesian_Network = "Bayesian_Network1",
                                     Association_rules = "Association1")),
                                     #LiNGAM = "LiNGAM1")),
            
            conditionalPanel(
              condition = "input.Variable_Network != 'Bayesian_Network1'",
              conditionalPanel(
                condition = "input.Variable_Network != 'Association1'",
                selectInput("Graph_type1", "Graph type",
                            choices = c(scatter_plot = "scatter_plot1",
                                        network = "network1")),
              ),
            ),
            conditionalPanel(
              condition = "input.Graph_type1 == 'network1'",
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
              ),
              conditionalPanel(
                condition = "input.Variable_Network == 'Cramer_Network1'",
                sliderInput("association_limit",
                            "Use absolute value of Cramer's coefficient of association more than",
                            min = 0,  max = 1, value = 0.9, step = 0.1)
              ),
            ),
            
            conditionalPanel(
              condition = "input.Variable_Network == 'Graphical_Lasso1'",
              sliderInput("RHO",
                          "RHO (regularization)",
                          min = 0,  max = 1, value = 0.2, step = 0.01)
            ),
            
            conditionalPanel(
              condition = "input.Variable_Network == 'Association1'",
              sliderInput("association_limit2",
                          "Choose set number for each evaluation (Confidense, Support and Lift)",
                          min = 0,  max = 100, value = 20, step = 1)
            ),
            conditionalPanel(
              condition = "input.Variable_Network == 'Bayesian_Network1'",
              selectInput("Structure_Learning", "Structure_Learning",
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
        
        
            
        conditionalPanel(
          condition = "input.Similarity_of_Variables_and_Categories == 'Between_label_column_and_others1'",
          numericInput('Label_column', 'Column number for  label', "1"),
          
            
          conditionalPanel(
            condition = "input.Between_label_column_and_others != 'Regression_analysis1'",
            
            conditionalPanel(
              condition = "input.Between_label_column_and_others != 'One_class1'",
              checkboxInput("Change_class2", "Change classes into numeric to category", FALSE),
              
              conditionalPanel(
                condition = "input.Change_class2 == 1",
                selectInput("Variable_for2", "Variable for changing categorical data",
                            choices = c(Only_label = "Only_label2",
                                        All_variables = "All_variables2")),
              
                selectInput("Na_analysis2", "2 categories, NA and numeric",
                            choices = c(NA_or_numeric = "NA_or_numeric2",
                                        More_than_2 = "More_than_2_2")),
                
                conditionalPanel(
                  condition = "input.Na_analysis2 == 'More_than_2_2'",
                  numericInput('NumericalToCategorcalD', 'No of ranges', "3"),
                ),
              ),
            ),
            
            conditionalPanel(
              condition = "input.Between_label_column_and_others == 'One_class1'",
              checkboxInput("Change_NA1", "Change NA into 1 and others into 0", FALSE),
              conditionalPanel(
                condition = "input.Change_NA1 == 1",
                selectInput("Variable_for1", "Variable for changing 0/1 data",
                            choices = c(Only_label = "Only_label1",
                                        All_variables = "All_variables1")),
              ),
            ),
          ),
          selectInput("Between_label_column_and_others", "Method",
                       choices = c(Scatter_plot = "Hidden1",
                                   Regression_analysis = "Regression_analysis1",
                                   Decision_Tree = "Decision_Tree1",
                                   One_class_classification = "One_class1")
          ),
          
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'Hidden1'",
            selectInput("finder", "finder",
                        choices = c(None = "Hidden_None1",
                                    With_PCA = "Hidden_PCA1",
                                    With_ICA = "Hidden_ICA1"))
          ),
          
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'Regression_analysis1'",
            selectInput("Regression_analysis", "Regression analysis Method",
                        choices = c(GLMM = "GLMM1",
                                    PCRA = "PCRA1")),
            conditionalPanel(
              condition = "input.Regression_analysis == 'GLMM1'",
              checkboxInput("Use_squared_model1", "Use squared model", FALSE),
              selectInput("family_link", "family_link",
                          choices = c(gaussian_identity = "gaussian_identity",
                                      poisson_log = "poisson_log",
                                      binomial_logit = "binomial_logit",
                                      binomial_probit = "binomial_probit"),
                          selected = "gaussian_identity"),
              p("'gaussian_identity' = Simple mutli regression analysis"),
              p("'poisson_log' = Regression analysis for count data of Y"),
              p("'binomial_logit' = Logistic regression analysis")
            ),
          ),
          
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'Decision_Tree1'",
            selectInput("Decision_Tree", "Tree Method",
                         choices = c(C5.0 = "C501",
                                     RandomForest = "RandomForest1",
                                     C5.0_based_RandomForest = "C50_based_RandomForest1")),
            conditionalPanel(
              condition = "input.Decision_Tree == 'C50_based_RandomForest1'",
              checkboxInput("Use_sampling_variables", "Random sampling of variables (rows)", TRUE),
              checkboxInput("Use_sampling_samples", "Random sampling of samples (columns)", TRUE),
            ),
            conditionalPanel(
              condition = "input.Decision_Tree != 'RandomForest1'",
              checkboxInput("Use_minCases", "Use minimum size of splits", TRUE),
              conditionalPanel(
                condition = "input.Use_minCases == 1",
                sliderInput("Ratio_of_columns",
                            "Ratio of the number of minimum size",
                            min = 0,  max = 0.3, value = 0.1, step = 0.001),
              ),
            ),
          ),
          
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'One_class1'",
            
            selectInput("Variables_type", "Variables type",
                        choices = c(All_Varaiables = "All_Varaiables1",
                                    Selected_Varaiables = "Selected_Varaiables1")
                                    ),
            
            
            conditionalPanel(
              condition = "input.Variables_type == 'All_Varaiables1'",
              selectInput("One_class11", "Method",
                           choices = c(Basic_test = "Basic_test_All_Varaiables1",
                                       MT = "MT_All_Varaiables1",
                                       PCA_MT = "PCA_MT_All_Varaiables1",
                                       Kernel_PCA_MT = "Kernel_PCA_MT1",
                                       One_Class_SVM = "One_Class_SVM_All_Varaiables1",
                                       One_Class_Minimum_Distance = "Minimum_Distance_All_Varaiables1")),
              
              conditionalPanel(
                condition = "input.One_class11 == 'Kernel_PCA_MT1'",
                sliderInput("kpar_value",
                            "kpar of Kernel_PCA",
                            min = 0,  max = 2, value = 0.01, step = 0.01),
                selectInput("Kernel2", "Kernel",
                            choices = c(anovadot= "anovadot",
                                        rbfdot= "rbfdot",
                                        laplacedot= "laplacedot",
                                        besseldot= "besseldot"),
                            selected = "rbfdot")
                
              ),
              
              conditionalPanel(
                condition = "input.One_class11 == 'One_Class_SVM_All_Varaiables1'",
                selectInput("Kernel4", "Kernel_library",
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
              ),
              
              conditionalPanel(
                condition = "input.One_class11 == 'Minimum_Distance_All_Varaiables1'",
                selectInput("Dimension_reduction_type6", "Component type",
                            choices = c(None = "None6",
                                        PCA = "PCA6"),
                ),
                selectInput("Use_scale_transformation6", "Scaling",
                            choices = c(None = "None6",
                                        Normalization = "Normalization6",
                                        Standardization = "Standardization6")),
              ),
            ),
              
            
            conditionalPanel(
              condition = "input.Variables_type == 'Selected_Varaiables1'",
              selectInput("One_class12", "Method",
                          choices = c(MT = "MT_Selected_Varaiables1",
                                      One_Class_SVM = "One_Class_SVM_Selected_Varaiables1",
                                      One_Class_Minimum_Distance = "Minimum_Distance_Selected_Varaiables1")
                          ),
              
              conditionalPanel(
                condition = "input.One_class12 == 'One_Class_SVM_Selected_Varaiables1'",
                sliderInput("nu2",
                            "nu (Adjust outliers)",
                            min = 0.0001,  max = 1, value = 0.1, step = 0.0001)
              ),
              conditionalPanel(
                condition = "input.One_class12 == 'Minimum_Distance_Selected_Varaiables1'",
                selectInput("Dimension_reduction_type5", "Component type",
                            choices = c(None = "None5",
                                        PCA = "PCA3"),
                ),
                selectInput("Use_scale_transformation5", "Scaling",
                            choices = c(None = "None5",
                                        Normalization = "Normalization5",
                                        Standardization = "Standardization5")),
              ),
              
              
              
            ),
          ),
          
        ),
      ),
      
      
      
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Samples1'",
        
        selectInput("Dimension_for_clustering", "Dimension for analysis",
                     choices = c(Dimension_2 = "Dimension_2",
                                 Dimension_All = "Dimension_All"),
        ),
        
        
        conditionalPanel(
          condition = "input.Dimension_for_clustering == 'Dimension_2'",   
          selectInput("Dimension_Reduction", "Change into 2 dimension data",
                       choices = c(None = "None1",
                                    MDS = "MDS1",
                                   tSNE = "MDS2",
                                   UMAP = "UMAP1",
                                   NetworkMDS = "nMDS1"),
                       selected = "None1"),
          
          conditionalPanel(
            condition = "input.Dimension_Reduction != 'None1'",
            #checkboxInput("PCA_use2", "Use PCA", TRUE),
            selectInput("Dimension_reduction_type7", "Component type",
                        choices = c(None = "None7",
                                    PCA = "PCA7"),
            ),
            #checkboxInput("Normalization_use2", "Use normalization", TRUE),
            selectInput("Use_scale_transformation2", "Scaling",
                        choices = c(None = "None2",
                                    Normalization = "Normalization2",
                                    Standardization = "Standardization2")),
            
          ),
          
          conditionalPanel(
            condition = "input.Dimension_Reduction == 'None1'",
            numericInput('Xcol11', 'Column number for X axis', "1"),
            numericInput('Ycol11', 'Column number for Y axis', "2"),
            numericInput('Scol11', 'Column number for area separate (if "0" not separate)', "0"),
            conditionalPanel(
              condition = "input.Scol11 > '0'",
              p("If using methods for categorical variable, this tool changes numerical variable into categorical.
                    For example, if '5 is the input, numerical variable is divided into 5 ranges.
                    And names of the ranges are used as categories."),
              
              numericInput('NumericalToCategorcalS11', 'No of ranges', "3"),
            ),
          ),
          
          
          conditionalPanel(
            condition = "input.Dimension_Reduction == 'MDS2'",
            numericInput('perplexity_value', 'perplexity of t-SNE', "1"),
            #sliderInput("perplexity_value",
            #            "perplexity of t-SNE",
            #            min = 1,  max = 1000, value = 1, step = 1)
          ),
        
          
          conditionalPanel(
            condition = "input.Dimension_Reduction != 'nMDS1'",
            conditionalPanel(
              condition = "input.AddClustering == 0",
              selectInput("plot_type2", "Plot type",
                           choices = c(Name = "G11",
                                       Index = "G12",
                                       Plot_and_Name = "G13",
                                       Plot_and_Index = "G14",
                                       Only_Plot = "G15"),
                           selected = "G15")
            ),
            conditionalPanel(
              condition = "input.Dimension_Reduction != 'None1'",
              checkboxInput("AddClustering", "Add clustering methods", FALSE),
              
            
              conditionalPanel(
                condition = "input.AddClustering == 1",
                  
                selectInput("Clustering", "Clustering",
                             choices = c(k_Means = "clust3",
                                         GMM= "clust1",
                                         DBSCAN = "clust2",
                                         HDBSCAN = "HDBSCAN1",
                                         One_class_SVM_Clustering = "One_class_SVM_Clustering1")
                            ),
                
                conditionalPanel(
                  condition = "input.Clustering == 'clust3' || input.Clustering == 'clust1'",
                  numericInput('k', 'Number of Clusters', 2)
                ),
                
                conditionalPanel(
                  condition = "input.Clustering == 'clust2'",
                  numericInput('eps_value', 'eps of DBSCAN', 0.1),
                  #sliderInput("eps_value",
                  #            "eps of DBSCAN",
                  #            min = 0,  max = 1, value = 0.1, step = 0.01)
                ),
                conditionalPanel(
                  condition = "input.Clustering == 'HDBSCAN1'",
                  numericInput('minPts1', 'minPts of HDBSCAN', 10),
                ),
                
                conditionalPanel(
                  condition = "input.Clustering == 'One_class_SVM_Clustering1'",
                  selectInput("Kernel_library", "Kernel_library",
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
                
              
                selectInput("plot_type", "Plot type",
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
          #checkboxInput("PCA_use4", "Use PCA", TRUE),
          selectInput("Dimension_reduction_type8", "Component type",
                      choices = c(None = "None8",
                                  PCA = "PCA8"),
          ),
          #checkboxInput("Normalization_use4", "Use normalization", TRUE),
          selectInput("Use_scale_transformation3", "Scaling",
                      choices = c(None = "None3",
                                  Normalization = "Normalization3",
                                  Standardization = "Standardization3")),
          #selectInput("Pre_processting4", "Pre processting",
          #            choices = c(PCA = "PCA4",
          #                        Normalization = "Normalization_use4",
          #                        None = "None4")),
          selectInput("Method_Dimension_All", "Method",
                       choices = c(Hierarchical = "hclust1",
                                   DBSCAN = "DBSCAN1",
                                   HDBSCAN = "HDBSCAN2"),
                       selected = "hclust1"),
            conditionalPanel(
              condition = "input.Method_Dimension_All == 'hclust1'", 
              selectInput("hclust_type1", "Type",
                          choices = c(ward = "ward.D2",
                                      single = "single")),
              numericInput('k2', 'Number of Clusters', 2)
            ),
        
        
          conditionalPanel(
            condition = "input.Method_Dimension_All == 'DBSCAN1'",  
            numericInput('eps_value2', 'eps of DBSCAN', 0.1),
            #sliderInput("eps_value2",
            #            "eps of DBSCAN",
            #            min = 0,  max = 1, value = 0.1, step = 0.01)
          ),
          
          conditionalPanel(
            condition = "input.Method_Dimension_All == 'HDBSCAN2'",
            numericInput('minPts2', 'minPts of HDBSCAN', 10),
          ),
        ),
      ),
      
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Names_in_Rows_and_Columns1'",
        
        selectInput("Matrix_type", "Matrix type",
                     choices = c(A_B = "A_B",
                                 A_A = "A_A")),
        
        a("About 'Matrix type' (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-4.html"),
        a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-4.html"),
        conditionalPanel(
          condition = "input.Matrix_type == 'A_B'",
          selectInput("A_B_method", "Method",
                       choices = c(Using_other_variables = "Using_other_variables1",
                                   Bipartite_graph = "Bipartite_graph1",
                                   Independence_Test = "Independence_Test1",
                                   Two_way_GLM = "Two_way_GLM1")),
          
          conditionalPanel(
            condition = "input.A_B_method == 'Bipartite_graph1'",
            
            selectInput("Layout2", "Layout",
                         choices = c(layout_default = "layout_default1",
                                     layout_as_bipartite = "layout_as_bipartite1",
                                     layout_as_star = "layout_as_star1"),
                         selected = "layout_default1"),
            sliderInput("Minimum_value_of_width_of_edge1",
                        "Minimum value of width of edge",
                        min = 0,  max = 5, value = 2, step = 0.1)
            
            
          ),
          conditionalPanel(
            condition = "input.A_B_method == 'Using_other_variables1'",
            selectInput("Make_variables", "Make variables",
                         choices = c(SVD = "SVD5",
                                     Correspondence = "Correspondence5",
                                     Factor = "Factor1")),
            
            conditionalPanel(
              condition = "input.Make_variables ==  'Factor1'",
              numericInput('Xcol13', 'Column number for X axis', "1"),
              numericInput('Ycol13', 'Column number for Y axis', "2"),
              numericInput('Factors2', 'Number of factors', "2"),
              selectInput("Factor_Rotation2", "Rotation_Type",
                          choices = c(varimax = "varimax",
                                      quartimax = "quartimax",
                                      geominT = "geominT",
                                      promax = "promax",
                                      cluster = "cluster",
                                      oblimin = "oblimin",
                                      geominQ = "geominQ")),
              
              selectInput("Dimension_reduction7", "Change into 2 dimension data for analysis of variables",
                          choices = c(MDS = "MDS7",
                                      tSNE = "tSNE7",
                                      UMAP = "UMAP7")),
              conditionalPanel(
                condition = "input.Dimension_reduction7 == 'tSNE7'",
                numericInput('perplexity_value7', 'perplexity of t-SNE', "1"),
                #sliderInput("perplexity_value7",
                #            "perplexity of t-SNE",
                #            min = 1,  max = 1000, value = 1, step = 1)
              ),
              
              selectInput("plot_type3", "Plot type for analysis of samples",
                          choices = c(Name = "G111",
                                      Index = "G121",
                                      Plot_and_Name = "G131",
                                      Plot_and_Index = "G141",
                                      Only_Plot = "G151"))
              
            ),
            conditionalPanel(
              condition = "input.Make_variables !=  'Factor1'",
              selectInput("Dimension_reduction5", "Change into 2 dimension data",
                           choices = c(MDS = "MDS5",
                                       tSNE = "tSNE5",
                                       UMAP = "UMAP5")),
              conditionalPanel(
                condition = "input.Dimension_reduction5 == 'tSNE5'",
                numericInput('perplexity_value5', 'perplexity of t-SNE', "1"),
                #sliderInput("perplexity_value5",
                #            "perplexity of t-SNE",
                #            min = 1,  max = 1000, value = 1, step = 1)
              ),
            ),
          ),
          conditionalPanel(
            condition = "input.A_B_method == 'Two_way_GLM1'",
            selectInput("family_link3", "family_link",
                         choices = c(gaussian_identity = "gaussian_identity",
                                     poisson_log = "poisson_log"),
                         selected = "gaussian_identity"),
            p("'gaussian_identity' = Similar to 'two-way anova without interaction' and 'Quantification theory type I'"),
            p("'poisson_log' = Log_Linear_model. Y is count data")
          ),
        ),
        
        conditionalPanel(
          condition = "input.Matrix_type == 'A_A'",
          selectInput("A_A_method", "Method",
                       choices = c(Monopartite_graph = "Monopartite_graph1",
                                   MDS_sammon = "MDS_sammon1",
                                   Eigen_value = "Eigen_value1")),
          
          conditionalPanel(
            condition = "input.A_A_method == 'Monopartite_graph1'",
            checkboxInput("Use_direction1", "Use direction", FALSE),
            checkboxInput("Change_to_Largecolse", "Change large=far data into large=close data", FALSE),
            sliderInput("Minimum_value_of_width_of_edge2",
                        "Minimum value of width of edge",
                        min = 0,  max = 5, value = 2, step = 0.1)
          ),
          conditionalPanel(
            condition = "input.A_A_method == 'MDS_sammon1'",
            checkboxInput("Change_to_Largefar", "Change large=close data into large=far data", FALSE),
          ),
          
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
            conditionalPanel(
              condition = "input.Using_MDS == 'Correspondence_MDS_Categories1'",
            
              p("This tool changes numerical variables into categorical.
                    For example, if '5 is the input, numerical variable is divided into 5 ranges.
                    And names of the ranges are used as categories."),
              
              numericInput('NumericalToCategorcalU', 'No of ranges', "3"),
            ),
            conditionalPanel(
              condition = "input.Using_MDS != 'Correspondence_MDS_Categories1'",
              
              p("Tool changes categorical variables into numerical (dummy variables)."),
              
            ),
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
        condition = "input.analysis == 'Time_series1'",
        
        selectInput("Dimension_type", "Dimension_type",
                     choices = c(Multi_variable = "Multi_variable",
                                 One_variable = "One_variable"
                     )
        ),
        conditionalPanel(
          condition = "input.Dimension_type == 'One_variable'",
          selectInput("Method4", "Method",
                       choices = c(Difference_previous = "Difference_previous",
                                   Auto_correlation = "Auto_correlation",
                                   fft = "fft",
                                   Quasi_periodic = "Quasi_periodic"
                       ),
          ),
          numericInput('Value_to_analyze', 'Value to analyze', "1"),
          conditionalPanel(
            condition = "input.Method4 == 'Difference_previous'",
            numericInput('Lag_of_diff', 'Lag_of_diff', "1"),
            p("If 1 is selected, diffence from 1 previous value is calculated."),
          ),
          
          conditionalPanel(
            condition = "input.Method4 == 'Quasi_periodic'",
            selectInput("Using_01variable_in_table", "Using_01variable_in_table",
                         choices = c(Yes = "Using_01variable_in_table_Y",
                                     No = "Using_01variable_in_table_N"
                         )
            ),
            conditionalPanel(
              condition = "input.Using_01variable_in_table == 'Using_01variable_in_table_Y'",
  
              numericInput('Column_of_01variable', 'Column of 01variable', "1"),
            ),
            conditionalPanel(
              condition = "input.Using_01variable_in_table == 'Using_01variable_in_table_N'",
              
              numericInput('Column_to_divide01', 'Column to divide 0 or 1', "1"),
              p("If 1 is selected, value in 1st column is used to divide 0 or 1."),
              numericInput('Value_to_0or1', 'Value to divide 0 or 1', "0"),
              p("If 30 is selected, value over 30 is 1. Value under 30 is 0."),
              
            ),
          ),
        ),
        
        conditionalPanel(
          condition = "input.Dimension_type == 'Multi_variable'",
          selectInput("Method5", "Method",
                       choices = c(Component_analysis = "Dimension_reduction3",
                                   Stratifeid_graph = "Stratifeid_graph3",
                                   Cross_correlation = "Cross_correlation")
          ),
          
            
          conditionalPanel(
            condition = "input.Method5 == 'Dimension_reduction3'",
            selectInput("Line_graph2", "Box_Type",
                         choices = c(Box_Integrated = "Box_Integrated1",
                                     Box_Separated = "Box_Separated1")
            ),
            selectInput("Dimension_reduction_type3", "Component type",
                         choices = c(None = "None3",
                                     PCA = "PCA3",
                                     ICA = "ICA3",
                                     Factor = "Factor3"),
            ),
            selectInput("Use_scale_transformation4", "Scaling",
                        choices = c(None = "None4",
                                    Normalization = "Normalization4",
                                    Standardization = "Standardization4")),
            conditionalPanel(
              condition = "input.Dimension_reduction_type3 ==  'Factor3'",
              numericInput('Factors3', 'Number of factors', "2"),
              selectInput("Factor_Rotation3", "Rotation_Type",
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
            condition = "input.Method5 == 'Stratifeid_graph3'",
            
            numericInput('Lcol3', 'Column number for main variable', "1"),
            numericInput('Ccol3', 'Column number for coloring (if "0" do not used colors)', "0"),
            numericInput('Scol3', 'Column number for area separate (if "0" not separate)', "0"),
            conditionalPanel(
              condition = "input.Ccol3 > 0",
              checkboxInput("NumericalToCategorcalSColor23", "If coloring varaiable is numerical, changing categorical varaiable", TRUE),
              checkboxInput("NumericalToCategorcalSColor13", "If coloring is numerical, changing categorical coloring", FALSE),
            ),
            numericInput('NumericalToCategorcalS3', 'No of ranges', "3"),
          ),
          conditionalPanel(
            condition = "input.Method5 == 'Cross_correlation'",
            
            numericInput('Xcol4', 'Value_to_analyze_X', "1"),
            numericInput('Ycol4', 'Value_to_analyze_Y', "1"),
          ),
        ),
      ),
      
      a("Guide (English)   ",href="http://data-science.tokyo/R-E/about_R-EDA1.html"),
      a(" (Japanese)   ",href="http://data-science.tokyo/R-J/about_R-EDA1.html"),
      
      selectInput("sep2", "Separator of CSV",  choices = c("Separator_Comma", "Separator_Semicolon", "Separator_Tab")),

      
    ),
    
    
    
    
    
    mainPanel(
      
      conditionalPanel(
        condition = "input.analysis == 'Basic_EDA1'",
        dataTableOutput("text00")
      ),
      conditionalPanel(
        condition = "input.analysis == 'Heat_map1'",
        
        h3("Heat map"),
        p("Visualization of Data Table"),
        #plotOutput("plot03"),
        plotlyOutput("plot03"),
        
        a("Code (English)",href="http://data-science.tokyo/R-E/R-E5-05.html"),
        a(" (Japanese)",href="http://data-science.tokyo/R-J/R-J5-05.html"),br(),
        a("About Heat map(English)",href="http://data-science.tokyo/ed-e/ede1-4-3-2.html"),
        a("(Japanese)",href="http://data-science.tokyo/ed/edj1-4-3-2.html"),
        
      ),
      
      conditionalPanel(
        condition = "input.analysis == 'Similarity_of_Variables_and_Categories1'",
        conditionalPanel(
          condition = "input.Similarity_of_Variables_and_Categories == 'Among_all_columns1'",
          
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
                    plotlyOutput("plot519"),
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
                      #p("Linear mixed model (LMM)"),
                      #verbatimTextOutput("text526"),
                      p("Generalized linear mixed model (GLMM)"),
                      plotlyOutput("plot527"),
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
                    #p("Linear mixed model (LMM)"),
                    #verbatimTextOutput("text529"),
                    p("Generalized linear mixed model (GLMM)"),
                    plotlyOutput("plot530"),
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
                      #p("Linear mixed model (LMM)"),
                      #verbatimTextOutput("text532"),
                      #verbatimTextOutput("text535"),
                      p("Generalized linear mixed model (GLMM)"),
                      plotlyOutput("plot533"),
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
              h3("Correlation coefficient analysis"),
              conditionalPanel(
                condition = "input.Graph_type1 == 'scatter_plot1'",
                
                plotlyOutput("plot06c"),
              ),
              conditionalPanel(
                condition = "input.Graph_type1 != 'scatter_plot1'",
                selectInput("network_library1", "Library of network",  choices = c("igraph", "networkD3")),
                conditionalPanel(
                  condition = "input.network_library1 == 'igraph'",
                  plotOutput("plot06"),
                ),
                conditionalPanel(
                  condition = "input.network_library1 == 'networkD3'",
                  simpleNetworkOutput("plot06b"),
                ),
              ),
              dataTableOutput("Data_Output2"),
              
              downloadButton("downloadData2", "Download analyzed data"),
              
              h4("Algorithm"),
              p("1. All categorical variables are changed into dummy variables"),
              p("2. Calculate correlation coefficient"),
              p("3. Calculate positions using correlation coefficient as distance"),
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-02.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-02.html"),br(),
              a("About Correlation of variables (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3.html"),br(),
              a("About Correlation Coefficient Network graph (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3-1.html"),
              a("(Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3-1.html")
            ),
            
            conditionalPanel(
              condition = "input.Variable_Network == 'Graphical_Lasso1'",
              h3("Graphical Lasso analysis"),
              conditionalPanel(
                condition = "input.Graph_type1 == 'scatter_plot1'",
                
                plotlyOutput("plot07c"),
              ),
              conditionalPanel(
                condition = "input.Graph_type1 != 'scatter_plot1'",
                selectInput("network_library2", "Library of network",  choices = c("igraph", "networkD3")),
                conditionalPanel(
                  condition = "input.network_library2 == 'igraph'",
                  plotOutput("plot07"),
                ),
                conditionalPanel(
                  condition = "input.network_library2 == 'networkD3'",
                  simpleNetworkOutput("plot07b"),
                ),
              ),
              dataTableOutput("Data_Output3"),
              downloadButton("downloadData3", "Download analyzed data"),
              h4("Algorithm"),
              p("1. All categorical variables are changed into dummy variables"),
              p("2. Calculate graphical lasso"),
              p("3. Calculate positions using graphical lasso as distance"),
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-02.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-02.html"),br(),
              a("About Correlation of variables (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3.html"),br(),
              a("About Correlation Coefficient Network graph (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3-1-1.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3-1-1.html")
            ),
            
            conditionalPanel(
              condition = "input.Variable_Network == 'Cramer_Network1'",
              h3("Cramer's coefficient of association analysis"),
              conditionalPanel(
                condition = "input.Graph_type1 == 'scatter_plot1'",
                
                plotlyOutput("plot09c"),
              ),
              conditionalPanel(
                condition = "input.Graph_type1 != 'scatter_plot1'",
                selectInput("network_library3", "Library of network",  choices = c("igraph", "networkD3")),
                conditionalPanel(
                  condition = "input.network_library3 == 'igraph'",
                  plotOutput("plot09"),
                ),
                conditionalPanel(
                  condition = "input.network_library3 == 'networkD3'",
                  simpleNetworkOutput("plot09b"),
                ),
              ),
              dataTableOutput("Data_Output4"),
              downloadButton("downloadData4", "Download analyzed data"),
              
              
              h4("Algorithm"),
              p("1. All numerical variables are changed into categorical variables"),
              p("2. Calculate Cramer's coefficient of association of all sets"),
              p("3. Calculate positions using Cramer's coefficient as distance"),
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
              h3("PCA and dimension reduction"),
              
              verbatimTextOutput("text410"),
              plotlyOutput("plot08"),
              h4("Algorithm"),
              p("1. All categorical variables are changed into dummy variables"),
              p("2. PCA to calculate factor loading. Output is multi-dimensional data"),
              p("3. MDS(sammon) or t-SNE as dimension reduction. Output is 2-dimension data"),
              p("* PCA is not used as the method of dimension reduction. In this tool, dimension reduction is MDS and t-SNE"),
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
              #verbatimTextOutput("text409"),
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
              p("4. MDS(sammon) or t-SNE as dimension reduction. Output is 2-dimension data"),
              p("* Correspondence analysis is not used as the method of dimension reduction. In this tool, dimension reduction is MDS and t-SNE"),
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-04.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-04.html"),br(),
              a("About similariy of categories (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-2.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-2.html"),br(),
              a("About the reason to use MDS after Correspondence analysis (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-4-2-4.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-4-2-4.html")
            ),
            conditionalPanel(
              condition = "input.Using_MDS == 'Factor_Analysis1'",
              h3("Factor analysis"),
              verbatimTextOutput("text406"),
              plotlyOutput("plot406"),
              plotlyOutput("plot408"),
              
              
              h4("Algorithm"),
              p("1. All categorical variables are changed into dummy variables"),
              p("2. Factor analysis to calculate factor loading. Output is multi-dimensional data"),
              p("3. MDS(sammon) or t-SNE as dimension reduction. Output is 2-dimension data"),
              p("* Factor analysis is not used as the method of dimension reduction. In this tool, dimension reduction is MDS and t-SNE"),
              
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E4-09.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J4-09.html"),br(),
              a("About Factor analysis (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-4.html"),br(),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-4.html"),br(),
            ),
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
            condition = "input.Between_label_column_and_others == 'Hidden1'",
            conditionalPanel(
              condition = "input.finder == 'Hidden_None1'",
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
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'Regression_analysis1'",
              conditionalPanel(
                condition = "input.Regression_analysis == 'GLMM1'",
              h3("Generalized Linear Mixed Model (Regression Analysis)"),
              p("Features in the model are automatically choosed using AIC."),
              plotlyOutput("plot113"),
              verbatimTextOutput("text113"),
              a("About GLMM (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-1-4.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-1-4.html")
            ),
            
            conditionalPanel(
              condition = "input.Regression_analysis == 'PCRA1'",
              h3("Principal Component Regression Analysis"),
              
              verbatimTextOutput("text114"),
              plotlyOutput("plot114"),
              plotOutput("plot18"),
              a("About PCRA (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-1-2-1-1.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-1-2-1-1.html")
            ),
          ),
          
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'Decision_Tree1'",
            h3("Decision Tree"),
            
            conditionalPanel(
              condition = "input.Decision_Tree == 'C50_based_RandomForest1'",
              plotOutput("plotDT01"),br(),
              plotOutput("plotDT02"),br(),
              plotOutput("plotDT03"),br(),
              plotOutput("plotDT04"),br(),
              plotOutput("plotDT05"),br(),
            ),
            conditionalPanel(
              condition = "input.Decision_Tree != 'C50_based_RandomForest1'",
              plotOutput("plot15"),
            ),
            
            a("About Decision_Tree (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-1.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-1.html"),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E4-02.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J4-02.html")
          ),
          
          
          conditionalPanel(
            condition = "input.Between_label_column_and_others == 'One_class1'",
            
            plotlyOutput("plot301"),
            conditionalPanel(
              condition = "input.Variables_type == 'All_Varaiables1'",
              conditionalPanel(
                condition = "input.One_class11 == 'Basic_test_All_Varaiables1'",
                h3("Check differences between label'0' and label'1'"),
              ),
              conditionalPanel(
                condition = "input.One_class11 == 'Basic_test_All_Varaiables1'",
                plotOutput("plot601"),
                plotOutput("plot602"),
                plotOutput("plot603"),
              ),
              conditionalPanel(
                condition = "input.One_class11 == 'MT_All_Varaiables1'",
                p("If there is multicollinearity among variables, MT does not work."),
                h4("Algorithm"),
                p("1. All categorical variables are changed into dummy variables"),
                p("2. Make model with the samples of label = 0 in the label column"),
                p("3. Calculate distance from the average of label = 0 samples"),
                a("About MT method (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-2-4.html"),
                a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-2-4.html"),br(),
              ),
              conditionalPanel(
                condition = "input.One_class11 == 'PCA_MT_All_Varaiables1'",
                plotlyOutput("plotPCAMT"),
                h3("Factor loading (Correlation coefficient between variables and PCs)"),
                dataTableOutput("Data_OutputPCAMT"),
                a("About Principal Component MT (English)   ",href="http://data-science.tokyo/ed/edj1-2-2-4-3-2.html"),
                a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-2-4-3-2.html"),br(),
              ),
              conditionalPanel(
                condition = "input.One_class11 == 'Kernel_PCA_MT1'",
                a("About Kernel Principal Component MT (English)   ",href="http://data-science.tokyo/ed/edj1-2-2-4-3-3.html"),
                a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-2-4-3-3.html"),br(),
              ),
              conditionalPanel(
                condition = "input.One_class11 == 'One_Class_SVM_All_Varaiables1'",
                a("About One-Class SVM (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-2-3-2.html"),
                a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-2-3-2.html"),br(),
              ),
              conditionalPanel(
                condition = "input.One_class11 == 'Minimum_Distance_All_Varaiables1'",
                h4("Algorithm"),
                p("1. All categorical variables are changed into dummy variables (except MT)"),
                p("2. PCA for dimension reduction"),
                p("3. Calculate distance from the nearest sample of label = 0 samples"),
                a("About One Class Minimum Distance method (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-5-3.html"),
                a("(Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-5-3.html"),
              ),
            ),
            conditionalPanel(
              condition = "input.Variables_type != 'All_Varaiables1'",
              conditionalPanel(
                condition = "input.One_class12 == 'MT_Selected_Varaiables1'",
                dataTableOutput("Data_OutputMTselected"),
              ),
              conditionalPanel(
                condition = "input.One_class12 == 'PCA_MT_Selected_Varaiables1'",
                dataTableOutput("Data_OutputPCAMTselected"),
              ),
              conditionalPanel(
                condition = "input.One_class12 == 'One_Class_SVM_Selected_Varaiables1'",
                dataTableOutput("Data_OutputOneClassSVMselected"),
              ),
              conditionalPanel(
                condition = "input.One_class12 == 'Minimum_Distance_Selected_Varaiables1'",
                dataTableOutput("Data_OutputMDselected"),
              ),
              
            ),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E3-01.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J3-01.html"),br(),
            a("About one-class model (English)   ",href="http://data-science.tokyo/ed-e/ede1-6-4-2.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-6-4-2.html"),
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
              conditionalPanel(
                condition = "input.Clustering == 'HDBSCAN1'",
                p("If HDBSCAN is used, clust name 0 is the samples judged as outliers")
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
            
            a("About NetworkMDS (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-3-1-1-1.html"),
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
          p("Clust name 0 is the samples judged as outliers"),
          conditionalPanel(
            condition = "input.Method_Dimension_All == 'hclust1'", 
            p("Download analyzed data for the next step.
                For example, if clust column is put on the first (left side) column,
                we can analyze with the function 'Similarity_of_Variables_and_Categories'"),
            downloadButton("downloadData51", "Download analyzed data")
          ),
          conditionalPanel(
            condition = "input.Method_Dimension_All != 'hclust1'", 
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
        
        conditionalPanel(
          condition = "input.Matrix_type == 'A_B'",
          conditionalPanel(
            condition = "input.A_B_method == 'Bipartite_graph1'",
            p("value of matrix is changed into the width of the edge, Cannot use negative values and categorical values"),
            plotOutput("plot401"),
            
            
            
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-06.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-06.html")
          ),
          conditionalPanel(
            condition = "input.A_B_method == 'Using_other_variables1'",
            conditionalPanel(
              condition = "input.Make_variables != 'Factor1'",
              plotlyOutput("plot405"),
              downloadButton("downloadData6", "Download analyzed data"),
            ),
            
            conditionalPanel(
              condition = "input.Make_variables == 'Correspondence5'",
              p("Analysis of count data (0, 1, 2, 3,,,,), Not use for negative values and categorical values"),
              h4("Contribution rate of eigenvalue"),
              textOutput("text4051"),
              h4("No of dimensions used in the model"),
              p("If contribution rate of eigenvalue is over 0.01, the dimension is used."),
              textOutput("text4052"),
              
            ),
            
            conditionalPanel(
              condition = "input.Make_variables == 'Factor1'",
              h3("Factor analysis"),
              verbatimTextOutput("text407"),
              h3("Similarity of variables by heatmap"),
              plotlyOutput("plot407"),
              h3("Similarity of variables"),
              p("Axis1 and axis2 do not have physical meaning. These two dimension are made from multi-dimension data."),
              plotlyOutput("plot409"),
              h3("Similarity of samples related with factors"),
              plotlyOutput("plot205"),
              a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E4-09.html"),
              a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J4-09.html"),br(),
              a("About Factor analysis(English)   ",href="http://data-science.tokyo/ed-e/ede1-2-4.html"),
              a("(Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-4.html"),br(),
            ),
            h4("Algorithm"),
            p("1. Make ulti-dimensional data."),
            p("2. Bind two matrix. One is for row. The other is for column"),
            p("3. MDS(sammon) or t-SNE to change from high dimension into 2 dimension"),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E1-06.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J1-06.html"),br(),
            a("About the reason to use MDS after Correspondence analysis (English)   ",href="http://data-science.tokyo/ed-e/ede1-3-4-2-4.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-3-4-2-4.html")
          ),
          conditionalPanel(
            condition = "input.A_B_method == 'Independence_Test1'",
            p("Analysis of count data (0, 1, 2, 3,,,,), Not use for negative values and categorical values"),
            
            verbatimTextOutput("text403"),
            
            a("About Two_way_GLM (English)   ",href="http://data-science.tokyo/ed-e/ede1-2-3-3-1.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-2-3-3-1.html")
          ),
          conditionalPanel(
            condition = "input.A_B_method == 'Two_way_GLM1'",
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
          
        ),
        
        conditionalPanel(
          condition = "input.Matrix_type == 'A_A'",
          conditionalPanel(
            condition = "input.A_A_method == 'Monopartite_graph1'",
            p("The data is needed to be the adjacency matrix. (large = close)"),
            #selectInput("network_library6", "Library of network",  choices = c("visNetwork", "igraph")),
            plotOutput("plot4012"),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E5-04.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J5-04.html"),
          ),
          conditionalPanel(
            condition = "input.A_A_method == 'MDS_sammon1'",
            p("The data is needed to be the distance matrix. (large = far)"),
            plotlyOutput("plot4014"),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E4-04.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J4-04.html"),
          ),
          conditionalPanel(
            condition = "input.A_A_method == 'Eigen_value1'",
            verbatimTextOutput("text4013"),
            downloadButton("downloadData4013", "Download analyzed data"),
          ),
        ),
      ),
      
      conditionalPanel(
        condition = "input.analysis == 'Time_series1'",
        conditionalPanel(
          condition = "input.Dimension_type == 'One_variable'",
          conditionalPanel(
            condition = "input.Method4 == 'Difference_previous'",
            h3("Row data"),
            plotlyOutput("plot503"),
            h3("Difference from previous value"),
            plotlyOutput("plot501"),
            plotlyOutput("plot502"),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E7-02.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J7-02.html"),
          ),
          
          conditionalPanel(
            condition = "input.Method4 == 'Auto_correlation'",
            h3("Auto_correlation"),
            p("Data to analyze"),
            plotlyOutput("plot724"),
            p("Correlogram, Auto_correlation"),
            plotOutput("plot723"),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E7-02.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J7-02.html"),
          ),
          conditionalPanel(
            condition = "input.Method4 == 'fft'",
            h3("fft"),
            p("Data to analyze"),
            plotlyOutput("plot722"),
            p("fft output"),
            plotlyOutput("plot721"),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E7-02.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J7-02.html"),
          ),
          conditionalPanel(
            condition = "input.Method4 == 'Quasi_periodic'",
            h3("Quasi periodic analysis"),
            dataTableOutput("text701"),
            downloadButton("downloadData700", "Download type 1 data"),
            downloadButton("downloadData704", "Download type 2 data"),
            br(),
            a("About quasi periodic data analysis (English)   ",href="http://data-science.tokyo/ed-e/ede1-9-3-6-4.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/ed/edj1-9-3-6-4.html"),br(),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E7-01.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J7-01.html"),
          ),
        ),
        
        conditionalPanel(
          condition = "input.Dimension_type == 'Multi_variable'",
          conditionalPanel(
            condition = "input.Method5== 'Dimension_reduction3'",
            h3("All variables"),
            plotlyOutput("plot701"),
            conditionalPanel(
              condition = "input.Dimension_reduction_type3 != 'None3'",
              verbatimTextOutput("text702"),
              downloadButton("downloadData714", "Download analysed data"),
            ),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E7-03.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J7-03.html"),
          ),
          conditionalPanel(
            condition = "input.Method5== 'Stratifeid_graph3'",
            h3("Stratifeid graph"),
            plotlyOutput("plot702"),
            
          ),
          conditionalPanel(
            condition = "input.Method5== 'Cross_correlation'",
            h3("Cross correlation"),
            p("Data to analyze"),
            plotlyOutput("plot726"),
            p("Correlogram, Auto_correlation"),
            plotOutput("plot725"),
            a("Code (English)   ",href="http://data-science.tokyo/R-E/R-E7-02.html"),
            a(" (Japanese)   ",href="http://data-science.tokyo/R-J/R-J7-02.html"),
            
          ),
        )
      )
      
    )
  )
)