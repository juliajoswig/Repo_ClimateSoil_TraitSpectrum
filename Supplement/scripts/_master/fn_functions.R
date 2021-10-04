

# basic functions
  source(file.path(origin,"scripts","_support_functions","colours.R"))
  source(file.path(origin,"scripts","_support_functions","fn_add_alpha.R"))
  source(file.path(origin,"scripts","_support_functions","fn_color_to_biomes.R"))
  source(file.path(origin,"scripts","_support_functions","fn_color_to_traits.R"))
  source(file.path(origin,"scripts","_support_functions","fn_name_to_biomes.R"))
  source(file.path(origin,"scripts","_support_functions","fn_put_into_traitGroup.R"))
  source(file.path(origin,"scripts","_support_functions","fn_rename_variable_names.R"))
  source(file.path(origin,"scripts","_support_functions","target_order.R"))
  
  source(file.path(origin,"scripts","_support_functions","fn_rename_variable_names.R"))
  source(file.path(origin,"scripts","_support_functions","fn_rename_vars_fromXtoTRYname.R"))
  
# analysis functions
  source(file.path(origin,"scripts","_master","002a_Model_Building.R"))# wrapper

# Ridge Regression
  source(file.path(origin,"scripts","01_analysis_functions","RidgeRegression","fn_RidgeRegression.R"))
# PLS:
  source(file.path(origin,"scripts","01_analysis_functions","PLS","fn_analysis_pls.R"))
# Random Forest
  source(file.path(origin,"scripts","01_analysis_functions","RandomForest","fn_analysis_RandomForest.R"))
  # Linear Model lm
  source(file.path(origin,"scripts","01_analysis_functions","lm","fn_analysis_lm.R"))
  
# Hierarchcal Partitioning
  source(file.path(origin,"scripts","_master","002b_Hierarchical_Partitioning.R"))
  source(file.path(origin,"scripts","01_analysis_functions","HierarchicalPartitioning","fn_hp_N2_wrapper.R"))
  source(file.path(origin,"scripts","01_analysis_functions","HierarchicalPartitioning","fn_hp_N2_inner.R"))
  
