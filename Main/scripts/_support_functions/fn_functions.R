
# ------------------------------------------------------------
# basic functions
# ------------------------------------------------------------
  source(file.path(origin,"scripts","_support_functions","colours.R")) # colour selection for figures 
  source(file.path(origin,"scripts","_support_functions","fn_add_alpha.R")) # colour modulation (transparency)
  source(file.path(origin,"scripts","_support_functions","fn_color_to_biomes.R")) # colour of biomes
  source(file.path(origin,"scripts","_support_functions","fn_color_to_traits.R")) # trait colours
  source(file.path(origin,"scripts","_support_functions","fn_name_to_biomes.R")) # scipts colours
  source(file.path(origin,"scripts","_support_functions","fn_put_into_traitGroup.R")) # trait name to trait group
  source(file.path(origin,"scripts","_support_functions","fn_rename_variable_names.R")) # rename climate and soil variable names 
  source(file.path(origin,"scripts","_support_functions","target_order.R")) # codes the order of trait names
  
# ------------------------------------------------------------
# analysis functions 
# ------------------------------------------------------------
# Ridge Regression
  source(file.path(origin,"scripts","01_analysis_functions","1_Model_Building.R"))
  source(file.path(origin,"scripts","01_analysis_functions","RidgeRegression","fn_RidgeRegression.R"))
# Hierarchical Partitioning
  source(file.path(origin,"scripts","01_analysis_functions","2_Hierarchical_Partitioning.R"))
  source(file.path(origin,"scripts","01_analysis_functions","HierarchicalPartitioning","fn_hp_N2_wrapper.R"))
  source(file.path(origin,"scripts","01_analysis_functions","HierarchicalPartitioning","fn_hp_N2_inner.R"))
  
# ------------------------------------------------------------
# figure functions  
# ------------------------------------------------------------
  source(file.path(origin,"scripts","02_figure_functions","fig_1","pl_Figure_1a.R"))
  source(file.path(origin,"scripts","02_figure_functions","fig_1","pl_Figure_1b.R"))
  source(file.path(origin,"scripts","02_figure_functions","fig_2","pl_Figure_2.R"))
  source(file.path(origin,"scripts","02_figure_functions","fig_3","pl_Figure_3a.R"))
  source(file.path(origin,"scripts","02_figure_functions","fig_3","pl_Figure_3b.R"))
  source(file.path(origin,"scripts","02_figure_functions","fig_4","pl_Figure_4.R"))
  source(file.path(origin,"scripts","02_figure_functions","fig_3","pl_Figure_3_overlaySqunb.R"))

# ------------------------------------------------------------
# table functions
# ------------------------------------------------------------
  source(file.path(origin,"scripts","03_table_functions","tab_1","Table_1.R"))
  
