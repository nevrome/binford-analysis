default_model <- function(df, relation){
  
  # create model function
  model_func <- function(df){
    lm(relation, data = df)
  }
  
  # check if input data.frame is grouped
  # if not, add artificial grouping into one
  # big group
  if(!is.grouped_df(df)){
    df %<>% mutate(id = 1) %>%
      group_by(id)
  }
  
  # calculate model and parameters
  df %<>% nest %>% 
    mutate(model = map(data, model_func)) %>%
    mutate(
      predictions = map2(data, model, add_predictions),
      resids = map2(data, model, add_residuals),
      glance = map(model, broom::glance)
    )
  
  return(df)
}
