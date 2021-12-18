server <- function(input, output, session) {
  
  source(file = "www/Panels/2_Uploading_data/server/server_UD.R", local = T)
  
  source(file = "www/Panels/3_Selecting_features/server/server_SF.R", local = T)
  
  source(file = "www/Panels/4_Results/server/server_R.R", local = T)
  
}