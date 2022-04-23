#UI CODE----
server <- function(input, output, session) {
  ###01-Uploading data server code----
  source(file = "www/Panels/2_Uploading_data/server/server_UD.R", local = T)
  ###02-Selecting features server code----
  source(file = "www/Panels/3_Selecting_features/server/server_SF.R", local = T)
  ###03-Results server code----
  source(file = "www/Panels/4_Results/server/server_R.R", local = T)
  
}