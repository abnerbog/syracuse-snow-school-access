library(httr)

#' Download a Private/Discoverable HydroShare Resource
#'
#' @param resource_id The 32-character ID of the HydroShare resource
#' @param save_path The directory to save the downloaded zip
get_data_from_hydroshare <- function(resource_id, save_path = ".") {

  # 1. Get credentials from .Renviron
  usr <- Sys.getenv("HYDROSHARE_USR")
  pwd <- Sys.getenv("HYDROSHARE_PWD")
  
  # 2. Use the official REST API endpoint for downloading the resource bundle
  # This endpoint handles Basic Auth correctly for private resources.
  url <- sprintf("https://www.hydroshare.org/hsapi/resource/%s/", resource_id)
  
  dest_file <- file.path(save_path, sprintf("%s.zip", resource_id))
  
  message("\nConnecting to HydroShare REST API...")
  
  # 3. Execute the request
  tryCatch({
    response <- GET(
      url, 
      authenticate(usr, pwd, type = "basic"),
      write_disk(dest_file, overwrite = TRUE),
      progress()
    )
    
    # 4. Check for Permission/Authentication errors
    # 401: Unauthorized (Wrong password)
    # 403: Forbidden (You don't have access to this specific resource)
    # 404: Not Found (The ID is wrong)
    s_code <- status_code(response)
    
    if (s_code == 401) {
      if (file.exists(dest_file)) file.remove(dest_file)
      stop("Authentication failed. Please check your username and password.")
    }
    
    if (s_code == 403) {
      if (file.exists(dest_file)) file.remove(dest_file)
      stop("Access Denied (403). This resource is private/discoverable and you do not have permission to view the files.")
    }
    
    if (http_error(response)) {
      if (file.exists(dest_file)) file.remove(dest_file)
      stop(sprintf("HTTP error %s", s_code))
    }
    
    message("\nSuccess! Resource downloaded to: ", dest_file)
    
  }, error = function(e) {
    message("\n--- Download Failed ---")
    message("Details: ", e$message)
    message("\nIf your credentials are correct, you need to request access from the resource owner to download these files.")
  })
}