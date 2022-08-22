#' Generate Project Folder Substructures
#'
#' @param project_dir A character argument containing the file 
#' path where an R project can be found. Defaults to `NULL`, which
#' assumes the user already has an existing R project opened.
#' 
#' @param ... Reserved for future development, currently unused.
#'
#' @return See details
#' 
#' @details If the function is run from within an R project it 
#' will create any of the substructure directories that are not
#' already present. If project_dir is specified as a valid 
#' directory, the function will search for an existing .Rproj
#' file and open it if one is found and throw an error otherwise
#' 
#' @importFrom rstudioapi getActiveProject openProject
#' 
#' @export make_project_dirs
#'
#' @examples
#' # Create folders in an existing project directory
#' make_project_dirs(project_dir = NULL)
#' 
#' # Alternatively, open an existing project
#' make_project_dirs(project_dir = "E:/Users/Dropbox/psci-3300-political-science-research")
#' 
make_project_dirs <- function(project_dir = NULL, ...) {
  
  ## Get the project directory
  if (is.null(project_dir)) {
    
    ## Get the directory for the RStudio project
    base_dir <- rstudioapi::getActiveProject()
    
    ## Validate the base directory
    stopifnot("You do not currently have an R project open" = !is.null(base_dir))
    
    ## Potential directory names to create
    new_dirs <- paste(
      base_dir, 
      c("data", "scripts", "functions", "documents", "figures", "models"), 
      sep = "/"
    )
    
    ## Check if the required directories already exist
    existing_dirs <- dir.exists(new_dirs)
    
    ## Directories to create excluding those that already exist
    create_dirs <- new_dirs[!existing_dirs]
    
    if (length(create_dirs) > 0) {
      # Loop across each directory to create
      for (i in seq_along(create_dirs)) {
        dir.create(path = create_dirs[i])
        print(paste(create_dirs[i], "was created in the project directory"))
      }
    }
    else {
      print("All required directories are already present in the project folder")
    }
  }
  
  ## Otherwise open the folder where the project is located
  else if (is.character(project_dir)) {
    
    ## Check if an RStudio project exists in the specified directory
    project_path <- list.files(
      path = project_dir,
      pattern = ".*.Rproj",
      full.names = TRUE
    )
    
    ## Validate the project path
    stopifnot("No R project was found in the specified directory" = length(project_path) > 0)
    
    ## Print Instructions
    print("An R project was found in the specified directory. You will need to run this function again from within the project")
    
    ## Open the R project in  the specified directory
    rstudioapi::openProject(path = project_path)
    
  }
}

