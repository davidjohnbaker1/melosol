#--------------------------------------------------
# Metadata for Berkowitz Script 
#--------------------------------------------------
# Import Meta Data In Global Environment
meta <- read.csv("berk_melo_meta.csv",
                  check.names = FALSE,
                  sep = ",")

#--------------------------------------------------
# Whoops, forgot .krn 

meta$Filename <- paste0(meta$Filename, ".krn")

#--------------------------------------------------
# Loop for krn files

add_metadata <- function(fns=list.files(pattern = "*.krn")){
  
  if (!dir.exists("kern_output")){
    dir.create("kern_output")
  } else {
    print("Dir already exists!")
  }
  
  # Import Krn Files 
  for(i in seq(along=fns)){
    
    #--------------------------------------------------
    # Import Files 
    krn_file <- readLines(con = fns[i])
    print(paste("Now Working On File",fns[i]))
    
    #--------------------------------------------------
    # Load in File Name and Indexer 
    krn_file_name <- paste("!!! OTL:",
                           gsub(pattern = "\\.krn", 
                                replacement = "", 
                                basename(fns[i])))
    
    krn_file_name_id <- basename(fns[i])
    
    #--------------------------------------------------
    # Grabs Metadata from Global Environment 
    selected_meta <- meta[meta$Filename==krn_file_name_id,]
    
    #--------------------------------------------------
    # Extract Objects 
    #--------------------------------------------------
    # Spreadsheet Data 
    encoder <- paste("!!! Encoder:",selected_meta$`!!! Encoder`)
    section <- paste("!!! Section:",selected_meta$`!!! Section`)
    page <- paste("!!! Page:",selected_meta$`!!! Page`)
    chapter <- paste("!!! Chapter:",selected_meta$`!!! Chapter`)
    music_mode <- paste("!!! Mode:",selected_meta$`!!! Mode`)
    key <- paste0("*", selected_meta$Key,":") 
    
    id_line <- which(grepl(pattern = "\\*k\\[",x = krn_file)) # Used for Searching
    
    #--------------------------------------------------
    # Combine Data Together
    
    updated_krn <- c(krn_file_name, 
                     chapter, 
                     section, 
                     page, 
                     music_mode, 
                     encoder, 
                     krn_file[1:id_line], 
                     key,
                     krn_file[(id_line + 1):length(krn_file)])
    
    updated_krn 
    writeLines(text = updated_krn, con = paste0("kern_output/",fns[i]))
    
    }
    
  
}

add_metadata()
