# Set the working directory to this                  
# source file's location    
cyphertext <- readLines("this_file_shush.R")              
              
chars <- c( 
  " ",             
  ".",       
  "?",         
  "E",    
  "T", 
  "A",        
  "O",     
  "I", 
  "N",                  
  "S",       
  "H",            
  "R",                         
  "D",    
  "L",             
  "C",  
  "U", 
  "M",                 
  "W",      
  "F",                     
  "G",                       
  "Y",    
  "P", 
  "B",        
  "V",     
  "K", 
  "J",         
  "X",    
  "Q",    
  "Z"             
)          
 
msg_char <- rep(NA, cyphertext |> length())      
 
for (i in seq_along(msg_char)) {              
  this_row <- (cyphertext[i] |> strsplit(""))[[1]]      
            
  if (length(this_row) == 0) {                    
    msg_char[i] <- NA    
  }            
  else { 
    rle_code <- this_row |>               
      rev() |>           
      rle()      
            
    if (rle_code$values[1] == " ")      
      msg_char[i] <- chars[rle_code$lengths[1]]               
    else     
      msg_char[i] <- NA    
  }            
} 
          
msg_char |> na.omit() |> paste0(collapse = "")    
     
   


