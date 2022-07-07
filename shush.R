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

str2vec <- function(s) strsplit(s, split = "")
strlen <- function(s) str2vec(s)[[1]] |> length()

msg     <- "WELL DONE IT WORKED. MAYBE IT NEEDS A LARGER CHARACTER SET?"
msg_vec <- (msg |> str2vec())[[1]]
msg_spaces_n <- rep(NA, msg_vec |> length())

for (i in seq_along(msg_vec))
  msg_spaces_n[i] <- which(msg_vec[i] == chars)

msg_spaces <- sapply(msg_spaces_n,
                     \(x) rep(" ", x) |> 
                       paste(collapse = ""))

input <- readLines("this_file_shush.R")
trimmed <- trimws(input, "r", " ")

final_rows <- max(length(trimmed),
                  length(msg_spaces))

pad_nas <- function(v, tot_len)
  c(v, rep(NA, max(0, tot_len - length(v))))

both <- data.frame(
  data   = pad_nas(trimmed, final_rows),
  spaces = pad_nas(msg_spaces, final_rows)
)

glue_space <- Vectorize(function(data, spaces) {
  paste0(
    ifelse(is.na(data),   "", data),
    ifelse(is.na(spaces), "", spaces)
  )
})

encoded <- glue_space(both$data, both$spaces)
writeLines(encoded, "this_file_shush.R")

#############################################

cyphertext <- readLines("this_file_shush.R")

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
