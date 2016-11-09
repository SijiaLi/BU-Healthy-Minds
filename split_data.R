## I split the data according to their column names.
## Split each column name by "_", the first element will be the name of the data.frame where it'll be stored
## If there's no "_", column names will be the name of the data.frame.
## e.g 
######    "anx_score" ---> anx
######    "dx_anx" ---> dx
######    "gender" ---> gender
######    "relig_aff_other_text" ---> relig

##  111 new data.frame will be created
##  there will be some small overlap. BE CAREFUL!
##  some columns need to be reorganized (such as, "belong1"-"belong9")

hms <- read.csv("HMS raw data 2016.csv")
row.names(hms) <- hms$responseid

key <- paste("df",unlist(lapply(strsplit(names(hms), split="_"),function(x){x[1]})),sep = "_")
# function to create data.frame
pull <- function(x){
  df = as.data.frame(hms[,grep(x,key)])
  colnames(df) = names(hms)[grep(x,key)]
  rownames(df) = rownames(hms)  # if not defined, single column section will lose row names.
  assign(x, df, envir=.GlobalEnv)
}

invisible(lapply(unique(key),pull))
#######################

# save.image("split_data.RData")




