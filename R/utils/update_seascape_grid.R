#update seascape

old_path <- "C:\\LEHOUX\\Maquereau\\Recherche\\mack_IBM\\data\\tmp_stack\\"
new_path <- "data/tmp_stack/"

# Loop through each RasterBrick and each layer
for(name in names(seascape)) {
  for(i in 1:nlayers(seascape[[name]])) {
    # Check if layer is from disk (not in memory)
    if(filename(seascape[[name]][[i]]) != "") {
      old_file <- filename(seascape[[name]][[i]])
      new_file <- gsub(pattern=old_path, replacement=new_path, x=old_file, fixed=T)
      seascape[[name]] <- brick(new_file)
    }
  }
}
