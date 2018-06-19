upload.dataset = function(fileName){
  return (read.csv(fileName, stringsAsFactors = F));
}