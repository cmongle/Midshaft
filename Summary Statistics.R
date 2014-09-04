#Summary Statistics

midshaft.raw.csa.function <- function(x)
{
  slicelength <- as.numeric(length(csa[which(label == x)]))
  slice.intervals <- as.integer(seq(1,slicelength, by=(slicelength*.05)))
  slice.intervals <- slice.intervals + min(slice[which(label == x)])
  slice.bind <- cbind(seq(1, slicelength), (csa[which(label == x)]))
  slice.matrix <- as.matrix(slice.bind[slice.intervals, ])
  csa.levels <- data.frame(rbind(slice.matrix[,2]))
  csa.levels <- cbind(x, csa.levels)
  colnames(csa.levels) <- c("specimen", seq(0,95, by=5))
  return(csa.levels)
}


x <- as.vector(data$label)

data <- mdply(unique(x), midshaft.raw.csa.function)
data <- as.matrix(data[,9:17])

apply(data, 2, sd)
apply(data, 2, mean)

write.table(rbind(apply(data, 2, mean),apply(data, 2, sd)), file="summary.txt", sep="\t")
