confusion.expand <- function (pred.c, class) {
  temp <-confusion(pred.c,class)
  row.sum <- apply(temp,1,sum)
  col.sum <- apply(temp,2,sum)
  t.sum <- sum(col.sum)
  tmp <- rbind(temp, rep("----", dim(temp)[2]), col.sum)
  tmp <- noquote(cbind(tmp, rep("|",dim(tmp)[1]), c(row.sum, "----", t.sum)))
  dimnames(tmp)<-list(object = c(dimnames(temp)[[1]],"-------","Col Sum"),
                      true = c(dimnames(temp)[[2]],"|","Row Sum"))
  attr(tmp, "error") <- attr(temp, "error")
  attr(tmp, "mismatch") <- attr(temp, "mismatch")
  return(tmp)
}
