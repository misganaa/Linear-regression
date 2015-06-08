# A PACKAGE FOR LINEAR REGRESSION
LR<-function(x,y)
{
  sum_x<-sum(x)
  sum_y<-sum(y)
  mult_xy<-sum(x*y)
  n<-length(x)
  b<-(n*mult_xy-(sum_x*sum_y))/(n*sum(x^2)-(sum_x)^2)
  a<-(sum_y-b*sum_x)/n
  y_e<-a+b*x
  sse<-sum((y-y_e)^2)
  sst<-sum((y-mean(y))^2)
  R_squared<-1-sse/sst
  pdf("./Desktop/mygraph.pdf")
  plot (x,y,col="red", xlab=substitute(x),ylab=substitute(y))
  lines(x,y_e,type="l", col="blue")
  dev.off()
  sprintf ("Coefficient of determination, R^2=%f",R_squared)
}
