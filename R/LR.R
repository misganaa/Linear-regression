# A PACKAGE FOR LINEAR REGRESSION

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 2 of the License, or
#     (at your option) any later version.

#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.

#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.


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
