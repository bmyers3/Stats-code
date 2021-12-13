
#-----------------------------------------------------------
#  R Function for Two sample Z-test for Proportions
#-----------------------------------------------------------
twoSampleProp<-function(alpha,H1,n1,n2,x1,x2){
  
  p1hat <- x1/n1
  p2hat <- x2/n2
  p0hat <- (x1+x2)/(n1+n2)
  
  z0 <- (p1hat-p2hat)/sqrt(p0hat*(1-p0hat)*(1/n1+1/n2))
  
  if (H1==1) {# H1: p1>p2
    pvalue <- 1-pnorm(z0,0,1)
    H1type <- ">" 
  }else if (H1==2) {# H1: p1<p2
    pvalue <- pnorm(z0,0,1)
    H1type <- "<" 
  }else if (H1==3) {# H1: p1!=p2
    pvalue <- 2*pnorm((-1)*abs(z0),0,1)
    H1type <- "!=" 
  }
  
  if (pvalue<alpha) {
    Decision <- "Reject H0"
    inequal  <- "less" 
  }else if (pvalue>alpha) {
    Decision <- "Do Not Reject H0"
    inequal  <- "greater" 
  }
  
  
  cat(
    paste("\n Two Sample Proportion Z-test","\n",
          "---------------------------------\n", 
          "    H0: p1=p2","  vs  ","H1: p1",H1type,"p2","\n",
          "---------------------------------\n", 
          "---------------------------------------\n", 
          " n1 = ",n1,"   x1 = ",x1,"   p1hat = ", round(p1hat,4),"\n",
          " n2 = ",n2,"   x2 = ",x2,"   p2hat = ", round(p2hat,4),"\n",
          "---------------------------------------\n", 
          "-------------------------------------------------------------------------\n", 
          " p0hat = ",round(p0hat,4),"    z0 = ",round(z0,4),"    p-value = ",round(pvalue,4),"\n",
          " We ",Decision," since p-value(=",round(pvalue,4),") is ",inequal," than alpha(=",alpha,")\n",sep="",
          "-------------------------------------------------------------------------\n\n\n" ))
}

# Execute twoSampleProp function

twoSampleProp(0.05,1,2103,1671,547,368)
twoSampleProp(0.05,2,2103,1671,547,368)
twoSampleProp(0.05,3,2103,1671,547,368)

#90% = z = 1.28
#95 = z = 1.645
#97.5 = z = 1.96
#99.9 = z = 2.33
#99.95 = z = 2.58
#CI formula: xbar +- zalpha/2(sigma/sqrt(n)) 





