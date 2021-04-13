jd2r_test<-function(jtest){
  if (is.jnull(jtest))
    return (NULL)
  else{
    desc<-.jcall(jtest, "S", "getDescription")
    val<-.jcall(jtest, "D", "getValue")
    pval<-.jcall(jtest, "D", "getPvalue")
    return (list(value=val, pvalue=pval, distribution=desc))
  }
}
