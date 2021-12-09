cal.fresnel.ellipse <- function(h, thetra, lambda) {
  C = h*(1+lambda/(2*sin(thetra)))/tan(thetra)
  a = 1/sin(thetra)* sqrt(lambda*h/sin(thetra) + (lambda/(2*sin(thetra)))^2)
  b = a*sin(thetra)
  return(c("C" = C, "a" = a, "b" = b))
}
