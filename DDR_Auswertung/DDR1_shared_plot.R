
rm(list=ls())

library(readxl)

R <<- 8.314462618    # J K^-1 mol^-1
p0 <<- 1013.25

COL_ACET_PRIM <<- "#458B00"
COL_ACET_SEC <<- "#7FFF00"

COL_NHEX_PRIM <<- "#0000CD"
COL_NHEX_SEC <<- "deepskyblue"

WIDTH <<- 7
HEIGHT <<- 5

quartz(height=HEIGHT, width=WIDTH)

# hack copied from https://stackoverflow.com/questions/1826519/how-to-assign-from-a-function-which-returns-more-than-one-value
':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}

functionReturningTwoValues <- function() {
  return(list(1, matrix(0, 2, 2)))
}
c(a, b) := functionReturningTwoValues()


digest_file <- function(filename, sheet) {
  
  raw_data = read_xlsx(path=filename, sheet=sheet)
  
  # for plot invt-lnp
  p = unlist(raw_data["p/mbar"])
  t = unlist(raw_data["T/°C"])
  lnp = log(p/p0)
  tk = t + 273.15   # t in K
  rez_tk = 1/tk
  
  sp1 = p
  sp2 = round(t, digits=1)
  sp3 = signif(rez_tk, digits=4)
  sp4 = round(lnp, digits=4)
  
  table_1 = data.frame(sp1,sp2,sp3,sp4, row.names=NULL)
  names(table_1) = c("p/mbar", "T/°C", "(1/T)/K^-1", "ln(p/p0)")
  print(table_1)
  
  gerade = lm(lnp ~ rez_tk)
  print(summary(gerade))
  a = summary(gerade)$coef[1,1]
  b = summary(gerade)$coef[2,1]
  sa = summary(gerade)$coef[1,2] # Standardfehler davon
  sb = summary(gerade)$coef[2,2]
  
  
  # for plot t-p
  dof = length(unlist(raw_data["p/mbar"]))-2  # degrees of freedom
  ts = qt(0.975, dof)   # ts Fraktile
  d_vH = -R * b      # molare Verdampfungsenthalpie
  c_d_vH = R * sb * ts   # entsprechendes Vertrauensintervall
  
  ntk = -b/a        # Normalsiedetemp. in K
  d_vS = R * a     # molare Verdampfungsentropie bei ntk
  
  table_2 = data.frame(d_vH, c_d_vH, ntk, d_vS)
  names(table_2) = c("d_vH", "c_d_vH", "ntk", "d_vS")
  print(table_2)
  
  t_curve = seq(from=5,to=ntk-273.15+2, length=201)
  p_curve = p0*exp(a + b/(t_curve + 273.15))
  
  
  return(list(
    rez_tk,
    lnp,
    gerade,
    
    t,
    p,
    ntk,
    t_curve,
    p_curve
  ))
  
}


ddr <- function(filename, sheet_acet, sheet_nhex, export_name_inv_ln, export_name_tp){
  
  # -- digest excel files --
  # acetone
  c(
    acet_rez_tk,
    acet_lnp,
    acet_gerade,
    
    acet_t,
    acet_p,
    acet_ntk,
    acet_t_curve,
    acet_p_curve
  ) := digest_file(filename, sheet_acet)
  
  # n-hexane
  c(
    nhex_rez_tk,
    nhex_lnp,
    nhex_gerade,
    
    nhex_t,
    nhex_p,
    nhex_ntk,
    nhex_t_curve,
    nhex_p_curve
  ) := digest_file(filename, sheet_nhex)
  
  # plot rez_tk / lnp
  plot(acet_rez_tk, acet_lnp, type="n",
       xlim=c(0.00293, 0.00358),
       ylim=c(-2.4, 0.0),
       xlab=expression((1/italic(T))*" / "*K^-1),
       ylab=expression("ln("*italic(p/p[0])*")")
  )
  grid(nx = NULL, ny = NULL,
       lty = 1,      # Grid line type
       col = "lightgray", # Grid line color
       lwd = 1)      # Grid line width

  abline(acet_gerade, lwd=2, col=COL_ACET_PRIM)
  points(acet_rez_tk, acet_lnp, type="p",pch=21, bg=COL_ACET_SEC, col=COL_ACET_PRIM)

  abline(nhex_gerade, lwd=2, col=COL_NHEX_PRIM)
  points(nhex_rez_tk, nhex_lnp, type="p",pch=21, bg=COL_NHEX_SEC, col=COL_NHEX_PRIM)

  legend(
    "topright",
    legend=c(
      "Acetone", "n-Hexane"
    ),
    col=c(COL_ACET_PRIM, COL_NHEX_PRIM),
    pt.bg=c(COL_ACET_SEC, COL_NHEX_SEC),
    pch=c( 21,21),
    bg="white"
  )
  
  dev.copy2pdf(file=export_name_inv_ln, width=WIDTH)
  

  plot(acet_t, acet_p, type="n", las=1,
     xlim=c(0,nhex_ntk-273.15+7),
     ylim=c(0,1200),
     xlab=expression(italic(T)*" / "*"°C"),
     ylab=expression(italic(p)*" / "*"mbar")
  )
  grid(nx = NULL, ny = NULL,
       lty = 1,      # Grid line type
       col = "lightgray", # Grid line color
       lwd = 1)      # Grid line width
  
  # acetone
  lines(acet_t_curve, acet_p_curve, lwd=2, col=COL_ACET_PRIM)
  points(acet_t, acet_p, type="p", pch = 21, bg=COL_ACET_SEC, col=COL_ACET_PRIM)
  abline(v=acet_ntk-273.15, lty=2)
  abline(h=p0, lty=2)

  lines(nhex_t_curve, nhex_p_curve, lwd=2, col=COL_NHEX_PRIM)
  points(nhex_t, nhex_p, type="p", pch = 21, bg=COL_NHEX_SEC, col=COL_NHEX_PRIM)
  abline(v=nhex_ntk-273.15, lty=2)
  
  legend(
    "left",
    legend=c(
      "Acetone", "n-Hexane", "boiling point (1 atm)"
    ),
    col=c(COL_ACET_PRIM, COL_NHEX_PRIM, "black"),
    pt.bg=c(COL_ACET_SEC, COL_NHEX_SEC),
    pch=c( 21,21, 3),
    lty=c(0,0,2),
    bg="white"
  )

  dev.copy2pdf(file=export_name_tp, width=WIDTH)
}

#ddr("DDR_Messdaten_1.xlsx", "n-Hexane (all)", "Fig_DDR1_nhex_1.pdf", "Fig_DDR1_nhex_2.pdf")

ddr(
  filename="DDR_Messdaten_1.xlsx",
  sheet_acet="Acetone (all)",
  sheet_nhex="n-Hexane (all)",
  export_name_inv_ln="ddr1_figs/DDR1_inv_ln.pdf",
  export_name_tp="ddr1_figs/DDR1_t_p.pdf"
)




