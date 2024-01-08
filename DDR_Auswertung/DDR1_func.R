
rm(list=ls())

library(readxl)

R = 8.314462618    # J K^-1 mol^-1

ddr <- function(filename, sheet, export_name_1, export_name_2){
  
  raw_data = read_xlsx(path=filename, sheet=sheet)
  
  p = unlist(raw_data["p/mbar"])
  t = unlist(raw_data["T/°C"])
  p0 = 1013.25
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
  
  plot(rez_tk, lnp, type="n",
       xlab=expression((1/italic(T))*" / "*K^-1),
       ylab=expression("ln("*italic(p/p[0])*")")
  )
  abline(gerade, lwd=2)
  points(rez_tk,lnp, type="p",pch=21, bg="white")
  dev.copy2pdf(file=export_name_1, width=7)
  
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
  
  plot(t, p, type="n", las=1,
       xlim=c(0,ntk-273.15+7),
       ylim=c(0,1200),
       xlab=expression(italic(T)*" / "*"°C"),
       ylab=expression(italic(p)*" / "*"mbar")
  )
  lines(t_curve, p_curve, lwd=2)
  points(t,p, type="p", pch = 21, bg="white")
  abline(v=ntk-273.15, lty=2)
  abline(h=p0, lty=2)
  dev.copy2pdf(file=export_name_2, width=7)
}

ddr("DDR_Messdaten_1.xlsx", "n-Hexane (all)", "Fig_DDR1_nhex_1.pdf", "Fig_DDR1_nhex_2.pdf")

ddr("DDR_Messdaten_1.xlsx", "Acetone (all)", "Fig_DDR1_acet_1.pdf", "Fig_DDR_acet_2.pdf")




