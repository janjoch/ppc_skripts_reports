  #n-Hexane (increasing)
library(readxl
        )
ddr <- function(filename, sheet, export_name){
  
  nhex_inc = read_xlsx(path=filename, sheet=sheet)
  
  p = unlist(nhex_inc["p/mbar"])
  t = unlist(nhex_inc["T/°C"])
  p0 = 1013.25
  lnp = log(p/p0)
  tk = t + 273.15   # t in K
  rez_tk = 1/tk
  
  sp1 = p
  sp2 = round(t, digits=1)
  sp3 = signif(rez_tk, digits=4)
  sp4 = round(lnp, digits=4)
  
  table = data.frame(sp1,sp2,sp3,sp4)
  names(table) = c("p/mbar", "T/°C", "(1/T)/K^-1", "ln(p/p0)")
  print(table)
  
  gerade = lm(lnp ~ rez_tk)
  print(summary(gerade))
  a = summary(gerade)$coef[1,1]
  b = summary(gerade)$coef[2,1]
  sa = summary(gerade)$coef[1,2]
  sb = summary(gerade)$coef[2,2]
  
  plot(rez_tk, lnp, type="n",
       xlab=expression((1/italic(T))*" / "*K^-1),
       ylab=expression("ln("*italic(p/p[0])*")")
  )
  abline(gerade, lwd=2)
  points(rez_tk,lnp, type="p",pch=21, bg="white")
  title(main="n-Hexane", sub="measured at steadily incresasing pressure and temperature")
  dev.copy2pdf(file=export_name, width=7)
  table
  
}
  
ddr("DDR_Messdaten_1.xlsx", "n-Hexane (increase)", "Fig_DDR1_nhex_incr.pdf")

ddr("DDR_Messdaten_1.xlsx", "n-Hexane (decrease)", "Fig_DDR1_nhex_decr.pdf")

table <- ddr("DDR_Messdaten_1.xlsx", "Acetone (decrease)", "Fig_DDR1_acet_decr.pdf")


