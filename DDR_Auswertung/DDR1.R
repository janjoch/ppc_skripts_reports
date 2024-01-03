#n-Hexane (increasing)

nhex_inc = read_xlsx("DDR_Messdaten_1.xlsx", sheet="n-Hexane (increase)")

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
dev.copy2pdf(file="Fig_DDR1_nhex_incr.pdf", width=7)





