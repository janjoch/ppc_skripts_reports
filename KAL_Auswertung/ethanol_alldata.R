
# author: github.com/janjoch, 2024

rm(list = ls()) # tabula rasa

library(readxl)

source("helpers.R")

WIDTH <<- 5
HEIGHT <<- 4

# quartz(height=HEIGHT, width=WIDTH)

# par(
#   mfrow=c(1,2),
#   mar=c(0.7, 5,1,0.7)
# )


# import data
data.import = read.csv("raw_data/waermekapazitaet_ethanol.csv")
mass = (data.import$mass_percentage_measured * 100)
heat = data.import$specific_heat_capacity_mean
heat.ci = data.import$standard_error_result_95
group = data.import$group

legend.import = read.csv("raw_data/waermekapazitaet_ethanol_groups.csv")
legend.id = legend.import$id
legend.name = legend.import$name


min.id = which.min(heat)
max.id = which.max(heat)
max.val = heat[max.id] + heat.ci[max.id]
min.val = heat[min.id] - heat.ci[min.id]
span = max.val - min.val
margin = 0.05
max.val = max.val + margin * span
min.val = min.val - margin * span


#plot.colorcycle = c("deepskyblue", "red", "green")
plot.colorcycle = c('#006BA4', '#FF800E', '#ABABAB', '#595959', '#5F9ED1', '#C85200', '#898989', '#A2C8EC', '#FFBC79', '#CFCFCF')

par(mai = c(1,1.2,0.3,0.3))
plot.init.grey(
  mass, heat,
  xlab="Massen-% Ethanol",
  ylab=expression("spezifische Wärmekapazität "*italic(c)[p]^sp*" / "*J*K^-1*g^-1),
  #xaxt='n'
  ylim=c(min.val, max.val),
  #ylim=c(acet.rho.data[1] - 1.2 * acet.rho.se[1], acet.rho.data[1] + 1.2 * acet.rho.se[1]),
)

plot.grid(nx=NA)

FBy(
  mass,
  heat,
  heat.ci,
  bg=plot.colorcycle[group]
)

# break

legend(
  "topright",
  legend=legend.name,
  pt.bg=plot.colorcycle[legend.id],
  pch=rep(21, length(legend.name)),
  lty=rep(0, length(legend.name)),
  bg="white"
)

# text(35,3000, "PRELIMINARY!", cex=2)



plot.save("exports/", "ethanol_alldata_5_4_in.pdf")

