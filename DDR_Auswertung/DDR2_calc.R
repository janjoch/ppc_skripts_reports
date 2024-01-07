
rm(list=ls())

A_m = read.table("ddr2_exports/methanol_areas.csv")
A_a = read.table("ddr2_exports/acetone_areas.csv")
A_n = read.table("ddr2_exports/n-hexane_areas.csv")

m = mean(unlist(A_m))
a = mean(unlist(A_a))
n = mean(unlist(A_n))

H_m = 37.43  # kJ/mol

rho_m = 0.7914
rho_a = 0.7899
rho_n = 0.6603

M_m = 32.04
M_a = 58.08
M_n = 86.18

H_a = H_m * rho_m / rho_a * M_a / M_m * a / m
H_n = H_m * rho_m / rho_n * M_n / M_m * n / m

sd_A_m = sd(A_m) / sqrt(3)
sd_A_a = sd(A_a) / sqrt(3)
sd_A_n = sd(A_n) / sqrt(3)

sd_H_m = 0.17
sd_ta_H_m = sd_H_m / H_m * H_a  # target acetone
sd_tn_H_m = sd_H_m / H_m * H_n  # target n-hexane

sd_a = sqrt(sd_ta_H_m^2 + sd_A_a^2)
sd_n = sqrt(sd_tn_H_m^2 + sd_A_n^2)
