
## Requirements

* Mono: https://www.mono-project.com/download/stable/
* `pip install pythonnet`
* DON'T pip install clr
	* the right clr comes with pythonnet


```python
# Example Pendulum

# Michael Wollensack METAS - 22.08.2023

  

print('Example Pendulum')

print('Begin')

  

# METAS UncLib

from metas_unclib import * # import METAS UncLib

use_linprop() # linear unc propagation

  

# Input quantaties time measurement

n_periods = 20

  

n_tau_meas = ufloatfromdistribution(

StudentTDistribution(24.9060, 0.0712 / np.sqrt(10), 9),

desc='Time N Periods Measurement / s')

  

dtau_react = ufloatfromdistribution(

UniformDistribution(-0.1, 0.1),

desc='Time Reaction / s')

  

dtau_res = ufloatfromdistribution(

UniformDistribution(-0.005, 0.005),

desc='Time Resolution / s')

  

# Input quantaties length measurement

L_meas = ufloatfromdistribution(

StudentTDistribution(0.33316, 0.00026 / np.sqrt(6), 5),

desc='Length Measurement / m')

  

dL_res = ufloatfromdistribution(

UniformDistribution(-0.0005, 0.0005),

desc='Length Resolution / m')

  

dL_read = ufloatfromdistribution(

TriangularDistribution(-0.0005, 0.0005),

desc='Length Reading / m')

  

# Input quantaties diameter measurement

D_meas = ufloatfromdistribution(

StudentTDistribution(0.1005, 0.000058 / np.sqrt(4), 3),

desc='Diameter Measurement / m')

  

dD_res = ufloatfromdistribution(

UniformDistribution(-0.001, 0.001),

desc='Diameter Resolution / m')

  

dD_read = ufloatfromdistribution(

TriangularDistribution(-0.0005, 0.0005),

desc='Diameter Reading / m')

  

# Measurement model

n_tau = n_tau_meas + dtau_res + dtau_react

tau = n_tau / n_periods

L = L_meas + dL_res + dL_read

D = D_meas + dD_res + dD_read

  

g = 4 * np.pi**2 / tau**2 * (L + D / 2 +

(D**2 / (5 * (2 * L + D))))

  

# Output quantaty

print(g)

unc_budget(g)

  

print('End')
```
