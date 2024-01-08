#!/usr/bin/env python
# coding: utf-8

# In[1]:


from IPython.display import display, Math


# In[2]:


import math


# In[3]:


import pandas as pd


# In[4]:


# METAS UncLib
from metas_unclib import *  # import METAS UncLib
use_linprop()               # linear unc propagation


# In[ ]:





# ### Functions

# $$\Delta_VH_{\text{subst}} = \Delta_VH_{\text{ref}} {\rho_{\text{ref}} \over \rho_{\text{subst}}} {M_{\text{subst}} \over M_{\text{ref}}}$$

# In[5]:


def calc_h(H_ref, rho_ref, rho_subst, M_ref, M_subst, A_ref, A_subst):
    return (
        H_ref * (rho_ref / rho_subst)
        * (M_subst / M_ref)
        * (A_subst / A_ref)
    )


# In[6]:


def get_areas(path):
    return list(pd.read_csv(path, sep=" ").iloc[0])


# ### Measured values and constants

# In[28]:


# rho values literature [kg/dm3]

rho_lit_m = ufloat(0.7914, desc="rho methanol (literature)")
rho_lit_a = ufloat(0.7899, desc="rho acetone (literature)")
rho_lit_n = ufloat(0.6603, desc="rho n-hexane (n-hexane)")


# In[29]:


# uncertainties of devices
u_m = 0.002  # [g]
u_V = 0.08  # [mL]


# In[30]:


# density acetone measured in exercise 3
m_a = ufloat(19.76, u_m, desc="mass acetone")  # [g]
V_a = ufloat(25., u_V, desc="volume acetone")  # [mL]
rho_a = m_a / V_a  # [kg/dm3]
rho_a


# In[31]:


# acetone calculated with method of the Meister book
1/V_a.value * u_m + abs(-m_a.value / V_a.value**2) * u_V  # [kg/dm3]


# In[32]:


# density n-hexane measured in exercise 3
m_n = ufloat(16.45, u_m, desc="mass n-hexane")  # [g]
V_n = ufloat(25., u_V, desc="volume n-hexane")  # [mL]
rho_n = m_n / V_n  # [kg/dm3]
rho_n


# In[33]:


# n-hexane calculated with method of the Meister book
1/V_n.value * u_m + abs(-m_n.value / V_n.value**2) * u_V  # [kg/dm3]


# In[34]:


# Molar masses [kg/mol]
M_m = ufloat(32.04, desc="M methanol")
M_a = ufloat(58.08, desc="M acetone")
M_n = ufloat(86.18, desc="M acetone")


# In[35]:


# evaporation enthalpy
H_m = ufloat(37.43, 0.17, desc="H methanol")
H_m


# ### Reference: Methanol

# In[36]:


# read in measured areas
met_samples = get_areas("../ddr2_exports/methanol_areas.csv")
met_samples


# In[37]:


# transform to uncertainty object
A_met = ufloatfromsamples(met_samples, desc="are methanol")
A_met


# ### Acetone

# In[38]:


# read in measured areas
ac_samples = get_areas("../ddr2_exports/acetone_areas.csv")
ac_samples


# In[39]:


# transform to uncertainty object
A_ac = ufloatfromsamples(ac_samples, desc="are acetone")
A_ac


# In[40]:


# calculate evaporation enthalpy of acetone
H_ac = calc_h(
    H_ref=H_m,
    rho_ref=rho_lit_m,
    rho_subst=rho_a,
    #rho_subst=rho_lit_a,
    M_subst=M_a,
    M_ref=M_m,
    A_ref=A_met,
    A_subst=A_ac,
)  # [kJ/mol]
H_ac


# In[20]:


H_ac.stdunc


# In[21]:


H_ac_result = r"\Delta_VH_{{\text{{acetone}}}} = {:.1f} \pm{:.1f} \text{{ kJ/mol }}(95 \space \%)".format(H_ac.value, H_ac.stdunc * 2)
display(Math(H_ac_result))


# In[22]:


print(H_ac_result)


# ### n-Hexane

# In[23]:


# read in measured areas
nhex_samples = get_areas("../ddr2_exports/n-hexane_areas.csv")
nhex_samples


# In[41]:


# transform to uncertainty object
A_nhex = ufloatfromsamples(nhex_samples, desc="are n-hexane")
A_nhex


# In[42]:


# calculate evaporation enthalpy of acetone
H_nhex = calc_h(
    H_ref=H_m,
    rho_ref=rho_lit_m,
    rho_subst=rho_n,
    #rho_subst=rho_lit_n,
    M_subst=M_n,
    M_ref=M_m,
    A_ref=A_met,
    A_subst=A_nhex,
)  # [kJ/mol]
H_nhex


# In[43]:


H_nhex_result = r"\Delta_VH_{{\text{{n-hexane}}}} = {:.1f} \pm{:.1f} \text{{ kJ/mol }}(95 \space \%)".format(H_nhex.value, H_nhex.stdunc * 2)
display(Math(H_nhex_result))


# In[44]:


print(H_nhex_result)


# In[45]:


unc_budget(H_nhex)


# In[ ]:




