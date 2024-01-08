#!/usr/bin/env python
# coding: utf-8

# In[6]:


import math


# In[1]:


from metas_unclib import *


# $c_L^{sp}$: spezifische Wärmekapaziät 
# 
# $C$: Wärmekapazität Wasser
# 
# $m$: Masse Lösungsmittel
# 
# $U, I$: Spannung, Strom Heizung
# 
# $b$: Steigung aus T-t-Diagramm

# In[8]:


def uniform(value, a):
    """
    returns UniformDistribution(value - a, value + a)
    """
    return UniformDistribution(value - a, value + a)


# ### $C_{Dewar}$

# $$C_{Dewar} = C_{Sys,kal} - C_W = {U I \over b} - m_W c_W^{sp}$$

# In[9]:


ref_V = ufloatfromdistribution(
  uniform(0.1, 0.0001),  # L
  desc='reference volume / L'
)
ref_V


# In[11]:


# rho water
water_rho = ufloat(0.9982, desc="water rho / kg/L")
water_m = water_rho * ref_V
water_m


# In[12]:


water_c_sp = ufloat(4182, desc="water specific heat capacity / J/K/kg]")


# In[13]:


U = ufloatfromdistribution(
  uniform(10.0, 0.05),  # V
  desc='voltage / V'
)
I = ufloatfromdistribution(
  uniform(1.61, 0.005),  # V
  desc='current / A'
)
P = U * I
P


# In[19]:


water_slope = ufloatfromsamples(
    (0.027875, 0.02771894, 0.02757587),
    desc='slope of water calibration / K/s'
)
water_slope


# In[20]:


dewar_C = U * I / water_slope - water_m * water_c_sp
dewar_C


# ### $c_L^{sp}$

# $$c_L^{sp} = {C_L \over m} = {C_{Sys,L} - C_{Dewar} \over m} = {U I \over b m} - {C_{Dewar} \over m}$$

# In[25]:


sol_slope = ufloat(
    0.0398696981455289,
    2.96817283935158E-05,
    desc="slope of solution / K/s"
)
sol_slope


# In[30]:


# since the scale is very accurate in comparison to the other uncertainties, the mass measurement is very accurate
sol_m_perV = ufloat(
  0.08236242,  # kg
  desc='solution mass / kg'
)


# In[31]:


U * I / sol_slope - dewar_C


# In[32]:


sol_c_sp = (U * I / sol_slope - dewar_C) / sol_m_perV
sol_c_sp


# In[ ]:




