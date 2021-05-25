from modules.comforttools.two_node_model import cal_ce, cal_set
from modules.comforttools.pmv import cal_pmv
import pandas as pd


df = r.df
df["PMV"] = 0.0
df["SET"] = 0.0
ta = 30
rh = 79
clo = 0.5
met = 1.1

for i in df.index:
  v = df.at[i, "v"]
  df.at[i, "SET"] = cal_set(ta=ta, tr=ta, v=v, rh=rh, met=met, clo=clo)
  ce = cal_ce(ta=ta, tr=ta, v=v, rh=rh, met=met, clo=clo)
  df.at[i, "PMV"] = cal_pmv(ta=ta - ce, tr=ta - ce, rh=rh, v=0.1, clo=clo, met=met)
