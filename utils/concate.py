import pandas as pd
df = pd.read_csv("base_rtk.csv", sep=';', header=0)
df2 = pd.read_csv("ff.csv", sep=';', header=0)
df2 = df2.drop(["Site"],axis=1)
newdf = df.join(df2.set_index('Arbre'),on='Arbre', how="outer",lsuffix='_caller', rsuffix='_other')
newdf = newdf.dropna(axis=0)
newdf.to_csv("merged.csv", sep=';')
