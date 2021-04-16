import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt


data = pd.read_excel(r'.\final_data\trust_in_EU.xlsx', usecols = [i for i in range(1, 36)])
data = data.loc[:, ['Trust_in_EU', 'EU_friends_abroad', 'share_other_eu', 'unemp_rate', 'ED5_8']]


sns.set_theme(style="ticks")

g = sns.pairplot(data, diag_kind='kde')
g.map_lower(sns.kdeplot, levels=5, color="purple")
plt.show()
