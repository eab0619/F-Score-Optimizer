import pandas as pd
import benchmark_metrics as bm



isr_joined_raw = pd.read_csv('../data-raw/isr_joined_raw.csv').iloc[:, 1:]

#drop cases where names are missing
isr_joined_raw = isr_joined_raw.dropna(subset=['name_first', 'name_last'])

#get homonomy dataset
isr_homonomy = bm.get_homonomy_rate(isr_joined_raw)[1]

#get name variation dataset
isr_var= bm.get_name_var(isr_joined_raw)[1]

#note that mention id is the unique identifier here
isr_final = isr_homonomy.merge(isr_var.loc[:,['mention_id', 'var_count', 'is_var']], on='mention_id')


#subset rows where each record has homonomy record or name variation
isr_noisy = isr_final.loc[(isr_final.homonomy==1) | (isr_final.var_count>0),:]


##generation scheme

#by definition n==1 are those without name variation (var_count==0) and homonomy==1
id1 = isr_noisy.groupby(['unique_id']).size().to_frame('n').reset_index().query('n==1').unique_id
id2 = isr_noisy.groupby(['unique_id']).size().to_frame('n').reset_index().query('n>1').unique_id


#separate into bipartite files
dfA = isr_noisy.loc[isr_noisy.unique_id.isin(id2)].groupby("unique_id").apply(lambda x: x.sample(1, random_state=1))
dfA = pd.concat([dfA,isr_noisy.loc[isr_noisy.unique_id.isin(id1)].copy()]).reset_index(drop=True)
dfA["file"] = "A"

dfB = isr_noisy.loc[isr_noisy.unique_id.isin(id2)].groupby("unique_id").apply(lambda x: x.sample(1, random_state=2)).reset_index(drop=True)
dfB["file"] = "B"


combined = pd.concat([dfA, dfB]).reset_index(drop=True)

match_rate = len(dfB.merge(dfA.loc[:,"unique_id"], on='unique_id', how='inner'))/ len(combined)
print("The match rate is: " + str(match_rate))
print("The Homonomy rate is: " + str(bm.get_homonomy_rate(combined)[0]))
print("The name variation rate is: " + str(bm.get_name_var(combined)[0]))

combined.to_csv("../data-raw/isr_benchmark-1.tsv", sep="\t", index=False)

