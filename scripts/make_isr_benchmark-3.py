import pandas as pd
import benchmark_metrics as bm

### This benchmark dataset has high variation rate

isr_joined_raw = pd.read_csv('../data-raw/isr_joined_raw.csv').iloc[:, 1:]

#drop cases where names are missing
isr_joined_raw = isr_joined_raw.dropna(subset=['name_first', 'name_last'])

#get homonomy dataset
isr_homonomy = bm.get_homonomy_rate(isr_joined_raw)[1]

#get name variation dataset
isr_var= bm.get_name_var(isr_joined_raw)[1]

#note that mention id is the unique identifier here
isr_final = isr_homonomy.merge(isr_var.loc[:,['mention_id', 'var_count', 'is_var']], on='mention_id')


#subset rows where each record has homonomy record and name variation
isr_noisy = isr_final.loc[(isr_final.homonomy==1)& (isr_final.var_count==0) ,:]


##generation scheme

# high homonomy rate isr benchmark
dfA = isr_noisy.groupby("unique_id").apply(lambda x: x.sample(1, random_state=1)).reset_index(drop=True)
dfA["file"] = "A"


dfB = isr_noisy.groupby("unique_id").apply(lambda x: x.sample(1, random_state=2)).drop_duplicates(subset=[ "name"]).reset_index(drop=True)
dfB["file"] = "B"


combined = pd.concat([dfA, dfB]).reset_index(drop=True)

match_rate = len(dfB.merge(dfA.loc[:,"unique_id"], on='unique_id', how='inner'))/ len(dfB)
print("The match rate is: " + str(match_rate))

homo_real = max(bm.get_homonomy_rate(dfA)[0],bm.get_homonomy_rate(dfB)[0])
print("The homonomy rate is : " + str(homo_real))

print("The name variation rate is: " + str(bm.get_name_var(combined)[0]))

combined.to_csv("../data-raw/isr_benchmark-3.tsv", sep="\t", index=False)

