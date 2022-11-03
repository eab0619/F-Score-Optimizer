import pandas as pd
import benchmark_metrics as bm
import random
import string


als_joined_raw = pd.read_csv('../data-raw/als_joined_raw.csv').iloc[:, 1:]

als_joined_raw = als_joined_raw.dropna(subset=['name_first','name_last',])

#create als benchmark datasets
als_var= bm.get_name_var(als_joined_raw)[1]
als_homonomy = bm.get_homonomy_rate(als_joined_raw)[1]

#note that mention id is unique here!!!
als_final = als_homonomy.merge(als_var.loc[:,['mention_id', 'var_count', 'is_var']], on='mention_id')


als_simple = als_final[als_final.homonomy==1].drop_duplicates(subset=['name', 'unique_id', 'city'])

als_random = als_final[(als_final.homonomy==0) & (als_final.is_var==0)].drop_duplicates(subset=['unique_id'])



#als benchmark 1
random.seed(20)
dfA = als_simple.groupby("unique_id").apply(lambda x: x.sample(1, random_state=1)).reset_index(drop=True)
dfA["file"] = "A"

dfB = als_simple.groupby("unique_id").apply(lambda x: x.sample(1, random_state=2)).reset_index(drop=True).drop_duplicates(subset=['name'])
dfB["file"] = "B"

#distort city
for i in dfA.index:
    u = random.uniform(0,1)
    if u <0.3:
        dfA.iloc[[i]] = dfA.iloc[[i]*1].assign(city=''.join(random.choice(string.ascii_lowercase) for i in range(10)))

add_non_match_A  = als_random.head(1000).copy()
add_non_match_A["file"] = "A"
add_non_match_B = als_random.tail(500).copy()
add_non_match_B["file"] = "B"

combined = pd.concat([dfA, add_non_match_A, dfB, add_non_match_B]).reset_index(drop=True)
#match rate
match_rate = len(dfB.merge(dfA.loc[:,"unique_id"], on='unique_id', how='inner'))/ len(combined.loc[combined.file=='B'])
print("The match rate is: " + str(match_rate))

#actual homonomy rate
homo_real = max(bm.get_homonomy_rate(dfA)[0],bm.get_homonomy_rate(dfB)[0])

print("The homonomy rate is : " + str(homo_real))

#actual name variation rate
print("The name variation rate is: " + str(bm.get_name_var(combined)[0]))


combined.to_csv("../data-raw/als_benchmark-1.tsv", sep="\t",index=False)