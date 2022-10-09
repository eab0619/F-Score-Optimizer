import pandas as pd
import pv_evaluation.benchmark
import os
import string
import random

#read in raw als data
als_data =  pv_evaluation.benchmark.load_als_inventors_benchmark().reset_index()

#read in raw inventor and raw location
rawinventor = pd.read_csv("~/Downloads/rawinventor.tsv", sep="\t", dtype=str)
rawinventor["mention_id"] = "US" + rawinventor.patent_id + "-" + rawinventor.sequence
rawlocation = pd.read_csv("~/Downloads/rawlocation.tsv", sep="\t", dtype=str, on_bad_lines='skip', engine='python')

#create joined data for als
als_joined_raw = (
     als_data
     .merge(rawinventor, on="mention_id", how="left")
     .merge(rawlocation, left_on="rawlocation_id", right_on="id", how="left")
 )

#write to directory
als_joined_raw.to_csv("/Users/ericbai/Library/Mobile Documents/com~apple~CloudDocs/Duke_MSS/Research/F-Score-Optimizer/data-raw/als_joined_raw.csv")