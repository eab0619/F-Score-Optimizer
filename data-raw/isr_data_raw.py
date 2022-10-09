import pandas as pd
import pv_evaluation.benchmark
import os
import string
import random

#read in raw Israeli data
isr_data = pv_evaluation.benchmark.load_israeli_inventors_benchmark().reset_index()

#read in raw inventor and raw location
rawinventor = pd.read_csv("~/Downloads/rawinventor.tsv", sep="\t", dtype=str)
rawinventor["mention_id"] = "US" + rawinventor.patent_id + "-" + rawinventor.sequence
rawlocation = pd.read_csv("~/Downloads/rawlocation.tsv", sep="\t", dtype=str, on_bad_lines='skip', engine='python')

#create joined data for als
isr_joined_raw = (
     isr_data
     .merge(rawinventor, on="mention_id", how="left")
     .merge(rawlocation, left_on="rawlocation_id", right_on="id", how="left")
 )

#write to directory
is_joined_raw.to_csv("/Users/ericbai/Library/Mobile Documents/com~apple~CloudDocs/Duke_MSS/Research/F-Score-Optimizer/data-raw/isr_joined_raw.csv")