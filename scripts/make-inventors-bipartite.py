#!/usr/bin/env python3

import pandas as pd
from pv_evaluation.benchmark import load_patentsview_inventors_benchmark

data = load_patentsview_inventors_benchmark().reset_index()

dfA = data.groupby("unique_id").apply(lambda x: x.sample(1, random_state=1))
dfA["file"] = "A"

dfB = data.groupby("unique_id").apply(lambda x: x.sample(1, random_state=2))
dfB["file"] = "B"

combined = pd.concat([dfA, dfB]).reset_index(drop=True)[["mention_id", "unique_id", "file"]]

rawinventor = pd.read_csv("data-raw/rawinventor.tsv", sep="\t", dtype=str)
rawlocation = pd.read_csv("data-raw/rawlocation.tsv", sep="\t", dtype=str)

rawinventor["mention_id"] = "US" + rawinventor.patent_id + "-" + rawinventor.sequence
joined = (
    combined
    .merge(rawinventor, on="mention_id", how="left")
    .merge(rawlocation, left_on="rawlocation_id", right_on="id", how="left")
)
joined.to_csv("data-raw/pv-bipartite.tsv", sep="\t")
