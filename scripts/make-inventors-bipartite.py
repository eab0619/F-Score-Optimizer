#!/usr/bin/env python3

from curses import raw
import pandas as pd
import os
import shutil
import wget
import zipfile
from pv_evaluation.benchmark import load_patentsview_inventors_benchmark

data = load_patentsview_inventors_benchmark().reset_index()

dfA = data.groupby("unique_id").apply(lambda x: x.sample(1, random_state=1))
dfA["file"] = "A"

dfB = data.groupby("unique_id").apply(lambda x: x.sample(1, random_state=2))
dfB["file"] = "B"

combined = pd.concat([dfA, dfB]).reset_index(drop=True)[["mention_id", "unique_id", "file"]]

if not os.path.isfile("rawinventor.tsv.zip"):
    wget.download("https://s3.amazonaws.com/data.patentsview.org/download/rawinventor.tsv.zip")
if not os.path.isfile("rawinventor.tsv"):
    with zipfile.ZipFile("rawinventor.tsv.zip","r") as zip_ref:
        zip_ref.extractall(".")

rawinventor = pd.read_csv("rawinventor.tsv", sep="\t", dtype=str)
rawinventor["mention_id"] = "US" + rawinventor.patent_id + "-" + rawinventor.sequence
joined = combined.merge(rawinventor, on="mention_id", how="left")
joined.to_csv("../data-raw/pv-bipartite.tsv", sep="\t")

os.remove("rawinventor.tsv")
os.remove("rawinventor.tsv.zip")