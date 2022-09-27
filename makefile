## Development utilities
##
## Usage:
## 		make <target> [<arg>=<value> ...]
##
## Targets:
## 		help:		Show this help message.
##		env: 		Create or update conda environment "pv-evaluation"
ENV?=f-score-optimizer

.PHONY: help env

help: makefile
	@sed -n "s/^##//p" $<

env: environment.yml
	@(echo "Creating ${ENV} environment..."; conda env create -f $<) \
	|| (echo "Updating ${ENV} environment...\n"; conda env update -f $<)

data: data-raw/rawinventor.tsv\
	data-raw/rawlocation.tsv\
	data-raw/patent.tsv\
	data-raw/pv-bipartite.tsv\
	data-raw/RLdata_bipartite.RData
	
data-raw/%.tsv:
	wget -O $(@F).zip https://s3.amazonaws.com/data.patentsview.org/download/$(@F).zip
	unzip -o -q $(@F).zip -d $(@D)
	rm $(@F).zip

data-raw/pv-bipartite.tsv: scripts/make-inventors-bipartite.py
	conda run -n ${ENV} python3 $<

data-raw/RLdata_bipartite.RData: scripts/make-rldata-bipartite.R
	Rscript --vanilla $<
