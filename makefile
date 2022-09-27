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
