## Development utilities
##
## Usage:
## 		make <target> [<arg>=<value> ...]
##
## Targets:
## 		help:		Show this help message.

.PHONY: help env

help: makefile
	@sed -n "s/^##//p" $<
