DOTDIR_NAME := .dotfiles
DOTDIR_PATH := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CANDIDATES  := $(wildcard .??*) #bin
EXCLUSIONS  := .git
DOTFILES    := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

.DEFAULT_GOAL := help

.PHONY: list deploy clean help

list: ## Show dotfiles link target to your home of this repo
	@$(foreach val, $(DOTFILES), /bin/ls -dF $(val);)

deploy: ## Create symlink to home directory
	@echo '==> Start to deploy dotfiles to home directory.'
	@$(foreach val, $(DOTFILES), ln -nsv --backup=numbered $(abspath $(val)) $(HOME)/$(val);)

init:
	@$(foreach val, $(wildcard ./etc/init/*.sh), bash $(val);)

unlink: ## Remove symlink to home directory
	@echo 'Remove dotfile symlinks in your home directory...'
	@-$(foreach val, $(DOTFILES), if [ -L $(HOME)/$(val) ]; then rm -vrf $(HOME)/$(val); fi;) # check whether link and remove.

clean: unlink ## Remove the dot files and this repository
	@echo 'Remove dotfiles repository...'
	@-if [ "$(shell basename $(DOTDIR_PATH))" = "$(DOTDIR_NAME)" ]; then rm -rf $(DOTDIR_PATH); fi # check whether dotfiles-dir name is ".dotfiles" and remove.

help: ## Self-documented Makefile
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
