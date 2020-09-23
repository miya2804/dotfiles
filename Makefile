DOTDIR_NAME := .dotfiles
DOTDIR_PATH := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CANDIDATES  := $(wildcard .??*) bin
EXCLUSIONS  := .git
DOTFILES    := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

.DEFAULT_GOAL := help

.PHONY: list deploy clean help

list: ## Show dotfiles link target to your home of this repo
	@$(foreach val, $(DOTFILES), /bin/ls -dF $(val);)

deploy: ## Create symlink to home directory
	@echo '==> Start to deploy dotfiles to home directory.'
	@$(foreach val, $(DOTFILES), \
		if [ -d $(HOME)/$(val) ]; then \
			mv $(HOME)/$(val) $(HOME)/$(val).bak; \
			ln -nsv --backup=numbered $(abspath $(val)) $(HOME)/$(val); \
		else \
			ln -nsv --backup=numbered $(abspath $(val)) $(HOME)/$(val); \
		fi;)

init:
	@$(foreach val, $(wildcard ./etc/init/*.sh), bash $(val);)

unlink: ## Remove symlink to home directory
	@echo 'Remove dotfile symlinks in your home directory...'
# 	check whether link and remove.
	@-$(foreach val, $(DOTFILES), \
		if [ -L $(HOME)/$(val) ]; then \
			rm -vrf $(HOME)/$(val); \
		fi;)

clean: unlink ## Remove the dot files and this repository
	@echo 'Remove dotfiles repository...'
# 	check whether dotfiles-dir name is ".dotfiles" and remove.
	@-if [ "$(shell basename $(DOTDIR_PATH))" = "$(DOTDIR_NAME)" ]; then rm -rf $(DOTDIR_PATH); fi

help: ## Self-documented Makefile
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
