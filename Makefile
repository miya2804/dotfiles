DOTDIR_NAME := .dotfiles
DOTDIR_PATH := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CANDIDATES  := $(wildcard .??*) bin
EXCLUSIONS  := .git
DOTFILES    := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

.DEFAULT_GOAL := help

.PHONY: list deploy init unlink clean help

list: ## Show dotfiles link target to your home of this repo
	@$(foreach val, $(DOTFILES), /bin/ls -dF $(val);)

deploy: ## Create symlink to home directory
	@echo '==> Start to deploy dotfiles to home directory.'
	@$(foreach val, $(DOTFILES), \
		if [ -d $(abspath $(HOME))/$(val) ]; then \
			mv $(abspath $(HOME))/$(val) $(abspath $(HOME))/$(val).bak; \
			ln -nsv --backup=numbered $(abspath $(val)) $(abspath $(HOME))/$(val); \
		else \
			ln -nsv --backup=numbered $(abspath $(val)) $(abspath $(HOME))/$(val); \
		fi;)

init: ## Run initialization scripts (.dotfiles/etc/init/*.sh)
	@read -p "Run initialize scripts? (y/n): " reply; \
	if [ "$$reply" = 'y' ]; then \
		echo 'Initializing...'"\n"; \
		$(foreach val, $(wildcard ./etc/init/*.sh), \
			echo "--- script: $(shell basename $(val)) ---"; \
			bash $(val);) \
	else \
		echo "terminated."; \
	fi

unlink: ## Remove symlink to home directory
	@echo 'Remove dotfile symlinks in your home directory...'
# 	check whether link and remove.
	@-$(foreach val, $(DOTFILES), \
		if [ -L $(abspath $(HOME))/$(val) ]; then \
			rm -vrf $(abspath $(HOME))/$(val); \
		fi;)

clean: unlink ## Remove the dot files and this repository
	@-read -p "Remove dotfiles repository? (y/n): " reply; \
	if [ "$$reply" = 'y' ]; then \
		echo 'Remove dotfiles repository...'; \
		if [ "$(shell basename $(DOTDIR_PATH))" = "$(DOTDIR_NAME)" ]; then \
			rm -rf $(DOTDIR_PATH); \
		fi \
	else \
		echo "terminated."; \
	fi

help: ## Self-documented Makefile
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
