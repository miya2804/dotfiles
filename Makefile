DOTDIR_NAME := .dotfiles
DOTDIR_PATH := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CANDIDATES  := $(wildcard .??*) bin
EXCLUSIONS  := .git
DOTFILES    := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

SPECIALS  := fish
FISH_ROOT := $(HOME)/.config/fish

.DEFAULT_GOAL := help


# GENERAL
.PHONY: list init help

list: ## Show dotfiles link target to your home of this repo
	@-$(foreach val, $(DOTFILES), /bin/ls -dF $(val);)
	@$(foreach val, $(SPECIALS), /bin/ls -dF $(val);)

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

help: ## Self-documented Makefile
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'


# DEPLOY
.PHONY: deploy default_deploy specials_deploy fish

deploy: default_deploy specials_deploy ## Create symlink of all targets

default_deploy: ## Create symlink of general dotfiles to home directory
	@echo 'Dotfiles depoloying...'
	@$(foreach val, $(DOTFILES), \
		if [ -d $(abspath $(HOME))/$(val) ]; then \
			mv $(abspath $(HOME))/$(val) $(abspath $(HOME))/$(val).bak; \
			ln -nsv --backup=numbered $(abspath $(val)) $(abspath $(HOME))/$(val); \
		else \
			ln -nsv --backup=numbered $(abspath $(val)) $(abspath $(HOME))/$(val); \
		fi;)

specials_deploy: $(SPECIALS) ## Create symlink in the corresponding location

fish:
	@echo 'Fish config file deploying...'
	@-mkdir -pv $(FISH_ROOT)/conf.d $(FISH_ROOT)/functions
	@for src in $$(find $(DOTDIR_PATH)/$@ -type f); do \
		ln -nsv --backup=numbered $$src $(FISH_ROOT)/$$(realpath --relative-to $(DOTDIR_PATH)/$@ $$src);\
	done


# CLEAN
.PHONY: clean unlink default_unlink specials_unlink

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

unlink: default_unlink specials_unlink fish_unlink ## Remove symlink of all targets

default_unlink: ## Remove symlink of general dotfiles from home directory
	@echo 'Removing symlinks of dotfiles in your home directory...'
# 	check whether link and remove.
	@-$(foreach val, $(DOTFILES), \
		if [ -L $(abspath $(HOME))/$(val) ]; then \
			rm -vrf $(abspath $(HOME))/$(val); \
		fi;)

specials_unlink: $(SPECIALS)_unlink ## Remove symlink in the corresponding location

fish_unlink:
	@echo 'Removing symlinks of fish config file...'
	@for src in $$(find $(DOTDIR_PATH)/fish -type f); do \
		if [ -L $(FISH_ROOT)/$$(realpath --relative-to $(DOTDIR_PATH)/fish $$src) ]; then \
			rm -vrf $(FISH_ROOT)/$$(realpath --relative-to $(DOTDIR_PATH)/fish $$src); \
		fi; \
	done
