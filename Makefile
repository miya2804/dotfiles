DOTDIR_NAME := .dotfiles
DOTDIR_PATH := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CANDIDATES  := $(wildcard .??*) bin
EXCLUSIONS  := .git
DOTFILES    := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

SCRIPT_DIR := $(DOTDIR_PATH)/etc/script/make
SPECIALS  := fish

.DEFAULT_GOAL := help


# GENERAL
.PHONY: list init help

list: ## Show dotfiles link target to your home of this repo
	@-$(foreach val, $(DOTFILES), /bin/ls -dF $(val);)
	@$(foreach val, $(SPECIALS), /bin/ls -dF $(val);)

init: ## Run initialization scripts
	@echo
	@$(SCRIPT_DIR)/initialize.sh

help: ## Self-documented Makefile
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'


# DEPLOY
.PHONY: deploy default_deploy specials_deploy fish_deploy

deploy: default_deploy specials_deploy ## Create symlink of all targets

default_deploy: default_unlink ## Create symlink of general dotfiles to home directory
	@echo
	@echo '--- Dotfiles deploy ---'
	@$(foreach val, $(DOTFILES), \
		if [ -d $(abspath $(HOME))/$(val) ]; then \
			mv $(abspath $(HOME))/$(val) $(abspath $(HOME))/$(val).bak; \
		fi; \
		ln -ns --backup=numbered $(abspath $(val)) $(abspath $(HOME))/$(val) \
			&& echo "symlink created '$(abspath $(HOME))/$(val)'";)
	@echo
	@printf " \033[37;1m✔ Dotfiles deploy\033[m...\033[32mOK\033[m\n"

specials_deploy: $(SPECIALS)_deploy ## Create symlink in the corresponding location

fish_deploy: fish_unlink
	@echo
	@$(SCRIPT_DIR)/fish_config.sh --deploy


# CLEAN
.PHONY: clean unlink default_unlink specials_unlink fish_unlink

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

unlink: default_unlink specials_unlink ## Remove symlink of all targets

default_unlink: ## Remove symlink of general dotfiles from home directory
	@echo
	@echo '--- Remove symlinks of dotfiles in your home directory ---'
	@-$(foreach val, $(DOTFILES), \
		if [ -L $(abspath $(HOME))/$(val) ]; then \
			rm -vrf $(abspath $(HOME))/$(val); \
		fi;)
	@echo
	@printf " \033[37;1m✔ Remove symlinks of dotfiles in your home directory\033[m...\033[32mOK\033[m\n"

specials_unlink: $(SPECIALS)_unlink ## Remove symlink in the corresponding location

fish_unlink:
	@echo
	@$(SCRIPT_DIR)/fish_config.sh --unlink
