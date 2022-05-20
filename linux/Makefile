me:=$(realpath $(lastword $(MAKEFILE_LIST)))
# here is where I reside and the root directory to work
here:=$(dir $(me))

#[tab]pid=`lsof -i:8000|tail -n 1|awk '{print $$2}'`; [[ ! -z "$$pid" ]] && kill $$pid; true


#------------------------------------------------------------------------------#
# env

# default target
.PHONY: env_all
env_all: init pkg_all dotfiles configs

# for console only
.PHONY: env_min
env_min: init pkg_min dotfiles configs

# for minimal desktop
.PHONY: env_min_desktop
env_min_desktop: init pkg_min_desktop dotfiles configs


#------------------------------------------------------------------------------#
# helpers

.PHONY: help
help:
	@grep .PHONY $(MAKEFILE_LIST) | grep -v grep | awk '{print $$2}'


#------------------------------------------------------------------------------#
# init

# initialization. configure high priorities	first
define add_archlinuxfr
if ! grep -q archlinuxfr $(1); then \
	echo '[archlinuxfr]' | sudo tee -a $(1); \
	echo 'SigLevel = Never' | sudo tee -a $(1); \
	echo 'Server = http://repo.archlinux.fr/$$arch' | sudo tee -a $(1); \
fi

endef

define create_emacs_launcher
if [[ ! -e $(1) ]]; then \
	echo '#! /usr/bin/bash' | sudo tee -a $(1); \
	echo 'if [[ ! -z $${DISPLAY} ]]; then' | sudo tee -a $(1); \
	echo '/usr/bin/emacs $$@ &' | sudo tee -a $(1); \
	echo 'else' | sudo tee -a $(1); \
	echo '/usr/bin/emacs -nw $$@' | sudo tee -a $(1); \
	echo 'fi' | sudo tee -a $(1); \
	sudo chmod a+x $(1); \
fi

endef

define ensure_su
if [[ ! -f /usr/bin/su ]]; then \
	echo "install su first" && \
	exit 1; \
fi

endef

# XXX: removed --needed when installing archlinux-keyring.
#      even if archlinux-keyring is up-to-date,
#      some package installations fail because of key import error.
#      but reinstalling archlinux-keyring works.
.PHONY: init
init:
	$(call ensure_su)
	su -c "pacman -Sy && pacman -S --noconfirm archlinux-keyring && pacman -S --needed --noconfirm base-devel && echo 'kj ALL=(ALL) NOPASSWD: ALL' > /etc/sudoers.d/kj"
	$(call add_archlinuxfr,/etc/pacman.conf)
	$(call create_emacs_launcher,/usr/local/bin/emacs_launcher.sh)


#------------------------------------------------------------------------------#
# configs

CONFIGS_DIR?=$(here)configs/
configs:=$(shell find $(CONFIGS_DIR) -type f -exec realpath {} \;)
.PHONY: configs
configs:
	$(foreach v,$(configs),sudo install -D -- $(v) $(subst $(CONFIGS_DIR),/,$(v));)


#------------------------------------------------------------------------------#
# dotfiles

# deploy dotfiles(and other configs)
define link_dotfile
mkdir -p -- "$(dir $(2))"
ln -sf -- "$(1)" "$(2)"

endef

DOTFILES_DIR?=$(here)dotfiles/
dotfiles:=$(shell find $(DOTFILES_DIR) -type f -exec realpath {} \;)
.PHONY: dotfiles
dotfiles:
	$(foreach v,$(dotfiles),$(call link_dotfile,$(v),$(subst $(DOTFILES_DIR),$(HOME)/,$(v))))


#------------------------------------------------------------------------------#
# pkgs

# pkg_all: everything
# pkg_min: minimal packages
# pkg_min_desktop: minimal desktop packages
#
# pkg_core: the essentials
# pkg_system: CUI home use
# pkg_desktop: minimal GUI
# pkg_yay: install yay
# pkg_aur: install from aur repo
# pkg_app: home use applications
# pkg_uninstall: uninstall unnecessaries

# dependencies
# pkg_core
# |-< pkg_system
# |-< pkg_desktop
#   |-< pkg_app
# |-< pkg_yay
#   |-< pkg_aur

.PHONY: pkg_all
pkg_all: pkg_update pkg_core pkg_system pkg_desktop pkg_app pkg_aur pkg_uninstall

.PHONY: pkg_min
pkg_min: pkg_update pkg_core pkg_uninstall

.PHONY: pkg_min_desktop
pkg_min_desktop: pkg_update pkg_desktop pkg_uninstall


# install and uninstall packages
installeds:=$(shell pacman -Q|awk '{print $$1}')
installeds+=$(shell pacman -Qg|awk '{print $$1}')
pkgs_dir:=$(here)pkgs/


.PHONY: pkg_update
pkg_update:
	sudo pacman -Syu --noconfirm


PKG_CORE?=$(filter-out $(installeds),$(shell cat $(pkgs_dir)core.txt))
.PHONY: pkg_core
pkg_core:
	$(foreach v,$(PKG_CORE),sudo pacman -Su --needed --noconfirm -- $(v);)


PKG_SYSTEM?=$(filter-out $(installeds),$(shell cat $(pkgs_dir)system.txt))
.PHONY: pkg_system
pkg_system: pkg_core
	$(foreach v,$(PKG_SYSTEM),sudo pacman -Su --needed --noconfirm -- $(v);)


PKG_DESKTOP?=$(filter-out $(installeds),$(shell cat $(pkgs_dir)desktop.txt))
.PHONY: pkg_desktop
pkg_desktop: pkg_core
	$(foreach v,$(PKG_DESKTOP),sudo pacman -Su --needed --noconfirm -- $(v);)


define install_yay
if ! pacman -Qs yay; then \
	rm -rf /tmp/yay/ && \
	git clone --depth 1 https://aur.archlinux.org/yay.git /tmp/yay/ && \
	cd /tmp/yay/ && \
	yes | makepkg -si; \
fi

endef

.PHONY: pkg_yay
pkg_yay: pkg_core
	$(call install_yay)

PKG_AUR?=$(filter-out $(installeds),$(shell cat $(pkgs_dir)aur.txt))
.PHONY: pkg_aur
pkg_aur: pkg_core pkg_yay
	$(foreach	v,$(PKG_AUR),yay -Su --needed --noconfirm -- $(v);)


PKG_APP?:=$(filter-out $(installeds),$(shell cat $(pkgs_dir)app.txt))
.PHONY: pkg_app
pkg_app: pkg_desktop
	$(foreach v,$(PKG_APP),sudo pacman -Su --needed --noconfirm -- $(v);)


PKG_UNINSTALL?=$(filter $(installeds),$(shell cat $(pkgs_dir)uninstall.txt))
.PHONY: pkg_uninstall
pkg_uninstall:
	$(foreach v,$(PKG_UNINSTALL),sudo pacman -Rsn --noconfirm -- $(v);)


