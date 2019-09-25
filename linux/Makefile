me:=$(realpath $(lastword $(MAKEFILE_LIST)))
# here is where I reside and the root directory to work
here:=$(dir $(me))


.PHONY: all
all: init pkg dotfiles configs


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

#------------------------------------------------------------------------------#
# init

.PHONY: init
init:
	su -c "echo 'kj ALL=(ALL) NOPASSWD: ALL' > /etc/sudoers.d/kj"
	$(call add_archlinuxfr,/etc/pacman.conf)
	$(call create_emacs_launcher,/usr/local/bin/emacs_launcher.sh)


#------------------------------------------------------------------------------#
# configs

CONFIGS_DIR?=$(here)configs/
configs:=$(shell find $(CONFIGS_DIR) -type f -exec realpath {} \;)
.PHONY: configs
configs:
	$(foreach v,$(configs),sudo cp -- $(v) $(subst $(CONFIGS_DIR),/,$(v)))


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
	bash -x $(DOTFILES_DIR)/reset-ratslap.sh


#------------------------------------------------------------------------------#
# pkgs

# install and uninstall packages
installeds:=$(shell pacman -Q|awk '{print $$1}')
installeds+=$(shell pacman -Qg|awk '{print $$1}')
pkgs_dir:=$(here)pkgs/
.PHONY: pkg
pkg: pkg_update pkg_core pkg_desktop pkg_yay pkg_aur pkg_uninstalls

.PHONY: pkg_update
pkg_update:
	sudo pacman -Syu --noconfirm

CORES?=$(filter-out $(installeds),$(shell cat $(pkgs_dir)core.txt))
.PHONY: pkg_core
pkg_core:
	$(foreach v,$(CORES),sudo pacman -Syu --noconfirm -- $(v);)

define install_yay
if ! pacman -Qs yay; then \
	rm -rf /tmp/yay/ && \
	git clone --depth 1 https://aur.archlinux.org/yay.git /tmp/yay/ && \
	cd /tmp/yay/ && \
	yes | makepkg -si; \
fi

endef

.PHONY: pkg_yay
pkg_yay:
	$(call install_yay)

AURS?=$(filter-out $(installeds),$(shell cat $(pkgs_dir)aurs.txt))
.PHONY: pkg_aur
pkg_aur:
	$(foreach	v,$(AURS),yay -S --noconfirm -- $(v);)

UNINSTALLS?=$(filter $(installeds),$(shell cat $(pkgs_dir)uninstalls.txt))
.PHONY: pkg_uninstalls
pkg_uninstalls:
	$(foreach v,$(UNINSTALLS),sudo pacman -Rsn --noconfirm -- $(v);)

DESKTOPS?=$(filter-out $(installeds),$(shell cat $(pkgs_dir)desktops.txt))
.PHONY: pkg_desktop
pkg_desktop:
	$(foreach v,$(DESKTOPS),sudo pacman -Syu --noconfirm -- $(v);)


