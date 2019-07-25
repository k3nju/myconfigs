ME:=$(realpath $(lastword $(MAKEFILE_LIST)))
# here is where I reside
HERE:=$(dir $(ME))


.PHONY: all
all: init dotfiles pkg


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

.PHONY: init
init:
	su -c "echo 'kj ALL=(ALL) NOPASSWD: ALL' > /etc/sudoers.d/kj"
	$(call add_archlinuxfr,/etc/pacman.conf)
	$(call create_emacs_launcher,/usr/local/bin/emacs_launcher.sh)

CONFIGS_DIR:=$(HERE)configs/
CONFIGS:=$(shell find $(CONFIGS_DIR) -type f -exec realpath {} \;)
.PHONY: configs
configs:
	$(foreach v,$(CONFIGS),sudo cp -- $(v) $(subst $(CONFIGS_DIR),/,$(v)))


# deploy dotfiles(and other configs)
define link_dotfile
	mkdir -p -- "$(dir $(2))"
	ln -sf -- "$(1)" "$(2)"

endef

DOTFILES_DIR:=$(HERE)dotfiles/
DOTFILES:=$(shell find $(DOTFILES_DIR) -type f -exec realpath {} \;)
.PHONY: dotfiles
dotfiles:
	$(foreach v,$(DOTFILES),$(call link_dotfile,$(v),$(subst $(DOTFILES_DIR),$(HOME)/,$(v))))


# install and uninstall packages
INSTALLEDS:=$(shell pacman -Q|awk '{print $$1}')
PKGS_DIR:=$(HERE)pkgs/
.PHONY: pkg
pkg: pkg_update pkg_core pkg_desktop pkg_yay pkg_aur pkg_uninstalls

.PHONY: pkg_update
pkg_update:
	sudo pacman -Syu --noconfirm

CORES:=$(filter-out $(INSTALLEDS),$(shell cat $(PKGS_DIR)core.txt))
.PHONY: pkg_core
pkg_core:
	$(foreach v,$(CORES),sudo pacman -Syu --noconfirm -- $(v);)

.PHONY: pkg_yay
pkg_yay:
	rm -rf /tmp/yay/ && \
	git clone --depth 1 https://aur.archlinux.org/yay.git /tmp/yay/ && \
	cd /tmp/yay/ && yes | makepkg -si

AURS:=$(filter-out $(INSTALLEDS),$(shell cat $(PKGS_DIR)aurs.txt))
.PHONY: pkg_aur
pkg_aur:
	$(foreach	v,$(AURS),yay -S --noconfirm -- $(v);)

UNINSTALLS:=$(filter $(INSTALLEDS),$(shell cat $(PKGS_DIR)uninstalls.txt))
.PHONY: pkg_uninstalls
pkg_uninstalls:
	$(foreach v,$(UNINSTALLS),sudo pacman -Rsn --noconfirm -- $(v);)

DESKTOPS:=$(filter-out $(INSTALLEDS),$(shell cat $(PKGS_DIR)desktops.txt))
.PHONY: pkg_desktop
pkg_desktop:
	$(foreach v,$(DESKTOPS),sudo pacman -Syu --noconfirm -- $(v);)

