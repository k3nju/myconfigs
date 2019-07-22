ME:=$(realpath $(lastword $(MAKEFILE_LIST)))
# here is where I reside
HERE:=$(dir $(ME))


.PHONY: all
all: system-wides dotfiles pkg
	echo "all"


# system-wide configs
# note: after making system-wides config, sudo will work.
SYSTEM_WIDES_DIR:=$(HERE)system-wides
SYSTEM_WIDES:=$(shell find $(SYSTEM_WIDES_DIR) -type f -exec realpath {} \;)

define add_archlinuxfr
if ! grep -q archlinuxfr /etc/pacman.conf; then \
	echo "[archlinuxfr]" >> /etc/pacman.conf; \
	echo "SigLevel = Never" >> /etc/pacman.conf; \
	echo 'Server = http://repo.archlinux.fr/$$arch' >> /etc/pacman.conf;
fi

endef

.PHONY: system-wides
system-wides:
	su -c "$(foreach v,$(SYSTEM_WIDES),cp -- $(v) $(subst $(SYSTEM_WIDES_DIR),,$(v));)"
	$(call add_archlinuxfr)

# deploy dotfiles(and other configs)
DOTFILES_DIR:=$(HERE)dotfiles/
DOTFILES:=$(shell find $(DOTFILES_DIR) -type f -exec realpath {} \;)

define link_dotfile
	mkdir -p -- "$(dir $(2))"
	ln -sfv -- "$(1)" "$(2)"

endef

.PHONY: dotfiles
dotfiles:
	$(foreach v,$(DOTFILES),$(call link_dotfile,$(v),$(subst $(DOTFILES_DIR),$(HOME)/,$(v))))


# install and uninstall packages
PKGS_DIR:=$(HERE)pkgs/
CORES:=$(shell cat $(PKGS_DIR)core.txt)
AURS:=$(shell cat $(PKGS_DIR)aurs.txt)
UNINSTALLS:=$(shell cat $(PKGS_DIR)uninstalls.txt)
DESKTOPS:=$(shell cat $(PKGS_DIR)desktops.txt)

.PHONY: pkg
pkg: pkgs_core yay aur uninstalls

.PHONY: pkgs_core
pkgs_core:
	$(foreach v,$(CORES),sudo pacman -Syu --noconfirm -- $(v);)

.PHONY: yay
yay:
	rm -rf /tmp/yay/ && \
	git clone --depth 1 https://aur.archlinux.org/yay.git /tmp/yay/ && \
	cd /tmp/yay/ && yes | makepkg -si

.PHONY: aur
aur:
	$(foreach	v,$(AURS),yay -S --noconfirm -- $(v);)

.PHONY: uninstalls
uninstalls:
	$(foreach v,$(UNINSTALLS),sudo pacman -Rsn --noconfirm -- $(v);)

.PHONY: pkg_desktop
pkg_desktop:
	$(foreach v,$(DESKTOPS),sudo pacman -Syu --noconfirm -- $(v);)


