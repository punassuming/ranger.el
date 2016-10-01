# Copyright (c) 2015-2016 Sebastian Wiesner <swiesner@lunaryorn.com>

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# These variables may be overridden by the user
TEXINFO_VERSION ?= 6.1
EMACS_VERSION ?= 24.5
VERBOSE ?= no
# Build a minimal Emacs with no special flags, to build as fast as possible
EMACSCONFFLAGS ?= --with-x-toolkit=no --without-x --without-all --with-xml2 \
	CFLAGS='-O2 -march=native' CXXFLAGS='-O2 -march=native'

ifeq ($(VERBOSE),yes)
SILENT=
else
SILENT=> /dev/null
endif

# Tear the version apart
VERSION_PARTS = $(subst -, ,$(EMACS_VERSION))
VERSION_PART = $(word 1,$(VERSION_PARTS))
PRE_RELEASE_PART = $(word 2,$(VERSION_PARTS))
# Whether the version is a release candidate
IS_RC = $(findstring rc,$(PRE_RELEASE_PART))

# Clone Emacs from the Github mirror because it's way faster than upstream
EMACS_GIT_URL = https://github.com/emacs-mirror/emacs.git
# Emacs FTP URL.  Prereleases are on alpha.gnu.org
ifeq ($(PRE_RELEASE_PART),)
EMACS_FTP_URL = "https://ftp.gnu.org/gnu/emacs"
else
EMACS_FTP_URL = "http://alpha.gnu.org/pub/gnu/emacs/pretest"
endif
# URL of the TAR file
EMACS_TAR_URL = $(EMACS_FTP_URL)/emacs-$(EMACS_VERSION).tar.xz

# If it's an RC the real reported Emacs version is the version without the
# prerelease postfix.  Otherwise it's just the version that we get.
ifneq ($(IS_RC),)
REPORTED_EMACS_VERSION = $(VERSION_PART)
else
REPORTED_EMACS_VERSION = $(EMACS_VERSION)
endif

# Tell recipe processes about the reported Emacs version
export REPORTED_EMACS_VERSION

.PHONY: download_emacs_stable clone_emacs_snapshot
.PHONY: install_emacs compile

download_emacs_stable:
	@echo "Download Emacs $(EMACS_VERSION) from $(EMACS_TAR_URL)"
	@curl -o "/tmp/emacs-$(EMACS_VERSION).tar.xz" "$(EMACS_TAR_URL)"
	@tar xf "/tmp/emacs-$(EMACS_VERSION).tar.xz" -C /tmp
	@mv /tmp/emacs-$(REPORTED_EMACS_VERSION) /tmp/emacs

clone_emacs_snapshot:
	@echo "Clone Emacs from Git"
	git clone --depth=1 '$(EMACS_GIT_URL)' /tmp/emacs
	# Create configure
	cd /tmp/emacs && ./autogen.sh

install_emacs:
	@echo "Install Emacs $(EMACS_VERSION)"
	@cd '/tmp/emacs' && ./configure --quiet --enable-silent-rules \
		--prefix="$(HOME)" $(EMACSCONFFLAGS) $(SILENT)
	@make -j2 -C '/tmp/emacs' V=0 install $(SILENT)

ifeq ($(EMACS_VERSION),snapshot)
install_emacs: clone_emacs_snapshot
else
install_emacs: download_emacs_stable
endif

compile:
	emacs -Q --batch -L . -f batch-byte-compile ranger.el
