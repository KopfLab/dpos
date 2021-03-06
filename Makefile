PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: docu install check

gui_dev:
	bundle exec guard

vignettes:
	Rscript -e "require(devtools); devtools::build_vignettes()"

docu: vignettes
	rm -f inst/doc/$(PKGNAME)_$(PKGVERS).pdf
	R CMD Rd2pdf --no-preview -o inst/doc/$(PKGNAME)_$(PKGVERS).pdf .

build:
	cd ..;\
	R CMD build --no-manual $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

local-install:
	rm -rf .local
	mkdir .local
	R CMD Install --library=.local .

autotest: local-install
	R -q -e "library($(PKGNAME), lib.loc = '.local')" \
       -e "library(testthat)" \
       -e "auto_test_package(pkg='.')"
