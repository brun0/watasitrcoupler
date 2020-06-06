npm=$(shell which npm 2> /dev/null)

all: build

.PHONY: build
build:
ifneq (,$(wildcard $(CURDIR)/package.json))
	make npm
endif

.PHONY: dev
dev:
ifneq (,$(wildcard $(CURDIR)/package.json))
	make npm-dev
endif

.PHONY: npm
npm:
	$(npm) install
	$(npm) run build

.PHONY: npm-dev
npm-dev:
	$(npm) install
	$(npm) run dev

