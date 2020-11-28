.PHONY: default
default: image

ifeq ($(origin stack_args),undefined)
export stack_args := --docker --docker-stack-exe download --no-nix
endif

ifeq (${TERM},dumb)
export stack_args := ${stack_args} --colour=never
else
export stack_args := ${stack_args} --colour=auto
endif

srcfiles = $(shell find src -type f)
test-srcfiles = $(shell find test-suite -type f)

tests = dist/tests/nejlacommon-tutorial-test-suite


image: build Dockerfile
	docker build -t 'nejlacommon-tutorial' .
	touch image

.PHONY: build
build: dist/nejlacommon-tutorial $(tests) dist/swagger.json


dist/nejlacommon-tutorial: $(srcfiles) $(test-srcfiles) package.yaml stack.yaml
	rm -f *.cabal
	rm -f stack.yaml.lock
	mkdir -p ./dist
	stack build --install-ghc \
	      --test --no-run-tests \
	      ${stack_args} \
	      ${stack_build_args} \
	      --copy-bins --local-bin-path ./dist

# Targets for test suites.
$(tests): dist/tests/% : dist/nejlacommon-tutorial $(test-srcfiles) package.yaml stack.yaml
	mkdir -p dist/tests
	cp -f "$(shell stack ${stack_args} path --dist-dir)/build/$(notdir $@)/$(notdir $@)" dist/tests/


dist/swagger.json: dist/nejlacommon-tutorial
	touch  dist/swagger.json
 # @TODO
 # stack run ${stack_args} -- swagger dist/swagger.json


.PHONY: up
up: image
	docker-compose -p nejlacommon-tutorial up

# Tests
#######################

.PHONY: test
test: $(tests)
	docker-compose -f docker-compose.testing.yaml run --rm test
	docker-compose -f docker-compose.testing.yaml down -v

.PHONY: test-down
test-down:
	docker-compose -f docker-compose.testing.yaml down -v

# Cleaning
#######################

.PHONY: clean
clean:
	docker-compose -p nejlacommon-tutorial down -v
	docker-compose -f docker-compose.testing.yaml down -v
	docker image prune -fa --filter="label=project=nejlacommon-tutorial"
	stack clean
	rm -f stack.yaml.lock
	rm -rf dist
	rm -f *.cabal
	rm -f image

.PHONY: distclean
distclean: clean
	rm -rf .stack-work
