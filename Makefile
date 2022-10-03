
REBAR = rebar3
BONDY_ERL_NODENAME ?= bondy@127.0.0.1
BONDY_ERL_DISTRIBUTED_COOKIE ?= bondy
CT_SUITE_FILE?=
ifdef CT_SUITE_FILE
CT_SUITE_ARGS = --suite ${CT_SUITE_FILE}
else
CT_SUITE_ARGS =
endif

.PHONY: genvars compile test xref eunit dialyzer tar

certs:
	cd config && ./make_certs

genvars:
	@cp config/prod/default_vars.config config/prod/vars.generated

compile:
	${REBAR} compile

docs: xref
	${REBAR} ex_doc
	cp -r doc/js/* apps/bondy/doc/
	cp -r doc/js/* apps/bondy_broker_bridge/doc/
	mkdir -p apps/bondy/doc/assets/
	mkdir -p apps/bondy_broker_bridge/doc/assets/
	cp -r doc/assets/* apps/bondy/doc/assets/
	cp -r doc/assets/* apps/bondy_broker_bridge/doc/assets/

clean-docs:
	rm -rf apps/bondy/doc/*
	rm -f apps/bondy/doc/.build
	rm -rf apps/bondy_broker_bridge/doc/*
	rm -f apps/bondy_broker_bridge/doc/.build

test: xref
	${REBAR} as test ct ${CT_SUITE_ARGS}

xref:
	${REBAR} xref skip_deps=true


cover: xref
	${REBAR} as test ct ${CT_SUITE_ARGS}, cover

dialyzer:
	${REBAR} dialyzer

tar:
	rm -rf _build/prod
	${REBAR} as prod tar
	mkdir -p _build/tar
	tar -zxvf _build/prod/rel/*/*.tar.gz -C _build/tar

# Notice we need REBAR3_PROFILE en var even if we use 'as prod' because this is
# handled by rebar.conf.script which does not know we have used the 'as prod'
# higher level command
prod-xcomp-rel:
	REBAR3_PROFILE=prod \
	REBAR3_TARGET_INCLUDE_ERTS=/Users/aramallo/otp/24.2/ \
	REBAR3_TARGET_SYSTEM_LIBS=/Users/aramallo/otp/24.2/lib \
	${REBAR} as prod release

# Notice we need REBAR3_PROFILE en var even if we use 'as prod' because this is
# handled by rebar.conf.script which does not know we have used the 'as prod'
# higher level command
prod-xcomp-tar:
	REBAR3_PROFILE=prod \
	REBAR3_TARGET_INCLUDE_ERTS=/Users/aramallo/otp/24.2/ \
	REBAR3_TARGET_SYSTEM_LIBS=/Users/aramallo/otp/24.2/lib \
	${REBAR} as prod tar

devrun:
	${REBAR} as dev release

	cp examples/config/security_config.json _build/dev/rel/bondy/etc/security_config.json

	cp examples/config/api_spec.json _build/dev/rel/bondy/etc/api_spec.json

	cp examples/config/broker_bridge_config.json _build/dev/rel/bondy/etc/broker_bridge_config.json

	_build/dev/rel/bondy/bin/bondy console

prodrun:
	${REBAR} as prod release
	RELX_REPLACE_OS_VARS=true \
	ERL_DIST_PORT=27788 \
	BONDY_ERL_NODENAME=${BONDY_ERL_NODENAME} \
	BONDY_ERL_DISTRIBUTED_COOKIE=${BONDY_ERL_DISTRIBUTED_COOKIE} \
	_build/prod/rel/bondy/bin/bondy console

prodtarrun: tar
	ERL_DIST_PORT=27788 BONDY_ERL_NODENAME=${BONDY_ERL_NODENAME} BONDY_ERL_DISTRIBUTED_COOKIE=${BONDY_ERL_DISTRIBUTED_COOKIE} _build/tar/bin/bondy console


node1:
	${REBAR} as node1 release
	ERL_DIST_PORT=27781 _build/node1/rel/bondy/bin/bondy console

node2:
	${REBAR} as node2 release
	ERL_DIST_PORT=27782 _build/node2/rel/bondy/bin/bondy console

node3:
	${REBAR} as node3 release
	ERL_DIST_PORT=27783 _build/node3/rel/bondy/bin/bondy console


edge1:
	${REBAR} as edge1 release
	ERL_DIST_PORT=27784 _build/edge1/rel/bondy/bin/bondy console


run-node1:
	_build/node1/rel/bondy/bin/bondy console

run-node2:
	_build/node2/rel/bondy/bin/bondy console

run-node3:
	_build/node3/rel/bondy/bin/bondy console

run-edge1:
	_build/edge1/rel/bondy/bin/bondy console


# DOCKER

docker-build-prod:
	docker buildx install
	docker stop bondy-prod || true
	docker rm bondy-prod || true
	docker rmi bondy-prod || true
	docker build \
		--pull \
		--platform linux/amd64 \
		--load \
		-t "bondy-prod" \
		-f deployment/Dockerfile .

docker-build-prod-alpine:
	docker buildx install
	docker stop bondy-prod || true
	docker rm bondy-prod || true
	docker rmi bondy-prod || true
	docker build \
		--pull \
		--platform linux/amd64 \
		--load \
		-t "bondy-prod" \
		-f deployment/alpine.Dockerfile .

docker-run-prod:
	# docker stop bondy-prod || true
	# docker rm bondy-prod || true
	docker run \
		--rm \
		-e BONDY_ERL_NODENAME=bondy1@127.0.0.1 \
		-e BONDY_ERL_DISTRIBUTED_COOKIE=bondy \
		-p 18080:18080 \
		-p 18081:18081 \
		-p 18082:18082 \
		-p 18086:18086 \
		-u 0:1000 \
		-v "$(PWD)/examples/custom_config/etc:/bondy/etc" \
		--name bondy-prod \
		bondy-prod:latest

docker-scan-prod:
	docker scan bondy-prod

