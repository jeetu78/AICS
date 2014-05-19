REBAR=./rebar
RELX=./relx
DIALYZER=$(shell which dialyzer)
ifeq ($(DIALYZER),)
 $(error "Dialyzer not available on this system")
endif

DEPSOLVER_PLT=./.plt

all: deps compile

deps:
	@$(REBAR) get-deps

compile: deps
	@$(REBAR) compile -r -v

$(DEPSOLVER_PLT):
	@$(DIALYZER) --output_plt $(DEPSOLVER_PLT) --build_plt \
		--apps erts kernel stdlib crypto public_key

dialyze: $(DEPSOLVER_PLT) compile
	@$(DIALYZER) --plt $(DEPSOLVER_PLT) --src apps/*/src -I deps/ \
		-Wunmatched_returns -Werror_handling -Wrace_conditions -Wno_undefined_callbacks

test: compile
	@$(REBAR) -r -v -DTEST eunit skip_deps=true verbose=0
	ct_run -dir apps/*/itest -pa ebin -verbosity 0 -logdir .ct/logs -erl_args +K true +A 10

doc: compile
	@$(REBAR) -r doc skip_deps=true

validate: dialyze test

release: clean validate
	@$(RELX) release tar

rel: compile
	@$(RELX) release tar

relup: clean validate
	@$(RELX) release relup tar

clean:
	@$(REBAR) -r clean

distclean: clean
	rm -rvf ./_rel
	# rm -rvf ./deps/*

ciclean: distclean
	rm $(DEPSOLVER_PLT)
	rm -rvf ./deps/*

.PHONY: all deps compile dialyze test doc validate release relup clean distclean ciclean
