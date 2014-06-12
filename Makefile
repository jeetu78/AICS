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
	@$(REBAR) compile

$(DEPSOLVER_PLT):
	@$(DIALYZER) --output_plt $(DEPSOLVER_PLT) --build_plt \
		--apps erts kernel stdlib crypto public_key

dialyze: $(DEPSOLVER_PLT) compile
	@$(DIALYZER) --plt $(DEPSOLVER_PLT) --src apps/*/src -I apps/ -I deps/ \
		-Wunmatched_returns -Werror_handling -Wrace_conditions -Wno_undefined_callbacks

test: compile
	@$(REBAR) -v  eunit skip_deps=true verbose=0
	ct_run -dir apps/*/itest -pa ebin -verbosity 0 -logdir .ct/logs -erl_args +K true +A 10

doc: compile
	@$(REBAR) doc skip_deps=true

validate: dialyze test

release: clean validate
	@$(RELX) -c ./ea_aics_relx.config release tar
	@$(RELX) -c ./ea_cs_relx.config release tar

rel: compile
	@$(RELX) -c ./ea_aics_relx.config release tar
	@$(RELX) -c ./ea_cs_relx.config release tar

relup: clean validate
	@$(RELX) release relup tar

clean:
	@$(REBAR) clean

distclean: clean
	rm -rvf ./_rel*
	# rm -rvf ./deps/*

ciclean: distclean
	rm -rvf $(DEPSOLVER_PLT)
	rm -rvf ./deps/*

.PHONY: all deps compile dialyze test doc validate release relup clean distclean ciclean
