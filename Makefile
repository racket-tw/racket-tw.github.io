SCRBL := raco scribble --htmls ++style style.css +m --redirect-main http://docs.racket-lang.org/

.PHONY: install build clean
install:
	raco pkg install --auto --skip-installed riposte semilit monotonic nanopass debug

build:
	@$(SCRBL) dist.scrbl
	@make -C post build
	@make -C tutorial build
