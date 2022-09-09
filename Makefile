SCRBL := raco scribble --htmls ++style style.css +m --redirect-main http://docs.racket-lang.org/

.PHONY: install build clean
install:
	raco req -A

build:
	@$(SCRBL) dist.scrbl
	@make -C post build
	@make -C tutorial build
