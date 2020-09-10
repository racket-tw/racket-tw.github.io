SCRBL := scribble ++main-xref-in --redirect-main http://docs.racket-lang.org/

dist:
	@git clone git@github.com:racket-tw/racket-tw.github.io.git dist -b master

dist/index.html: dist index.scrbl
	@cd dist; $(SCRBL) ../index.scrbl
dist/tutorial: dist
	@mkdir -p dist/tutorial
dist/tutorial/quick-start.html: dist/tutorial tutorial/quick-start.scrbl
	@cd dist/tutorial; $(SCRBL) ../../tutorial/quick-start.scrbl
dist/tutorial/module.html: dist/tutorial tutorial/module.scrbl
	@cd dist/tutorial; $(SCRBL) ../../tutorial/module.scrbl

.PHONY: build
build: dist/index.html dist/tutorial/quick-start.html dist/tutorial/module.html

.PHONY: publish
publish: build
	@cd dist; git add -A
	@cd dist; git commit -m "update $$(date +%Y%m%d%H%M%s)"
	@cd dist; git push origin master
