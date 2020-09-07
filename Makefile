dist:
	@git clone git@github.com:racket-tw/racket-tw.github.io.git dist -b master

dist/index.html: dist index.scrbl
	@cd dist; scribble ../index.scrbl
dist/tutorial: dist
	@mkdir -p dist/tutorial
dist/tutorial/quick-start.html: dist/tutorial tutorial/quick-start.scrbl
	@cd dist/tutorial; scribble ../../tutorial/quick-start.scrbl
dist/tutorial/module.html: dist/tutorial tutorial/module.scrbl
	@cd dist/tutorial; scribble ../../tutorial/module.scrbl

.PHONY: all
all: dist/index.html dist/tutorial/quick-start.html dist/tutorial/module.html
