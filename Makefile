dist:
	@git clone git@github.com:racket-tw/racket-tw.github.io.git dist -b master

dist/index.html: dist
	@cd dist; scribble ../index.scrbl
dist/tutorial: dist
	@mkdir -p dist/tutorial
dist/tutorial/quick-start.html: dist/tutorial
	@cd dist/tutorial; scribble ../../tutorial/quick-start.scrbl

.PHONY: all
all: dist/index.html dist/tutorial/quick-start.html
