SCRBL := raco scribble +m --redirect-main http://docs.racket-lang.org/

dist:
	@git clone git@github.com:racket-tw/racket-tw.github.io.git dist -b master
OBJS = $(patsubst %.scrbl, %.html, $(shell ls *.scrbl **/*.scrbl tutorial/**/*.scrbl))
OUT_DIR = dist
OUT_OBJS = $(addprefix $(OUT_DIR)/, $(OBJS))
$(OUT_DIR)/%.html: %.scrbl
	@mkdir -p $(dir $@)
	@$(SCRBL) --dest $(dir $@) $<

.PHONY: build publish
build: $(OUT_DIR) $(OUT_OBJS)
publish: build
	@cd dist; git add -A
	@cd dist; git commit -m "update $$(date +%Y/%m/%d-%H:%M:%S)"
	@cd dist; git push origin master
