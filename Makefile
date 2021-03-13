SCRBL := raco scribble +m --redirect-main http://docs.racket-lang.org/

dist:
	@mkdir dist
OBJS = $(patsubst %.scrbl, %.html, $(shell ls *.scrbl **/*.scrbl tutorial/**/*.scrbl))
OUT_DIR = dist
OUT_OBJS = $(addprefix $(OUT_DIR)/, $(OBJS))
$(OUT_DIR)/%.html: %.scrbl
	@mkdir -p $(dir $@)
	@$(SCRBL) --dest $(dir $@) $<

.PHONY: build
build: $(OUT_DIR) $(OUT_OBJS)
