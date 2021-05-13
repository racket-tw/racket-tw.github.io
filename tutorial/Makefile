SCRBL := raco scribble +m --redirect-main http://docs.racket-lang.org/

OBJS = $(patsubst %.scrbl, %.html, $(shell ls **/*.scrbl))
OUT_DIR = ../dist
OUT_OBJS = $(addprefix $(OUT_DIR)/, $(OBJS))
$(OUT_DIR)/%.html: %.scrbl
	@mkdir -p $(dir $@)
	@$(SCRBL) --dest $(dir $@) $<

.PHONY: all
all: $(OUT_DIR) $(OUT_OBJS)
