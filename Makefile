SCRBL := raco scribble ++style style.css +m --redirect-main http://docs.racket-lang.org/

OBJS = $(patsubst %.scrbl, %.html, $(shell ls *.scrbl))
OUT_DIR = dist
OUT_OBJS = $(addprefix $(OUT_DIR)/, $(OBJS))
$(OUT_DIR)/%.html: %.scrbl style.css
	@mkdir -p $(dir $@)
	@$(SCRBL) --dest $(dir $@) $<

$(OUT_DIR):
	@mkdir $(OUT_DIR)
.PHONY: build clean
build: $(OUT_DIR) $(OUT_OBJS)
	@make -C post build
	@make -C tutorial build
clean:
	@rm -r $(OUT_DIR)
