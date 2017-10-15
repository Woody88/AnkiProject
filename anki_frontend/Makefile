
BIN = ./node_modules/.bin
SRC = src
BUILD = build
CSS = assets/css
JS = assets/js
FONTS = assets/fonts
IMAGES = assets/images
HTML = src/index.html


build: build-directory html css js
	@$(BIN)/browser-sync reload
	@echo -e "\n\n\n\n"

build-directory:
	mkdir -p $(BUILD)

jsFolder:
	$(BIN)/uglifyjs $(JS)/*.js --compress --mangle --output app-dist.js
	mv app-dist.js $(BUILD)/app-dist.js

js:
	$(BIN)/elm make $(SRC)/Main.elm \
		--output $(BUILD)/app.js
	$(BIN)/uglifyjs --compress --mangle \
		--output $(BUILD)/app.min.js \
		$(BUILD)/app.js 2> /dev/null
	mv $(BUILD)/app.min.js $(BUILD)/app.js

css:
	elm-css src/Stylesheets.elm \
	  rm $(BUILD)/index.css
		cp index.css $(BUILD)/index.css
# css:
# 	cat $(CSS)/*.css | \
# 		$(BIN)/cleancss --output $(BUILD)/style.css

html:
	cp $(HTML) $(BUILD)/index.html

watch:
	$(BIN)/chokidar "$(SRC)" "assets" -c "make"

browser-sync:
	$(BIN)/browser-sync start \
		--server $(BUILD) --files $(BUILD)
fonts:
	cp -r $(FONTS) $(BUILD)

images:
	cp -r $(IMAGES) $(BUILD)

setup:
	npm install
