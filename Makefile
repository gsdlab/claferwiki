all:
	stack build

install:
	stack install
	stack install gitit
	@if test ! -d ".git"; then \
		cp -f  -r .git $(to); \
	fi
	mkdir -p $(to)
	cp -f stack.yaml $(to)
	cp -f  gitit.cnf $(to)
	cp -f  claferwiki.sh $(to)
	chmod +x $(to)/claferwiki.sh
	cp -f  README.md $(to)
	mkdir -p $(to)/static/img
	cp -f  static/img/logo.png $(to)/static/img
	mkdir -p $(to)/static/css
	cp -f  static/css/custom.css $(to)/static/css/custom.css
	cp -f  static/css/clafer.css $(to)/static/css/clafer.css
	mkdir -p $(to)/templates
	cp -f  templates/footer.st $(to)/templates/footer.st
	cp -rf .stack-work/ $(to)



update:
	cp -f  claferwiki.sh $(to)
	cp -f  README.md $(to)
	cp -f  static/img/logo.png $(to)/static/img
	cp -f  static/css/custom.css $(to)/static/css/custom.css
	cp -f  static/css/clafer.css $(to)/static/css/clafer.css

clean:
	stack clean
