install:
#   cabal update
# 	cabal install -fhighlighting pandoc
#	cabal install gitit
	mkdir -p $(to)
	cp -f --preserve=all gitit.cnf $(to)
	cp -f --preserve=all claferwiki.sh $(to)
	cp -f --preserve=all README.md $(to)
	mkdir -p $(to)/static/img
	cp -f --preserve=all static/img/logo.png $(to)/static/img
	mkdir -p $(to)/plugins
	cp -f --preserve=all plugins/ClaferCleanup.hs $(to)/plugins
	cp -f --preserve=all plugins/ClaferReader.hs $(to)/plugins
	cp -f --preserve=all plugins/ClaferCompiler.hs $(to)/plugins
	cp -f --preserve=all plugins/ClaferWriter.hs $(to)/plugins

