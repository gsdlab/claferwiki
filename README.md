Clafer Wiki v0.3.1
==================

**Clafer-based wiki** is a wiki system integrated with Clafer: it allows for embedding Clafer model fragments in wiki pages and provides model authoring support. The project is based on the [Gitit wiki](http://gitit.net/). Goals of the project:

* provide infrastructure for informal-to-formal modeling with Clafer,
* provide web interface for working with current tools: [Clafer compiler](https://github.com/gsdlab/clafer) and [Clafer instance generator](https://github.com/gsdlab/claferIG).

Dependencies
------------

* [The Haskell Platform](http://hackage.haskell.org/platform) 2012.2.0.0
  * on Ubuntu Precise, there is Haskell Platform 2012.1.0.0
  * alternatively, GHC 7.4.1
* [Git](http://git-scm.com) 
* [Gitit wiki](http://gitit.net) 0.10.0.1
* [Clafer compiler](https://github.com/gsdlab/clafer/) 0.3.1
  * alternatively, download the binary distribution [clafer-tools-0.3.1](https://github.com/gsdlab/claferig/downloads)
  
Installation
------------

* install the Haskell Platrorm
* install Git

### From `clafer-tools-0.3.1` binary distribution

* extract `clafer-tools-0.3.1` archive to some `<source directory>`
  * in `claferwiki` subfolder, execute `make install to=<target directory>`

### From source code

* install Clafer compiler
* in some `<source directory>` where you want to have the wiki source code,
  * execute `git clone git://github.com/gsdlab/claferwiki.git`
  * execute `make install to=<target directory>` 

Running
-------

* in the `<target directory>` execute `claferwiki.sh` to start the wiki server

Update
------

* in the `<target directory>` execute `git pull` 

Features
--------

* syntax coloring for Clafer models
* linking from clafer name references within model fragments to clafer definitions
* linking from clafer names used in wiki text to clafer definitions
* pop-up information about clafers on mouse over events
* translating constraints to controlled natural language and showing as pop-up?

How it works
------------

* Clafer Wiki is a set of plugins for the GitIt wiki which processes clafer code blocks and invokes the Clafer compiler.
* All code blocks on a single page are interpreted as a single module.
* The Clafer compiler generates HTML rendering of each code block.
* The rendering is enriched with:
  * links to the definitions for super clafers (inheritance)
  * links to the types of references
  * compiler error highlights
