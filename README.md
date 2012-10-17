Clafer Wiki
===========

Version v0.3.1.17-10-2012

**Clafer-based wiki** is a wiki system integrated with Clafer: it allows for embedding Clafer model fragments in wiki pages and provides model authoring support. The project is based on the [Gitit wiki](http://gitit.net/). Goals of the project:

* provide infrastructure for informal-to-formal modeling with Clafer,
* provide web interface for collaborative modeling with [Clafer compiler](https://github.com/gsdlab/clafer).

Dependencies
------------

* [The Haskell Platform](http://hackage.haskell.org/platform) 2012.2.0.0
  * on Ubuntu Precise, there is Haskell Platform 2012.1.0.0
  * alternatively, GHC 7.4.1
* [Git](http://git-scm.com) 
* [Gitit wiki](http://gitit.net) 0.10.0.1
* [Clafer compiler](https://github.com/gsdlab/clafer/) 0.3.1
  * the binary distribution from [clafer-tools-0.3.1](https://github.com/gsdlab/claferig/downloads) cannot be used becase the wiki calls the compiler using API.
  
Installation
------------

* install the Haskell Platrorm
* install Git
* install Clafer compiler from source code
* in some `<source directory>` where you want to have the wiki source code,
  * execute `git clone git://github.com/gsdlab/claferwiki.git`
  * execute `make dependencies` to install `pandoc` with code highlighting support and `gitit` wiki 
  * execute `make install to=<target directory>` 

### Important: Branches must correspond

Clafer, ClaferIG, and ClaferWiki are following the *simultaneous release model*. 
The branch `master` contains releases, whereas the branch `develop` contains code under development. 
When building the tools, the branches should match:
Releases `clafer/master` and `claferwiki/master` are guaranteed to work well together.
Development versions `clafer/develop` and `claferwiki/develop` should work well together but this might not always be the case.


Running
-------

* in the `<target directory>` execute `claferwiki.sh` to start the wiki server

Wiki can be configured by editing the `gitit.cnf` file. See [Configuring and customizing gitit](http://gitit.net/README#configuring-and-customizing-gitit).

Update
------

* in the `<source directory>` execute `git pull` 
* execute `make update to=<target directory>` 
  * this will keep the directory structure and your existing git repository with the wiki contents

Features
--------

<a href="https://raw.github.com/gsdlab/claferwiki/master/spec/telematics-screenshot-1.png">
<img src="https://raw.github.com/gsdlab/claferwiki/master/spec/telematics-screenshot-1.png" width="30%" alt="Telematics Example, Module Overview">
</a>
<a href="https://raw.github.com/gsdlab/claferwiki/master/spec/telematics-screenshot-2.png">
<img src="https://raw.github.com/gsdlab/claferwiki/master/spec/telematics-screenshot-2.png" width="30%" alt="Telematics Example, Module Details" >
</a>

* syntax coloring for Clafer models
* linking from clafer name references within model fragments to clafer definitions
* linking from clafer names used in wiki text to clafer definitions
* pop-up information about clafers in graph rendering
* translating constraints to controlled natural language and showing as pop-up?
* overview with graph rendering, statistics, and download links for the entire model source and self-contained HTML rendering

Using Clafer Wiki
-----------------

For general usage information for the GitIt wiki see the [README](http://gitit.net/README).

You can insert code blocks with clafer code anywhere in the page as follows:


\`\`\`clafer

`<here goes your model fragment>`

\`\`\`

The model overview, including the graph, stats, and download links, can be added as follows:

\`\`\` `{.clafer .summary}`

`<the contents in this block are ignored>`

\`\`\`

To have the code blocks correctly processed, make sure to add an empty line before and after the code block, even if the code block is the last element on the page.

How it works
------------

* Clafer Wiki is a set of plugins for the GitIt wiki which processes clafer code blocks and invokes the Clafer compiler.
* All code blocks on a single page are interpreted as a single module.
* The Clafer compiler generates HTML rendering of each code block.
* The rendering is enriched with:
  * links to the definitions for super clafers (inheritance)
  * links to the types of references
  * compiler error highlights
