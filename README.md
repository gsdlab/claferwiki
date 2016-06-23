# Claferwiki

##### v0.4.4

**Claferwiki** is a wiki system integrated with [Clafer compiler](https://github.com/gsdlab/clafer). [Clafer](http://clafer.org) is a lightweight yet powerful structural modeling language. Claferwiki allows for embedding Clafer model fragments in wiki pages and provides model authoring support including code highlighting, parse and semantic error reporting, hyperlinking from identifier use to its definition, and graphical view rendering.

Claferwiki supports informal-to-formal modeling, that is, gradually refining parts of specification in natural language into a Clafer model fragments. Claferwiki supports *literate modeling* - both the rich text and the model fragments can be freely mixed. Informal-to-formal modeling is important during domain modeling.

Also, Claferwiki acts as a collaborative, lightweight, web-based model publishing environment for Clafer.
In addition to code highlighting, error reporting, hyperlinking, and graphical view rendering, it also provides model versioning and distributed online/offline editing capabilities as it is based on the Git distributed version control system and the [Gitit wiki](http://gitit.net/).

Claferwiki is also integrated with other Clafer Web Tools, allowing to open the current page in:

* Clafer Integrated Development Environment ([ClaferIDE](https://github.com/gsdlab/claferIDE)),
* Clafer Configurator ([ClaferConfigurator](https://github.com/gsdlab/ClaferConfigurator)),
* Multi-Objective [Visualizer and Explorer](https://github.com/gsdlab/ClaferMooVisualizer)

### Live demo

[Try me!](http://t3-necsis.cs.uwaterloo.ca:8091/)

If the demo is down or you encounter a bug, please email [Michal Antkiewicz](mailto:mantkiew@gsd.uwaterloo.ca).

## Contributors

* [Michał Antkiewicz](http://gsd.uwaterloo.ca/mantkiew), Main developer. Requirements, development, architecture, testing, technology transfer.
* Chris Walker, co-op student May-Aug, 2012. Developer of Clafer Wiki, HTML and GraphViz generators.
* [Jimmy Liang](http://gsd.uwaterloo.ca/jliang), Clafer compiler support, including multi-fragment compilation, source/AST/IR traceability, parsing and compilation error reporting.

## Getting the Clafer Wiki

Clafer can be installed either from Hackage or from the source code.

### Dependencies for running

Regardless of the installation method, the following are required:

* [Clafer compiler](https://github.com/gsdlab/clafer/) v0.4.4
* [GHC](https://www.haskell.org/downloads) >= v7.10.*
* [Git](http://git-scm.com)
* [Gitit wiki](http://gitit.net) v0.12.1.1
* GraphViz

### Installation from Hackage

Claferwiki is now available on [Hackage](http://hackage.haskell.org/package/claferwiki-0.4.4/) and it can be installed using either [`stack`](https://github.com/commercialhaskell/stack) or [`cabal-install`](https://hackage.haskell.org/package/cabal-install).

#### Installation using `stack`

Stack is the only requirement: no other Haskell tooling needs to be installed because stack will automatically install everything that's needed.

1. `stack update`
2. `stack install claferwiki`

#### Installation using `cabal-install`

1. `cabal update`
2. `cabal install claferwiki-0.4.4 -fhighlighting -fhttps -fplugins -fnetwork-uri`
3. `cd <cabal's lib or share folder>`  (`C:\Users\<user>\AppData\Roaming\cabal\i386-windows-ghc-8.0.1\claferwiki-0.4.4` on Windows or `.cabal/share/x86_64-linux-ghc-8.0.1/claferwiki-0.4.4/` on Linux)
  * execute `make install to=<target directory>`
  * this will copy the wiki files

### Important: branches must correspond

All related projects are following the *simultaneous release model*.
The branch `master` contains releases, whereas the branch `develop` contains code under development.
When building the tools, the branches should match.
Releases from branches 'master` are guaranteed to work well together.
Development versions from branches `develop` should work well together but this might not always be the case.

### Installation from source code

1. In some `<source directory>` where you want to have the wiki source code
   * execute `git clone git://github.com/gsdlab/claferwiki.git`
2. execute `stack update`
3. execute `make init`
4. [install clafer from source code](https://github.com/gsdlab/clafer#installation-from-the-source-code)
4. execute `make`
5. execute `make install to=<target directory>`
  * this will copy the wiki files
6. in `<target directory>`, execute `git init` to create a git repository for the wiki data
  * NOTE: see `repository-path:` option in `gitit.cnf`


# Usage

Wiki can be configured by editing the `gitit.cnf` file. See [Configuring and customizing gitit](http://gitit.net/README#configuring-and-customizing-gitit).

#### Installation using `stack`

* in the `<target directory>` execute `stack exec gitit -- -f gitit.cnf` to start the wiki server.

#### Using `cabal-install`

* in the `<target directory>` execute `claferwiki.sh` to start the wiki server.

The script can either use gitit and clafer installed in the user package space (default) or in the sandbox. The sandbox location can be provided using the parameter `--sandbox` as follows:

* `./claferwiki.sh --sandbox <relative or absolute path to the sandbox>`, or
* `./claferwiki.sh --sandbox` which will use the default sandbox location `../.clafertools-cabal-sandbox`

## Update

* in the `<source directory>` execute `git pull`
* execute `make update to=<target directory>`
  * this will keep the directory structure and your existing git repository with the wiki contents

## Features

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
* integration with ClaferMooVisualizer

## Using Clafer Wiki

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

## How it works

* Clafer Wiki is a set of plugins for the GitIt wiki which processes clafer code blocks and invokes the Clafer compiler.
* All code blocks on a single page are interpreted as a single module.
* The Clafer compiler generates HTML rendering of each code block.
* The rendering is enriched with:
  * links to the definitions for super clafers (inheritance)
  * links to the types of references
  * compiler error highlights

# Need help?

* Visit [language's website](http://clafer.org).
* Report issues to [issue tracker](https://github.com/gsdlab/claferwiki/issues)
