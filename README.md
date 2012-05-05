Clafer Wiki
===========

**Clafer-based wiki** is a wiki system integrated with Clafer: it allows for embedding Clafer model fragments in wiki pages and provides authoring support. The project is based on the [Gitit wiki](http://gitit.net/). Goals of the project:

* provide infrastructure for informal-to-formal modeling with Clafer,
* provide web interface for working with current tools: [Clafer compiler](https://github.com/gsdlab/clafer) and [Clafer instance generator](https://github.com/gsdlab/claferIG).

Dependencies
------------

* [The Haskell Platform](http://hackage.haskell.org/platform)
* [Git](http://git-scm.com)
* [Gitit wiki](http://gitit.net)
* [Clafer compiler](https://github.com/gsdlab/clafer)
  
Installation
------------

* install the dependencies
  * enable syntax highlighting [see note 2](http://gitit.net/Installing)
* in some `<target directory>` where you want to have the wiki,
  * execute `git clone git://github.com/gsdlab/claferwiki.git`
  * execute `make run` 

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

* Clafer Wiki is a plugin for the GitIt wiki which processes clafer code blocks and invokes the Clafer compiler and instance generator.
* All code blocks on a single page are interpreted as a single module.
* The Clafer compiler generates HTML rendering of each code block.
* The rendering is enriched with links to the definitions for super clafers (inheritance) and the types of references.

Example
-------

The following example model:

```clafer
abstract A
    b ?
    c -> C

abstract B
    d ?

abstract C : B

anA : A
    [ b ]
    [ c = aC ]

aC : C
    [ no d ]
```

will be rendered as

<div id="c1_A">
<b>abstract</b> A<br>
<span style="padding-left:20px">b <b>?</b></span><br>
<span style="padding-left:20px">c <b>-&gt;</b> <a href="#c6_C">C</a></span>
</div>
<br>
<div id="c4_B">
<b>abstract</b> B<br>
<span style="padding-left:20px">d <b>?</b></span>
</div>
<br>
<div id="c6_C">
<b>abstract</b> C <b>:</b> <a href="#c4_B">B</a>
</div>
<br>
<div id="c7_anA">
anA <b>:</b> <a href="#c1_A">A</a><br>
<span style="padding-left:20px"><b>[</b> b <b>]</b></span><br>
<span style="padding-left:20px"><b>[</b> c <b>=</b> <a href="#c8_aC">aC</a> <b>]</b></span>
</div>
<br>
<div id="c8_aC">
aC <b>:</b> <a href="#c6_C">C</a><br>
<span style="padding-left:20px"><b>[</b> <b>no</b> d <b>]</b></span>
</div>




