Clafer-based Wiki
=================

**Clafer-based wiki** is a wiki system integrated with Clafer: it allows for embedding Clafer model fragments in wiki pages and provides authoring support. The project is based on the [Gitit wiki](http://gitit.net/). Goals of the project:

* provide infrastructure for informal-to-formal modeling with Clafer,
* provide web interface for working with current tools: [Clafer compiler](https://github.com/gsdlab/clafer) and [Clafer instance generator](https://github.com/gsdlab/claferIG).

Features
--------

* syntax coloring for Clafer models
* linking from clafer name rererences within model fragments to clafer definitions
* linking from clafer names used in wiki text to clafer definitions
* pop-up information about clafers on mouse over events
* translating constraints to controlled natural language and showing as pop-up?

How it works
------------

* It's a plugin to GitIt wiki which processes clafer code blocks and invokes the Clafer compiler and instance generator.