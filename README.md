Clafer-based Wiki
=================

**Clafer-based wiki** is a wiki system integrated with Clafer. The project is based on the [Gitit wiki](http://gitit.net/). Goals of the project:
* provide infrastructure for informal-to-formal modeling with Clafer,
* provide web interface for working with current tools: [Clafer compiler](https://github.com/gsdlab/clafer) and [ClaferIG](https://github.com/gsdlab/claferIG).

Requirements
------------
* syntax coloring for Clafer models
* navigation over Clafer code with navigable links (e.g. for references)
* tag for clafers in the text
* translating constraints to CNL and showing as pop-up?

How it works
------------
* it's a plugin to GitIt wiki which processes clafer code blocks and invokes the Clafer translator.