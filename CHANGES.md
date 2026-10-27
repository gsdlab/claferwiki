* claferwiki Version 0.5.5, unreleased

Minor release with Clafer 0.5.5.

* Models can use decimals, a new primitive type `decimal` with a fixed number of fractional digits, e.g., `cost -> decimal = 19.99`.
* The buttons for ClaferIDE, ClaferConfigurator, and ClaferMooVisualizer can link to the tools under paths, e.g., `https://wiselab.uwaterloo.ca/claferide/`: the environment variables `CLAFERIDE_URL`, `CLAFERCONFIGURATOR_URL`, and `CLAFERMOOVISUALIZER_URL` give the tools' URLs (by default, still the ports 8094, 8093, and 8092). Behind a reverse proxy, the model's URL in the links follows the headers `X-Forwarded-Proto` and `X-Forwarded-Prefix`, e.g., `https://wiselab.uwaterloo.ca/modelwiki/clafer/<page>.cfr`, so the proxy no longer needs to rewrite the links. The links no longer contain the port 80 when the wiki is reached without a port.

* claferwiki Version 0.5.4, Oct 25, 2026

Minor release with Clafer 0.5.4.

* The model graphs contain only the `svg` element of Graphviz's output, without its XML declaration, DOCTYPE and comments, which do not belong in an HTML page. HTML rewriting proxies, such as Apache's `mod_proxy_html`, mangled them and showed the graphs' double-click handler as text in the model summary.
* `custom.css` imports gitit's `highlighting.css` instead of `hk-pyg.css`, which gitit no longer provides: every page requested a missing stylesheet, and code blocks had no syntax highlighting.
* The live demo is at https://wiselab.uwaterloo.ca/modelwiki/.

[Release](https://github.com/gsdlab/claferwiki/pull/22)

* claferwiki Version 0.5.3, Oct 15, 2026

Minor release with Clafer 0.5.3.

* The buttons for ClaferIDE, ClaferConfigurator, and ClaferMooVisualizer link to the host and port that the browser used to reach the wiki (the `Host` or `X-Forwarded-Host` header), instead of the short host name of the machine, which made the links unreachable (e.g., `http://t3-necsis:8094/` instead of `http://t3-necsis.cs.uwaterloo.ca:8094/`).

[Release](https://github.com/gsdlab/claferwiki/pull/21)

* claferwiki Version 0.5.2 released on Oct 9, 2026

[Release](https://github.com/gsdlab/claferwiki/pull/20)

* claferwiki Version 0.5.1 released on Aug 24, 2023

[Release](https://github.com/gsdlab/claferwiki/pull/19)

* claferwiki Version 0.4.5 released on Mar 13, 2017

[Release](https://github.com/gsdlab/claferwiki/pull/18)

* claferwiki Version 0.4.4 released on Jun 23, 2016

[Release](https://github.com/gsdlab/claferwiki/pull/15)

* claferwiki Version 0.4.3 released on Dec 22, 2015

[Release](https://github.com/gsdlab/claferwiki/pull/14)

* claferwiki Version 0.4.2.1 released on Oct 20, 2015

Updated to clafer 0.4.2.1, added stack instructions to README.

* claferwiki Version 0.4.2 released on Oct 16, 2015

[Release](https://github.com/gsdlab/claferwiki/pull/13)

* claferwiki Version 0.4.1 released on Sep 1, 2015

[Release](https://github.com/gsdlab/claferwiki/pull/12)

* claferwiki Version 0.4.0 released on Jul 28, 2015

[Release](https://github.com/gsdlab/claferwiki/pull/11)

* claferwiki Version 0.3.10 released on April 24, 2015

[Release](https://github.com/gsdlab/claferwiki/pull/10)

* claferwiki Version 0.3.9 released on March 06, 2015

[Release](https://github.com/gsdlab/claferwiki/pull/9)

* claferwiki Version 0.3.8 released on January 27, 2015

[Release](https://github.com/gsdlab/claferwiki/pull/8)

* claferwiki Version 0.3.7 released on October 23, 2014

[Release](https://github.com/gsdlab/claferwiki/pull/7)

* claferwiki Version 0.3.6.1 released on July 08, 2014

[Release](https://github.com/gsdlab/claferwiki/pull/6)

* claferwiki Version 0.3.6 released on May 23, 2014

[Release](https://github.com/gsdlab/claferwiki/pull/5)

* claferwiki Version 0.3.5 released on January 20, 2014

[Release](https://github.com/gsdlab/claferwiki/pull/4)

* claferwiki Version 0.3.4 released on September 20, 2013

[Release Diff](https://github.com/gsdlab/claferwiki/compare/61a313dd9f65c0a378d97bb4e04be2464af7dbe5...b705ad9744502d15ae39d8503808914c5e798d8d)

* claferwiki Version 0.3.3 released on August 14, 2013

[Release](https://github.com/gsdlab/claferwiki/pull/1)
