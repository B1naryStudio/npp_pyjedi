# npp_pyjedi
Notepad++ plugin to wrap Jedi features

It uses patched DFPN++ library (http://www.cab.i24.cc/projects/npppluginbase/) to communicate with Notepad++ and Python Jedi library (http://jedi.jedidjah.ch) to analize sources.
Implemented features: auto-complete, go to definition, search usages, show function call signature hint, show docstring hint on mouse hover.

Written in Delphi XE2. Compilation required "superobject" (https://code.google.com/p/superobject/) and "Python for Delphi" (http://mmm-experts.com/Products.aspx?ProductId=3) libraries.

Using it requires Python with installed Jedi (e.g. pip install jedi).