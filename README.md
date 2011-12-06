.emacs
=================

Thanks to magnars, the guy from the [Emacs Rocks](http://emacsrocks.com) screencasts, for inspiring me to update my .emacs config (I've forked his).

Setup
-----
To grab all the dependencies:

    cd ~
    mkdir old.emacs && mv .emacs* old.emacs

    git clone --recursive git://github.com/graehl/.emacs.d.git

To update including submodules:

    git pull
    git submodule update --init

Todo
----

* Stuff in defuns/ should use require/provide.
* bindings should all be prefaced by a require
* bindings should be uniformly (kbd "C-<f1>")
* see also TODO


Install emacs on mac
--------------------
I use [Cocoa Emacs with fullscreen support](http://citizen428.net/blog/2010/06/26/fullscreen-emacs-on-macos-x/). My version is 24.0.90.2 (Oct 11 2011).
