#!/bin/sh
echo "Updating personal Emacs site-lisp"
pushd ~/.emacs.d/site-lisp

#Yasnippet
wget http://yasnippet.googlecode.com/files/yasnippet-0.6.1c.tar.bz2
tar jxf yasnippet-0.6.1c.tar.bz2
rm yasnippet-0.6.1c.tar.bz2

#Growl
wget http://www.emacswiki.org/emacs/download/todochiku.el

#Twitter
wget http://www.emacswiki.org/emacs/download/twit.el

#Google C Style
wget http://google-styleguide.googlecode.com/svn/trunk/google-c-style.el

#ANSI Color
wget http://www.emacswiki.org/cgi-bin/wiki/download/ansi-color.el

#Color Theme
wget http://www.emacswiki.org/cgi-bin/emacs/download/color-theme-tango.el

#MATLAB
wget http://www.mathworks.com/matlabcentral/files/104/matlab.el

#Stratego
wget https://svn.strategoxt.org/repos/StrategoXT/stratego-editors/trunk/emacs/stratego.el

#AutoComplete
wget http://auto-complete.googlecode.com/hg/auto-complete-config.el
wget http://auto-complete.googlecode.com/hg/auto-complete.el

#Pymacs
wget http://pymacs.progiciels-bpi.ca/archives/Pymacs-0.24-beta1.tar.gz
tar zxf Pymacs-0.24-beta1.tar.gz
rm Pymacs-0.24-beta1.tar.gz

#Zen Coding
wget http://github.com/chrisdone/zencoding/raw/master/zencoding-mode.el

popd
