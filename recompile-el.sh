#!/bin/bash

echo "Cleaning old .elc files..."
find ~/.emacs.d -name "*.elc" -delete

echo "Recompiling with new Emacs version..."
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval "(byte-recompile-directory \"~/.emacs.d\" 0 t)"

echo "Done! Start Emacs normally now and run M-x package-recompile-all."

