;;; pre-early-init.el --- This file is loaded before early-init.el. Use it for configurations that need to be set even earlier in the startup sequence, typically affecting the initial setup of the Emacs environment. -*- no-byte-compile: t; lexical-binding: t; -*-

;;; These settings control the visibility of dialogs, context menus, toolbars, menu bars, and tooltips.
(setq minimal-emacs-ui-features '(context-menu tool-bar menu-bar dialogs tooltips))
