zettel-mode [name subject to change]
====================================

An opinionated Emacs mode for [Zettelkasten][1]-style note-taking.
Heavily inspired by [org-roam][2], striving to be simpler, less
dependent on the external state (like the auxiliary database of
`org-roam`) and local only to the affected files (i.e. no global minor
modes).

ASSUMPTIONS
-----------

Uses [Deft][3] as its "entry point" and was developed under an
assumption the `Deft` files reside in `~/.deft/`.  It should work with
any `deft-directory` with the final directory named `.deft` but it
wasn't tested.  In other cases, you'll need to customize
`auto-mode-alist` accordingly.  Using subdirectories wasn't tested.

[1]: https://zettelkasten.de/
[2]: https://github.com/org-roam/org-roam
[3]: https://github.com/jrblevin/deft

Recommended `Deft` settings:

```elisp
(setq deft-default-extension "org")
```

FEATURES
--------

- Quick new note creation: select some text and press <kbd>C-c C-l</kbd>.
- A sidebar with lists of references to and from the current file.
- A fully optional reference and backreference cache (rebuilt with `make(1)`).

For the settings see the provided [Customize][4] menus.

[4]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html

SCRIPTS
-------

In the `scripts/` directory reside some helper scripts for common
maintenance tasks for the notes.  In general they work but they
probably don't handle all the corner cases (like filenames with
special characters).  Each script has a brief description included as
a comment.

FAQ
---

Q: Why it's not on MELPA?

A: I don't want to hoard such a generic name as `zettel-mode` and for
now I don't have a better name for it either.

COPYRIGHT
---------

Copyright (C) 2020  Wojciech Siewierski

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
