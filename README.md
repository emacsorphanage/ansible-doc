ansible-doc
===========

![Maintenance](https://img.shields.io/maintenance/yes/2016.svg)
[![License GPL 3][badge-license]][copying]
[![MELPA Stable][badge-melpa-stable]](http://stable.melpa.org/#/ansible-doc)
[![MELPA][badge-melpa]](http://melpa.org/#/ansible-doc)

Ansible documentation lookup for GNU Emacs:

![Ansible documentation in GNU Emacs][screenshot]

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
[COPYING]: https://github.com/lunaryorn/ansible-doc.el/blob/master/COPYING
[screenshot]: https://raw.githubusercontent.com/lunaryorn/ansible-doc.el/master/screenshot.png
[badge-melpa-stable]: http://stable.melpa.org/packages/ansible-doc-badge.svg
[badge-melpa]: http://melpa.org/packages/ansible-doc-badge.svg

Installation
------------

As usual, from [MELPA][] or [MELPA Stable][], with `M-x package-install RET
ansible-doc`.

In your [`Cask`][cask] file:

```cl
(source melpa)

(depends-on "ansible-doc")
```

In your `init.el`:

```cl
(add-hook 'yaml-mode-hook #'ansible-doc-mode)
```

[Cask]: https://github.com/cask/cask
[MELPA]: http://melpa.milkbox.net
[MELPA Stable]: http://melpa-stable.milkbox.net

Usage
-----

In a YAML Mode buffer, press <kbd>C-c ?</kbd> (`ansible-doc`) to view the
documentation of an Ansible Module.  The command prompts for the name of a
module, defaulting to the module at point if any.

The first invocation of `ansible-doc` may take some time, because the command
assembles a list of all Ansible modules for completion.

License
-------

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See [`COPYING`][copying] for details.
