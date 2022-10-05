# Diffgit

Diffgit is a minor mode for displaying git repo diffs with diffstatic.

Diffgit provides simple interfaces for emacs user to diff git repo with [diffstatic](https://difftastic.wilfred.me.uk/).

## Features

* show work tree diff
* show diff of a specific commit in Git

## Installation

1. Install Emacs 26 and above versions.
2. Install difftastic, see [diffstatic](https://difftastic.wilfred.me.uk/).
3. Configure git to use external difftool:

Add the following to `.gitconfig`

``` shell
[diff]
        tool = difftastic

[difftool]
        prompt = false

[difftool "difftastic"]
        cmd = difft "$LOCAL" "$REMOTE"
```

4. (Optional) Install package [xterm-color](https://github.com/atomontage/xterm-color)
5. Add following to your emacs config
```elisp
(add-to-list 'load-path "<path-to-diffgit>")

(require 'diffgit)
```

> Simple as That!

## Usage

Diffgit provide with two commands to invoke diffstatic in a git repo:

* `diffgit-gen-work-tree-diff`: show work tree diff
* `diffgit-gen-diff-by-commit`: show diff of a specific commit in Git
