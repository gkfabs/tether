# tether

Review GitHub pull requests and GitLab merge requests from the file you are
reading in Emacs: comment on the current line or region, and jump between
the file, the PR diff, the forge topic, the review threads and the browser
while keeping file and line.

Built on [Forge](https://github.com/magit/forge): the repository, the pull
request and the API token all come from Forge, so there is nothing extra to
configure.

**Status:** early development. Commenting and navigation work; review
threads, the comment editor and draft reviews are still to come.

## Requirements

- Emacs 29.1 or later
- Forge set up for the repository (`forge-add-repository`), with its token
  in your auth-source, and its pull requests pulled (`forge-pull`)

## Setup

```elisp
(use-package tether
  :load-path "~/src/tether/"           ; until it is on MELPA
  :hook (after-init . global-tether-mode)
  :bind (:map tether-mode-map ("C-c t" . tether-prefix-map)))
```

`global-tether-mode` turns `tether-mode` on in the files of a repository
whose current branch has a pull request in forge's database. Opening a file
never contacts GitHub or GitLab: the first tether command does.

## Usage

The `tether` menu (`C-c t @` with the binding above, or `@` in
`magit-dispatch` and `forge-dispatch`) has every command:

| Key | Command | |
|-----|---------|-|
| `c` | `tether-nitpick` | comment on the current line or region |
| `F` | `tether-nitpick-file` | comment on the whole file |
| `f` | `tether-visit-file` | the file at the line (the PR head's version when the PR is not checked out) |
| `d` | `tether-diff` | the PR diff, at the hunk of the line |
| `t` | `tether-topic` | forge's buffer of the pull request |
| `b` | `tether-browse` | the PR diff in the browser, at the line |
| `m` | `tether-status` | magit-status |
| `v` | `tether-review-pullreq` | pick any pull request and open its diff |
| `s`, `g`, `x` | | show, refresh, forget what tether knows about the PR |

A line can take a comment when it is in the pull request diff (a changed
line, or one of the three lines around it), and has not changed locally
since the PR head: tether maps lines through uncommitted and unsaved edits.
In the PR diff, removed lines can be commented too. With a prefix argument,
commands let you pick the pull request.

The mode line shows the pull request (`#12` on GitHub, `!12` on GitLab),
with ⚠ when the local branch is not the PR head. Once tether resolved the
PR, the fringe marks the lines that can take a comment.

On GitHub, while you have a pending review (started on github.com),
comments are added to it: GitHub refuses single comments then.

## Tested versions

CI runs against two dependency sets:

| Set      | magit                | forge            | ghub             |
|----------|----------------------|------------------|------------------|
| `min`    | latest release (4.7.1 on 2026-10-08) | 0.6.0  | 5.3.0            |
| `latest` | MELPA snapshot       | MELPA snapshot   | MELPA snapshot   |

`Package-Requires` can only express minimum versions. If you pin packages
(straight.el or elpaca lockfiles, Nix), pin within this range. If tether
warns that magit/forge/ghub changed, run `M-x tether-check-dependencies`
and report the output.

## Development

Needs [Eask](https://emacs-eask.github.io/).

```sh
make deps                    # latest dependency set
make all                     # compile, lint, surface check, tests
make deps all TETHER_DEPS=min
```

`make surface` fails when the code uses a magit/forge/ghub symbol that is
internal or not in the allowlist in `tether-deps.el`.

## License

GPL-3.0-or-later.
