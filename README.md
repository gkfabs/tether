# tether

Review GitHub pull requests and GitLab merge requests from the file you are
reading in Emacs: comment on the current line or region, and jump between
the file, the PR diff, the forge topic, the review threads and the browser
while keeping file and line.

Built on [Forge](https://github.com/magit/forge): the repository, the pull
request and the API token all come from Forge, so there is nothing extra to
configure.

Commenting, suggestions, navigation, review threads (reply, resolve,
delete), draft reviews, single-commit review and the CI status of pull
requests work on GitHub and GitLab. GitHub Enterprise Server should work too, but is only covered by
unit tests.

## Requirements

- Emacs 29.1 or later
- Forge set up for the repository (`forge-add-repository`), with its token
  in your auth-source, and its pull requests pulled (`forge-pull`)

## Installation

From [MELPA](https://melpa.org/): `M-x package-install RET tether`. Or from
the repository: `M-x package-vc-install RET https://github.com/gkfabs/tether`.

## Setup

```elisp
(use-package tether
  :ensure t
  :hook (after-init . global-tether-mode)
  :bind (:map tether-mode-map ("C-c t" . tether-prefix-map)))
```

tether binds no key by default: `tether-prefix-map` holds the commands,
and every one also works from `M-x`.

`global-tether-mode` turns `tether-mode` on in the files of a repository
whose current branch has a pull request in forge's database. Opening a file
never contacts GitHub or GitLab: the first tether command does.

## Usage

The `tether` menu (`C-c t @` with the binding above, or `@` in
`magit-dispatch` and `forge-dispatch`) has every command:

| Key | Command | |
|-----|---------|-|
| `c` | `tether-nitpick` | comment on the current line or region |
| `S` | `tether-suggest` | suggest a change of the line or region |
| `F` | `tether-nitpick-file` | comment on the whole file |
| `f` | `tether-visit-file` | the file at the line (the PR head's version when the PR is not checked out) |
| `d` | `tether-diff` | the PR diff, at the hunk of the line |
| `t` | `tether-topic` | forge's buffer of the pull request |
| `b` | `tether-browse` | the PR diff in the browser, at the line |
| `P` | `tether-browse-ci` | the CI of the PR at point (in a list of PRs) or of the buffer, in the browser |
| `m` | `tether-status` | magit-status |
| `v` | `tether-review-pullreq` | pick any pull request and open its diff |
| `C` | `tether-review-commit` | review the commit at point alone |
| `u` | `tether-review-url` | review the pull request of a URL copied from the browser; a comment link also shows its thread |
| `l` | `tether-threads` | the review threads, by file |
| `.` | `tether-show-thread` | the thread of the current line: every comment, and a reply area below (`tether-reply` is the same) |
| `H` | `tether-toggle-resolved` | show or hide resolved threads in files |
| `R` | `tether-resolve` | resolve the thread at point, or reopen it |
| `e` | `tether-edit-comment` | edit one of your comments in the thread of the line (`C-c C-e` in a thread window) |
| `k` | `tether-delete-comment` | delete one of your comments, after confirmation |
| `w` | `tether-review-start` | start a draft review: comments are drafts until submitted |
| `W` | `tether-review-submit` | publish the drafts as a comment, an approval or a change request |
| `X` | `tether-review-discard` | delete the draft review |
| `A` | `tether-approve` | approve the pull request |
| `n`, `p` | `tether-next-thread`, `tether-previous-thread` | go from thread to thread, across files |
| `s`, `g`, `x` | | show, refresh, forget what tether knows about the PR |

Comments are written in a Markdown buffer showing the hunk for reference:
`C-c C-c` sends, `C-c C-k` cancels, `C-c @` mentions someone, `C-c C-s`
adds or removes a suggestion block, and `M-p`/`M-n` bring back text that
was not sent. When posting fails, the buffer stays open and says why.

A line can take a comment when it is in the pull request diff (a changed
line, or one of the three lines around it), and has not changed locally
since the PR head: tether maps lines through uncommitted and unsaved edits.
On GitLab, any line of a file the merge request changes can take one, and
a comment may cover lines of several hunks, as in GitLab's web interface;
GitHub only takes lines of one hunk.
In the PR diff, removed lines can be commented too. With a prefix argument,
commands let you pick the pull request.

The mode line shows the pull request (`#12` on GitHub, `!12` on GitLab),
with ⚠ when the local branch is not the PR head. Once tether resolved the
PR, the fringe marks the lines that can take a comment.

Review threads are fetched in the background once the pull request is
resolved, and again after each comment. They show on their line, with a
fringe mark and the start of the discussion at the end of the line
(resolved ones dimmed; `tether-show-resolved`, `tether-thread-summary`), and the mode line counts
the open ones (`!540·3`). The thread list keeps outdated threads apart,
with the hunk they were written on.

The PR diff (`tether-diff`, or a magit diff of the same range) shows the
open threads in full under their line, removed lines included, and `n`/`p`
go from thread to thread there. So does magit's commit buffer (`RET` on a
commit in the log), when the commit belongs to a pull request tether
resolved: the threads written on that commit, and those whose line has not
changed since. Comments from there go on that commit, as with
`tether-review-commit`. `tether-diff-thread-comments` set to nil shows
only the start of each thread.

Pressing `g` in magit-status also fetches the CI status of the open pull
requests (GitLab's head pipeline, GitHub's checks), in one request per
repository, and shows it at the end of each line of forge's pull request
list: ✓ passed, ✗ failed, ● running, ⊘ canceled or skipped. The mode line
shows the one of the buffer's pull request (`!540·3✓`); `tether-refresh`
fetches it too. Nothing else does: a magit-status refreshing by itself,
after a commit for example, shows what was fetched last.
`tether-ci-status` set to nil turns it off. On a branch without a pull
request, this needs `global-tether-mode`.

A draft review lives on the forge: GitHub's pending review, GitLab's draft
notes, so one started in the web interface works too. The mode line shows
its drafts (`!540✎3`). On GitLab, a change request makes you a reviewer if
you are not one (tether asks first), and tether checks it was recorded.

On GitHub, while you have a pending review (started on github.com),
comments are added to it: GitHub refuses single comments then.

## Related packages

[code-review](https://github.com/wandersoncferreira/code-review),
[emacs-pr-review](https://github.com/blahgeek/emacs-pr-review) and
[github-review](https://github.com/charignon/github-review) review in a
buffer of their own, built from the diff. tether comments from the file
you are reading, and leaves the repository, the pull request and the token
to Forge.

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

## AI assistance

tether is written with the help of an AI assistant (Claude), and each file
says so in its header (`Assisted-by:`). The author designs it, reviews the
changes and uses it on real pull requests; the tests and CI above check
every change.

## License

GPL-3.0-or-later.
