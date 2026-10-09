# Commenting on PRs/MRs from the file buffer — design notes

## Problems with the current code-review fork

1. **Token lookup** — ghub looks for `<user>^code-review` on `machine gitlab.com`
   (`ghub--token`). The forge token (`<user>^forge`, often on `gitlab.com/api/v4`)
   is not reused, so users must configure a second `.authinfo` entry.
2. **403 / Cloudflare challenge** — `code-review-gitlab--normalize-request`
   dropped `:host`, relying on ghub's default GitLab host being the API root.
   ghub `20260902` changed `ghub-default-host-alist` to `(gitlab . "gitlab.com")`,
   so requests hit the web UI (`https://gitlab.com/projects/...`) instead of the
   API. Fixed by passing `:host "gitlab.com/api/v4"` explicitly.
3. **Fragility** — the package depends on ghub/forge internals (default hosts,
   token naming, request args); each update of forge/magit/ghub can break it.
4. **UX** — reviewing happens in a dedicated diff buffer; you cannot stay in the
   file you are reading and just comment.

## Existing packages

- [emacs-pr-review](https://github.com/blahgeek/emacs-pr-review) — GitHub and
  GitLab (GitLab marked "experimental"); comments from its own review buffer;
  no forge integration mentioned.
- [code-review](https://github.com/wandersoncferreira/code-review) — upstream of
  this fork; diff-buffer based.
- [github-review](https://github.com/charignon/github-review) — GitHub only;
  diff-buffer based.

None lets you comment from the normal file buffer.

## Proposed design: a small forge extension

- **Comment where you are**: a command that comments on the current line/region
  of a file buffer, plus the same command in magit diff buffers.
- **See existing threads in place**: show review threads on the file at their
  lines (overlays or margin marker), reply/resolve from there.
- **Draft review then submit**:
  - GitHub: pending review via GraphQL (`addPullRequestReviewThread`, then submit).
  - GitLab: `draft_notes` API, then `bulk_publish`.
- **Built on forge**:
  - resolve the repository and the PR of the current branch through forge;
  - send requests with ghub, reusing the forge token (fixes problem 1);
  - dispatch per forge with `cl-defmethod` on forge's GitHub/GitLab repository
    classes, like forge does (Forgejo can come later);
  - add an entry to forge's transient dispatch.
- **Line validation**: both APIs only accept comments on lines in the PR diff.
  Compute the diff locally (base...head via magit), mark commentable lines in
  the fringe, fail early otherwise. Warn when `HEAD` is not the PR head commit,
  since line numbers would not match.
- **Out of scope**: permalinks and blame (`git-link`, `blamer` do that well).

## MELPA requirements

- Unique prefix (not `forge-`, not `my/`).
- Clean `package-lint`, `checkdoc`, byte-compile without warnings.
- GPL-compatible license.
- `Package-Requires` on emacs, magit, forge.
- ERT tests for the line → diff mapping, using fixture diffs.

## Main risk

forge internals move often; this is what keeps breaking code-review. Depend on
as few forge functions as possible — identifying which ones are stable is the
first task.

## MVP

Comment on a line/region from a file buffer (`tether-nitpick`), GitHub +
GitLab, posted directly (no drafts), **plus the navigation transient** (jump
matrix below). Then: inline threads, then draft reviews.

## Name

**`tether`** (prefix `tether-`): ties the code you are reading to its MR —
comments, diff and topic stay one keystroke away. Navigation is the core of the
package, so the name is about that, not only commenting.

- Commenting command: `tether-nitpick`.
- No recipe in `melpa/melpa/recipes` (checked 2026-10-06). Still to check:
  GNU ELPA and NonGNU ELPA.
- Alternatives (also free on MELPA): `waypoint`, `junction`, `wayfinder`.

## Navigation

### Review context

One object per repo, resolved once and cached: forge repo, pullreq, base/head
SHAs, diff line map, fetched threads. Every view reaches it the same way, so
every jump is symmetric.

- File buffer: `forge-get-pullreq :branch` (reads `branch.<name>.pullRequest`,
  set by `forge-checkout-pullreq`).
- forge/magit buffers: `forge-current-pullreq`.
- Do **not** use `forge--pullreq-range` / `forge--pullreq-ref` (internal).
  Build `base...head` from public slots (`base-ref`, `head-ref`, `base-rev`)
  and `magit-rev-verify`.

### `tether-mode`

- Global minor mode, active in file buffers of a repo whose current branch has
  a PR.
- Mode-line lighter `!123 ·4` (PR number, open threads).
- Prefix key opens the `tether` transient; the same transient is appended to
  `forge-dispatch` and `magit-dispatch`.

### Jump matrix

Every view reaches every other one, keeping file + line where it makes sense.

| From \ To      | File (at line)                   | PR diff (at hunk)        | Forge topic           | Thread list | Browser (at line)      | magit-status   |
|----------------|----------------------------------|--------------------------|-----------------------|-------------|------------------------|----------------|
| File buffer    | —                                | own: line map → hunk     | `forge-visit-pullreq` | own         | own URL builder        | `magit-status` |
| PR diff        | `magit-diff-visit-worktree-file` | —                        | `forge-visit-pullreq` | own         | own                    | `magit-status` |
| Forge topic    | via thread list                  | `magit-diff-range`       | —                     | own         | `forge-browse-pullreq` | `magit-status` |
| Thread list    | `RET`                            | `d`                      | `t`                   | —           | `b`                    | `s`            |

- **Thread list** (magit-section, grouped by file): the review hub. Forge's
  topic buffer does not show line-level review threads.
- **In the file**: `n`/`p` to next/previous thread or commentable hunk;
  threads as overlays or margin markers; `RET` shows the thread, reply/resolve.
- **Browser URL at line**:
  - GitLab: `…/merge_requests/N/diffs#<sha1(path)>_<old>_<new>`
  - GitHub: `…/pull/N/files#diff-<sha256(path)>R<line>`
- **File → diff at hunk**: reuse the line map; `magit-diff-range base...head --
  file`, then walk `hunk` sections to the one containing the line. Not
  `magit-diff-visit--position` (internal).

## Coverage vs code-review

### Needed, reuse the idea

| code-review feature                                  | Where                                       | In tether                                                    |
|------------------------------------------------------|---------------------------------------------|--------------------------------------------------------------|
| Comment editor (markdown, `C-c C-c`/`C-c C-k`, `C-c @`) | `code-review-comment.el`                    | buffer opened by `tether-nitpick`, no emojify                |
| Code suggestions                                     | `code-review-comment.el`                    | region prefilled into a ```` ```suggestion ```` block        |
| Outdated comments with original hunk                 | `code-review-section.el`                    | thread list shows them with the stored hunk, not in the file |
| Approve / request changes / LGTM                     | `code-review-actions.el` (`code-review-submit-*`) | verdict when submitting a draft review; GitLab approve is a separate endpoint |
| Review a single commit                               | `code-review-commit-diff`                   | line map on `sha^..sha`, comments target that commit         |
| Start from URL                                       | `code-review-start`                         | `forge-visit-topic-from-url`, then resolve the context       |

### Left to forge

Labels, assignees, milestone, title, description (`forge-topic-menu`), merge
(`forge-merge`), open the PR (`forge-visit-pullreq`). The tether transient only
links to them.

### Dropped

Bitbucket, reactions/emojify, promote comment to issue, binary files, local
save/resume of reviews (server-side drafts replace it), dependencies on
`deferred`, `a`, `uuidgen`, `closql` DB.

## Estimate

One developer who knows Emacs Lisp and forge, in days.

| #  | Item                                                                  | Days    | Reuse                                         |
|----|-----------------------------------------------------------------------|---------|-----------------------------------------------|
| 1  | Skeleton, `package-lint`, `checkdoc`, CI, Makefile                    | 0.5–1   | `Makefile`, `Cask`                            |
| 2  | Review context: PR from branch, SHAs, HEAD ≠ PR head warning, cache   | 1–1.5   | forge public API                              |
| 3  | Diff line map + ERT fixture tests                                     | 1–2     | `code-review-parse-hunk.el`                   |
| 4  | Post line/region comment (GitLab `position`/diff_refs, GitHub)        | 2–3     | `code-review-gitlab.el`, `-github.el`, e0ec8a1 |
| 5  | Navigation: `tether-mode`, transient, jump matrix, URL builders       | 2–3     | magit/forge commands                          |
| 6  | Comment editor + suggestions                                          | 1–1.5   | `code-review-comment.el`                      |
| —  | **MVP (1–6)**                                                         | **8–12** |                                              |
| 7  | Fetch threads, overlays/margin, thread list, `n`/`p`                  | 3–5     |                                               |
| 8  | Outdated threads in the list                                          | 1–2     | `code-review-section.el`                      |
| 9  | Reply / resolve                                                       | 1–2     |                                               |
| 10 | Draft reviews + verdict (approve / request changes)                   | 2.5–4   |                                               |
| 11 | Single-commit review                                                  | 1       |                                               |
| 12 | Docs, MELPA recipe, review round-trips                                | 1–2     |                                               |
| —  | **Total**                                                             | **18–28** |                                             |

Risks that push it up:

- forge API drift — keep the list of forge symbols used (all public) in this
  doc.
- GitLab `position` edge cases: renamed files, old-side-only lines.
- Line drift when the worktree differs from the PR head.
