# tether

Review GitHub pull requests and GitLab merge requests from the file you are
reading in Emacs: comment on the current line or region, and jump between
the file, the PR diff, the forge topic, the review threads and the browser
while keeping file and line.

Built on [Forge](https://github.com/magit/forge): the repository, the pull
request and the API token all come from Forge, so there is nothing extra to
configure.

**Status:** early development. Nothing user-facing works yet; see
[SPEC.md](SPEC.md) for the design and the roadmap.

## Requirements

- Emacs 29.1 or later
- Forge set up for the repository (`forge-add-repository`), with its token
  in your auth-source

## Tested versions

CI runs against two dependency sets (SPEC.md §2.2):

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
