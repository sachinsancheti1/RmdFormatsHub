# Deployment (Docker + Railway)

This app is deployed as a Docker container on [Railway](https://railway.com), gated with HTTP Basic Auth. This doc covers how the setup works, how to operate it, and the non-obvious issues hit while building it — worth reading before touching the `Dockerfile`.

## How it's wired together

- `Dockerfile` builds on `rocker/r-ver`, installs the R packages `app.R` needs, and bakes in TinyTeX plus the `beamer` LaTeX package (for the Beamer/Knitr PDF output formats).
- `nginx` sits in front as a reverse proxy: it's the only process bound to Railway's public `$PORT`, enforces HTTP Basic Auth, and forwards traffic (including WebSocket upgrades, which Shiny's reactivity depends on) to the Shiny process, which listens on `127.0.0.1:3838` only.
- `start.sh` is the container's entrypoint: it hashes `BASIC_AUTH_USER`/`BASIC_AUTH_PASS` into `/etc/nginx/.htpasswd`, renders `nginx.conf.template` with the runtime `$PORT`, starts Shiny in the background, then execs nginx in the foreground.
- `railway.json` tells Railway to build via the `Dockerfile` rather than auto-detecting a buildpack.

## Required environment variables

`BASIC_AUTH_USER` and `BASIC_AUTH_PASS` must be set as Railway service variables — `start.sh` fails fast on startup if either is missing. Set or rotate them with:

```
railway variables --set "BASIC_AUTH_USER=<user>" --set "BASIC_AUTH_PASS=<password>"
```

Changing a variable triggers Railway to redeploy the current image automatically with the new value.

## Deploying / redeploying

Railway is connected to this GitHub repo (`sachinsancheti1/RmdFormatsHub`, `main` branch) and auto-deploys on every push — no manual step needed for normal changes. Check the current state with:

```
railway status            # current deployment state and public URL
```

For a one-off deploy from local uncommitted changes (bypassing GitHub), or to reconnect the CLI to the project on a new machine:

```
railway login          # one-time
railway link            # one-time, if not already linked in this directory
railway up               # builds from the local working directory and deploys
```

## Debugging inside the live container

Railway supports SSHing directly into the running container, which is much faster than guessing from build logs alone:

```
railway ssh keys add -k ~/.ssh/id_ed25519.pub   # one-time, registers a local key with Railway
railway ssh "<command>"
```

## Gotchas hit while building this

None of these are obvious from reading the `Dockerfile` in isolation, so recording the reasoning here:

1. **The `fs` R package fails to compile without `libuv1-dev`** (`fatal error: uv.h: No such file or directory`). This cascades: `fs` → `sass` → `bslib` → `shiny`/`rmarkdown` → `shinyAce`/`shinyjs` all fail with it missing.
2. **TinyTeX's installer needs `xz-utils`** — without it, `tar` can't extract the `.tar.xz` archive it downloads, and the install aborts.
3. **`tlmgr` (TeX Live's package manager) needs the full `perl` package**, not Debian's minimal `perl-base` — otherwise it crashes immediately with `Can't locate File/Find.pm`.
4. **`tlmgr` also needs `wget` (or `curl`) present** for its downloads. Without one, its Perl-native downloader silently fails to fetch anything from CTAN — and critically, `tinytex::tlmgr_install()` does **not** raise an R error when this happens. A naive `RUN R -e "tinytex::tlmgr_install('beamer')"` reports Docker build success while silently shipping a broken Beamer setup. The `Dockerfile` here verifies with `kpsewhich beamer.cls` (and `translator.sty`) after the install and fails the build loudly if either is missing.
5. **CTAN's mirror redirector occasionally routes to a stale mirror** with a checksum mismatch on a specific package (hit this with `translator.sty`, a Beamer dependency). Retrying against a different mirror resolves it — the `Dockerfile` retries up to 5 times before giving up.

## Security note

The app runs `rmarkdown::render()` on any uploaded `.Rmd` file, which means arbitrary embedded R/knitr code executes server-side by design. HTTP Basic Auth is the current mitigation for the public URL. If this ever moves to multi-user access or gets its auth removed, treat the upload endpoint as equivalent to a remote code execution surface, not just a file converter.
