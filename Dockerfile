FROM rocker/r-ver:4.4.1

# System libraries needed to compile the R packages app.R depends on,
# plus pandoc (rmarkdown's document converter, not bundled with r-ver).
RUN apt-get update && apt-get install -y --no-install-recommends \
    pandoc \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    libuv1-dev \
    xz-utils \
    perl \
    wget \
    nginx \
    apache2-utils \
    gettext-base \
    && rm -rf /var/lib/apt/lists/* \
    && rm -f /etc/nginx/sites-enabled/default

# Pre-install R packages so the container doesn't compile them on every boot.
RUN R -e "install.packages(c('shiny','rmarkdown','shinyAce','shinyjs','zip','fs','yaml','tinytex'), repos='https://cloud.r-project.org')"

# TinyTeX provides the LaTeX engine pandoc needs for the Beamer and Knitr PDF
# output formats. It ships without the `beamer` package (and its deps), which
# tlmgr fetches on demand -- but tlmgr's downloader is silently broken without
# `wget` on Debian (it doesn't error, it just never gets the file), and tlmgr
# itself needs the full `perl` package above (perl-base lacks File::Find).
# tlmgr_install() also doesn't raise an R error on failure, so a single call
# here would report Docker build success even if the install silently failed
# -- hence the retry loop with an explicit kpsewhich verification at the end
# that actually fails the build if the packages never landed (CTAN's mirror
# redirector occasionally lands on a stale mirror; a retry picks a new one).
RUN R -e "tinytex::install_tinytex()"
ENV PATH="/root/.TinyTeX/bin/x86_64-linux:${PATH}"
RUN for i in 1 2 3 4 5; do \
      R -e "tinytex::tlmgr_install(c('beamer','translator','fp','pgf'))"; \
      kpsewhich beamer.cls >/dev/null 2>&1 && kpsewhich translator.sty >/dev/null 2>&1 && break; \
      echo "beamer install attempt $i failed, retrying..."; sleep 5; \
    done; \
    kpsewhich beamer.cls && kpsewhich translator.sty

WORKDIR /app
COPY app.R .

# nginx sits in front and is the only process bound to Railway's public
# $PORT; it enforces HTTP Basic Auth (credentials from env vars, hashed into
# /etc/nginx/.htpasswd at container start, never baked into the image) and
# reverse-proxies to Shiny, which listens on localhost only. WebSocket
# upgrade headers are required here since Shiny's reactivity relies on a
# persistent connection, not plain request/response.
RUN mkdir -p /etc/nginx/templates
COPY nginx.conf.template /etc/nginx/templates/default.conf.template
COPY start.sh /start.sh
RUN chmod +x /start.sh

# Railway (and most PaaS hosts) assign the listen port via $PORT at runtime;
# 3838 is just the local-dev fallback. BASIC_AUTH_USER/BASIC_AUTH_PASS must
# be set at runtime -- start.sh fails fast if they're missing.
ENV PORT=3838
EXPOSE 3838

CMD ["/start.sh"]
