FROM docker.io/r-base:latest

RUN apt-get update && \
    apt-get install -y libpq-dev libssl-dev && \
    rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('ojodb', repos = c('https://openjusticeok.r-universe.dev', 'https://cloud.r-project.org'))"

CMD ["R"]

