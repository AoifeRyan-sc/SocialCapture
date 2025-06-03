# Base image
FROM --platform=linux/amd64 rocker/shiny

# Install system libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libglpk-dev \
    libssl-dev && \
    apt-get clean

## app folder
COPY /app ./app
COPY deploy.R deploy.R
COPY renv.lock /renv.lock 

# Install renv
RUN Rscript -e 'install.packages("renv")' &&\
    Rscript -e 'renv::restore(lockfile = "/renv.lock")' &&\

# Use the script as entrypoint
CMD Rscript deploy.R
