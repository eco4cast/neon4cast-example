# devcontainer-focused Rocker
FROM ghcr.io/rocker-org/devcontainer/geospatial:4.2

RUN R -e 'if(file.exists("DESCRIPTION")) remotes::install_deps()'
RUN R -e 'if(file.exists("install.R")) source("install.R")'
