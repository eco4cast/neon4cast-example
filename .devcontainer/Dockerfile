# devcontainer-focused Rocker
FROM ghcr.io/rocker-org/devcontainer/geospatial:4.2

# must be in .devcontainer dir
# RUN R -e 'if(file.exists("install.R")) source("install.R")'

RUN R -e "remotes::install_github('eco4cast/neon4cast')"
RUN R -e "install.packages('rMR')"

