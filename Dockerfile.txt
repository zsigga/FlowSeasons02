FROM rocker/shiny:latest

# Used for healthchecks
RUN apt-get update && apt-get install -y curl

RUN rm -r /srv/shiny-server/*
COPY . /srv/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server

RUN R -e "install.packages('shiny', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('ggvis', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('dplyr', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('dbplyr', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('RSQLite', repos='https://cloud.r-project.org')"

USER shiny

EXPOSE 3838
