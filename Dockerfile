FROM rstudio/plumber

RUN R -e "install.packages('Distance')"
RUN R -e "install.packages('readr', dependencies = TRUE)"

COPY . .



EXPOSE 8000
CMD ["Rscript","plumber.R"]

