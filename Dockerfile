FROM rstudio/plumber
RUN R -e "install.packages('Distance')"
RUN R -e "install.packages('randomForest')"
RUN R -e "install.packages('RMark')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('readr', dependencies = TRUE)"

RUN mkdir /app

COPY ./mark /usr/local/bin/mark
RUN chmod +x /usr/local/bin/mark

COPY . /app
WORKDIR /app

EXPOSE 8000
CMD ["Rscript","plumber.R"]

