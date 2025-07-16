FROM rocker/shiny-verse:latest
LABEL org.opencontainers.image.source=https://github.com/hoenlab/data-portal


# Install system libraries needed by R packages or Python
RUN apt-get update && apt-get install -y \
    libpq-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget \
    vim


RUN R -e 'install.packages(c("shinyjs", "shinyalert", "shinydashboard", "shinyWidgets", "visNetwork", "DT", "sqldf", "rsconnect", "shinyFiles", "rhandsontable", "sendmailR", "reactable", "auth0", "datamods","openxlsx","aws.s3"))'


# Install your R packages
RUN R -e "install.packages(c('reticulate', 'jsonlite', 'shinyAce'))"

# Install Miniconda system-wide
ENV PATH="/opt/miniconda/bin:${PATH}"
RUN curl -L -o /tmp/miniconda.sh "https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh" && \
    bash /tmp/miniconda.sh -b -p /opt/miniconda && \
    rm /tmp/miniconda.sh

# Create the Conda environment and install Python packages
RUN conda create -n project_py_env python=3.9 -y && \
    conda run -n project_py_env pip install --no-cache-dir psycopg2-binary python-dotenv

RUN R -e 'install.packages(c("jose"))'
RUN R -e "install.packages(c('httr2', 'jose', 'openssl', 'jsonlite', 'urltools', 'httr'))"
RUN R -e "install.packages(c('strex', 'purrr'))"

# Set the working directory
WORKDIR /srv/shiny-server/

# Copy your entire project into the container
COPY . .

# Set correct file permissions for the shiny user
RUN chown -R shiny:shiny /srv/shiny-server

# Tell Docker to expose the Shiny port
EXPOSE 3838

# --- CRITICAL FIX ---
# Tell R to run the app from the 'madi-shiny-front' subdirectory
CMD ["R", "-e", "shiny::runApp('madi-shiny-front', host='0.0.0.0', port=3838)"]
